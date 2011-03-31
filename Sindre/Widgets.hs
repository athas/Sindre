{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.X11
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  portable
--
-- Portable Sindre gadgets that can be used by any substrate.
--
-----------------------------------------------------------------------------

module Sindre.Widgets ( mkHorizontally
                      , mkVertically 
                      , sizeable )
    where
  
import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime
import Sindre.Util

import "monads-fd" Control.Monad.Reader
import "monads-fd" Control.Monad.State
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M
  
data Oriented = Oriented {
      divideSpace :: Rectangle -> Integer -> [Rectangle]
    , mergeSpace :: [Rectangle] -> Rectangle
    , children :: [WidgetRef]
  }

instance MonadSubstrate m => Object m Oriented where

instance MonadSubstrate m => Widget m Oriented where
    composeI r = do
      n     <- genericLength <$> gets children
      rects <- gets divideSpace <*> pure r <*> pure n
      chlds <- gets children
      gets mergeSpace <*> zipWithM compose chlds rects
    drawI r = do
      n     <- genericLength <$> gets children
      rects <- gets divideSpace <*> pure r <*> pure n
      chlds <- gets children
      concat <$> zipWithM draw chlds rects

mkHorizontally :: MonadSubstrate m => Constructor m
mkHorizontally = sizeable mkHorizontally'
    where mkHorizontally' w m cs
              | m == M.empty =
                  construct (Oriented splitVert merge (map snd cs), w)
          mkHorizontally' _ _ _ = error "horizontally: bad args"
          merge rects = Rectangle (0, 0)
                        (sum $ map rectWidth rects)
                        (foldl max 0 $ map rectHeight rects)

mkVertically :: MonadSubstrate m => Constructor m
mkVertically = sizeable mkVertically'
    where mkVertically' w m cs
              | m == M.empty =
                  construct (Oriented splitHoriz merge (map snd cs), w)
          mkVertically' _ _ _ = error "vertically: bad args"
          merge rects = Rectangle (0, 0)
                        (foldl max 0 $ map rectWidth rects)
                        (sum $ map rectHeight rects)

data SizeableWidget s =
    SizeableWidget {
      maxWidth  :: Maybe Integer
    , maxHeight :: Maybe Integer
    , walign    :: Align
    , halign    :: Align
    , state     :: s
    }

data Align = AlignNeg | AlignPos | AlignCenter

encap :: (MonadSindre im m,
          MonadState (SizeableWidget s) (m im),
          MonadReader WidgetRef (m im)) =>
         (WidgetRef -> s -> Sindre im (b, s)) -> m im b
encap m = do st <- gets state
             wr <- ask
             (v, s') <- sindre $ m wr st
             modify $ \w -> w { state = s' }
             return v

instance Widget m s => Object m (SizeableWidget s) where
  callMethodI m vs = encap $ runObjectM $ callMethodI m vs
  fieldSetI f v = encap $ runObjectM $ fieldSetI f v
  fieldGetI f = encap $ runObjectM $ fieldGetI f

instance Widget m s => Widget m (SizeableWidget s) where
  composeI rect = do rect' <- adjustRectangle <$> get <*> pure rect
                     encap $ runWidgetM $ composeI rect'
  recvSubEventI e = encap $ runWidgetM $ recvSubEventI e
  recvEventI e = encap $ runWidgetM $ recvEventI e
  drawI rect = do rect' <- adjustRectangle <$> get <*> pure rect
                  encap $ runWidgetM $ drawI rect'

adjustRectangle :: SizeableWidget s -> Rectangle -> Rectangle
adjustRectangle sw (Rectangle (cx, cy) w h) =
    Rectangle (cx', cy') (min w maxw) (min h maxh)
    where cx' = frob (walign sw) cx w maxw
          cy' = frob (halign sw) cy h maxh
          maxw = fromMaybe w $ maxWidth sw
          maxh = fromMaybe h $ maxHeight sw
          frob AlignCenter c d maxv = c + (d - maxv) `div` 2
          frob AlignNeg c _ _ = c
          frob AlignPos c d maxv = c + d - maxv

asInteger :: Value -> Integer
asInteger (IntegerV x) = x
asInteger _ = error "not an integer"

asXAlign :: Value -> Align
asXAlign (StringV "left")  = AlignNeg
asXAlign (StringV "right") = AlignPos
asXAlign (StringV "center") = AlignCenter
asXAlign _ = error "Not a known stickyness"

asYAlign :: Value -> Align
asYAlign (StringV "top")  = AlignNeg
asYAlign (StringV "bottom") = AlignPos
asYAlign (StringV "center") = AlignCenter
asYAlign _ = error "Not a known stickyness"

sizeable :: MonadSubstrate m => Constructor m -> Constructor m
sizeable con w m cs = do
    let (maxh, m')  = extract "maxheight" m
        (maxw, m'') = extract "maxwidth" m'
        (xstick, m''') = extract "halign" m''
        (ystick, m'''') = extract "valign" m'''
    (NewWidget s, w') <- con w m'''' cs
    construct ( SizeableWidget
                (asInteger <$> maxw)
                (asInteger <$> maxh) 
                (maybe AlignCenter asXAlign xstick)
                (maybe AlignCenter asYAlign ystick)
                s
              , w')
