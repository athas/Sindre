{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Visp.X11
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  portable
--
-- Portable Visp gadgets that can be used by any substrate.
--
-----------------------------------------------------------------------------

module Visp.Widgets ( mkHorizontally
                    , mkVertically 
                    , sizeable )
    where
  
import Visp.Visp
import Visp.Compiler
import Visp.Runtime
import Visp.Util

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M
  
data Oriented = Oriented {
      divideSpace :: Rectangle -> Integer -> [Rectangle]
    , mergeSpace :: [Rectangle] -> Rectangle
    , children :: [SubWidget]
  }

instance MonadVisp m => Object m Oriented where

instance MonadVisp m => Widget m Oriented where
    compose o r = do
      rects <- zipWithM compose (children o) $ divideSpace o r n
      return $ mergeSpace o rects
        where n = genericLength (children o)
    draw o r = do
      zipWithM_ draw (children o) $ divideSpace o r n
      return (r, o)
        where n = genericLength (children o)

mkHorizontally :: MonadVisp m => Constructor m
mkHorizontally = sizeable mkHorizontally'
    where mkHorizontally' w m cs
              | m == M.empty = construct (Oriented splitVert merge cs, w)
          mkHorizontally' _ _ _ = error "horizontally: bad args"
          merge rects = Rectangle (0, 0)
                        (sum $ map rectWidth rects)
                        (foldl max 0 $ map rectHeight rects)

mkVertically :: MonadVisp m => Constructor m
mkVertically = sizeable mkVertically'
    where mkVertically' w m cs
              | m == M.empty = construct (Oriented splitHoriz merge cs, w)
          mkVertically' _ _ _ = error "vertically: bad args"
          merge rects = Rectangle (0, 0)
                        (foldl max 0 $ map rectWidth rects)
                        (sum $ map rectHeight rects)

data SizeableWidget s =
    SizeableWidget {
                 maxHeight :: Maybe Integer
               , maxWidth  :: Maybe Integer
               , state     :: s
               }

encap :: Widget m s => SizeableWidget s -> (s -> m (a, s)) ->
         m (a, SizeableWidget s)
encap sw f = do (v, s') <- f $ state sw
                return $ (v, sw { state = s' })

instance Widget m s => Object m (SizeableWidget s) where
  callMethod sw m vs = encap sw (\s -> callMethod s m vs)
  fieldSet r f v = snd <$> (encap r $ \s -> ((,) ()) <$> fieldSet s f v)
  fieldGet r f = fst <$> (encap r $ \s -> (flip (,) s) <$> fieldGet s f)

instance Widget m s => Widget m (SizeableWidget s) where
  compose r rect = fst <$> (encap r $ \s -> (flip (,) s) <$> compose s rect')
      where rect' = adjustRectangle r rect
  recvSubEvent r e = encap r $ \s -> recvSubEvent s e
  recvEvent r e = encap r $ \s -> recvEvent s e
  draw r rect = encap r $ \s -> draw s rect'
      where rect' = adjustRectangle r rect

adjustRectangle :: SizeableWidget s -> Rectangle -> Rectangle
adjustRectangle sw (Rectangle (cx, cy) w h) =
  Rectangle (cx', cy') (min w maxw) (min h maxh)
    where cx' | w <= maxw = cx
              | otherwise = cx + (w - maxw) `div` 2
          cy' | h <= maxh = cy
              | otherwise = cy + (h - maxh) `div` 2
          maxw = fromMaybe w $ maxWidth sw
          maxh = fromMaybe h $ maxHeight sw

asInteger :: Value -> Integer
asInteger (IntegerV x) = x
asInteger _ = error "not an integer"

sizeable :: MonadVisp m => Constructor m -> Constructor m
sizeable con w m cs = do
    let (maxh, m')  = extract "maxheight" m
        (maxw, m'') = extract "maxwidth" m'
    (s, w') <- con w m'' cs
    construct $ ( SizeableWidget
                  (asInteger <$> maxh) 
                  (asInteger <$> maxw)
                  s
                , w')
