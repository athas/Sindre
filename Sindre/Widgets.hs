{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Portable Sindre gadgets that can be used by any backend.
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

import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M


asXAlign :: Value -> Maybe Align
asXAlign (StringV "left")  = Just AlignNeg
asXAlign (StringV "right") = Just AlignPos
asXAlign (StringV "mid") = Just AlignCenter
asXAlign _ = Nothing

asYAlign :: Value -> Maybe Align
asYAlign (StringV "top")  = Just AlignNeg
asYAlign (StringV "bot") = Just AlignPos
asYAlign (StringV "mid") = Just AlignCenter
asYAlign _ = Nothing
  
data Oriented = Oriented {
      mergeSpace :: [SpaceNeed] -> SpaceNeed
    , splitSpace :: Rectangle -> [SpaceNeed] -> [Rectangle]
    , children :: [WidgetRef]
  }

instance MonadBackend m => Object m Oriented where

instance MonadBackend m => Widget m Oriented where
    composeI r = do
      chlds <- gets children
      gets mergeSpace <*> mapM (flip compose r) chlds
    drawI r = do
      chlds <- gets children
      rects <- gets splitSpace <*> pure r <*> mapM (flip compose r) chlds
      concat <$> zipWithM draw chlds rects

mkHorizontally :: MonadBackend m => Constructor m
mkHorizontally = sizeable mkHorizontally'
    where mkHorizontally' w m cs
              | m == M.empty =
                  construct (Oriented merge
                             (\r -> splitVert r . map fst) (map snd cs), w)
          mkHorizontally' _ _ _ = error "horizontally: bad args"
          merge rects = ( sumPrim $ map fst rects
                        , sumSec $ map snd rects )

mkVertically :: MonadBackend m => Constructor m
mkVertically = sizeable mkVertically'
    where mkVertically' w m cs
              | m == M.empty =
                  construct (Oriented merge
                             (\r -> splitHoriz r . map snd) (map snd cs), w)
          mkVertically' _ _ _ = error "vertically: bad args"
          merge rects = ( sumSec $ map fst rects
                        , sumPrim $ map snd rects)

data SizeableWidget s =
    SizeableWidget {
      maxWidth  :: Maybe Integer
    , maxHeight :: Maybe Integer
    , walign    :: (Align, Align)
    , instate   :: s
    }

encap :: (MonadSindre im m,
          MonadState (SizeableWidget s) (m im),
          MonadReader WidgetRef (m im)) =>
         (WidgetRef -> s -> Sindre im (b, s)) -> m im b
encap m = do st <- gets instate
             wr <- ask
             (v, s') <- sindre $ m wr st
             modify $ \w -> w { instate = s' }
             return v

instance Widget m s => Object m (SizeableWidget s) where
  callMethodI m vs = encap $ runObjectM $ callMethodI m vs
  fieldSetI f v = encap $ runObjectM $ fieldSetI f v
  fieldGetI f = encap $ runObjectM $ fieldGetI f

instance Widget m s => Widget m (SizeableWidget s) where
  composeI rect = do rect' <- constrainRect <$> get <*> pure rect
                     encap $ runWidgetM $ composeI rect'
  recvSubEventI e = encap $ runWidgetM $ recvSubEventI e
  recvEventI e = encap $ runWidgetM $ recvEventI e
  drawI rect = do walign' <- gets walign
                  rect' <- constrainRect <$> get <*> pure rect
                  encap $ runWidgetM $ drawI $ adjustRect walign' rect rect'

constrainRect :: SizeableWidget s -> Rectangle -> Rectangle
constrainRect sw rect@(Rectangle _ w h) =
  rect { rectWidth = min maxw w
       , rectHeight = min maxh h }
      where maxw = fromMaybe w $ maxWidth sw
            maxh = fromMaybe h $ maxHeight sw

sizeable :: MonadBackend m => Constructor m -> Constructor m
sizeable con w m cs = do
  let (maxh, m')  = extract "maxheight" m
      (maxw, m'') = extract "maxwidth" m'
      (xstick, m''') = extract "halign" m''
      (ystick, m'''') = extract "valign" m'''
  (NewWidget s, w') <- con w m'''' cs
  construct ( SizeableWidget
              (asInteger <$> maxw)
              (asInteger <$> maxh) 
              ( fromMaybe AlignCenter $ asXAlign =<< xstick
              , fromMaybe AlignCenter $ asYAlign =<< ystick)
              s
            , w')
    where asInteger = fromMaybe (error "Must be an integer") . mold
