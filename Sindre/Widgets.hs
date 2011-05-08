{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.X11
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  portable
--
-- Portable Sindre gadgets and helper functions that can be used by
-- any backend.
--
-----------------------------------------------------------------------------

module Sindre.Widgets ( mkHorizontally
                      , mkVertically
                      , changeFields
                      )
    where
  
import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime

import Control.Monad.Error
import Control.Monad.State
import Control.Applicative

data Oriented = Oriented {
      mergeSpace :: [SpaceNeed] -> SpaceNeed
    , splitSpace :: Maybe Rectangle -> [SpaceNeed] -> [Maybe Rectangle]
    , children   :: [WidgetRef]
  }

instance MonadBackend m => Object m Oriented where

instance MonadBackend m => Widget m Oriented where
    composeI = do
      chlds <- gets children
      gets mergeSpace <*> mapM compose chlds
    drawI r = do
      chlds <- gets children
      rects <- gets splitSpace <*> pure r <*> mapM compose chlds
      concat <$> zipWithM draw (reverse chlds) (reverse rects)

splitter :: (Rectangle -> [SpaceNeed] -> [Rectangle])
         -> Maybe Rectangle -> [SpaceNeed] -> [Maybe Rectangle]
splitter _ Nothing = map (const Nothing)
splitter f (Just r) = fmap Just . f r

layouting :: MonadBackend m => (forall a. ((a, a) -> a)) -> Constructor m
layouting f w _ cs = construct ( Oriented merge (splitter split) (map snd cs)
                               , w)
    where merge rects = ( f (sumPrim, sumSec) $ map fst rects
                        , f (sumSec, sumPrim) $ map snd rects )
          split r     = f (splitVert, splitHoriz) r . map f

mkHorizontally :: MonadBackend m => Constructor m
mkHorizontally = layouting fst

mkVertically :: MonadBackend m => Constructor m
mkVertically = layouting snd

changeFields :: MonadBackend im =>
               [Identifier] -> [a -> Value]
            -> (a -> ObjectM a im a) -> ObjectM a im ()
changeFields ks fs m = do
  s <- get
  s' <- m s
  put s' >> zipWithM_ (\k f -> changed k (f s) (f s')) ks fs
