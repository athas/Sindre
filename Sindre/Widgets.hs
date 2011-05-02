{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
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
                      , sizeable
                      , ConstructorM
                      , constructing
                      , Param(..)
                      , Align(..)
                      , paramM
                      , paramAs
                      , param
                      , noParam
                      , badValue
                      )
    where
  
import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime
import Sindre.Util

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M

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
layouting f w _ cs = constructing $
  construct (Oriented merge (splitter split) (map snd cs), w)
    where merge rects = ( f (sumPrim, sumSec) $ map fst rects
                        , f (sumSec, sumPrim) $ map snd rects )
          split r     = f (splitVert, splitHoriz) r . map f

mkHorizontally :: MonadBackend m => Constructor m
mkHorizontally = sizeable $ layouting fst

mkVertically :: MonadBackend m => Constructor m
mkVertically = sizeable $ layouting snd

data SizeableWidget s =
    SizeableWidget {
      minDims :: (Maybe Integer, Maybe Integer)
    , maxDims :: (Maybe Integer, Maybe Integer)
    , instate :: s
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
  recvBackEventI e = encap $ runObjectM $ recvBackEventI e
  recvEventI e = encap $ runObjectM $ recvEventI e

instance Widget m s => Widget m (SizeableWidget s) where
  composeI = do
    (maxw, maxh) <- gets maxDims
    (minw, minh) <- gets minDims
    (wreq, hreq) <- encap $ runWidgetM composeI
    return (f wreq minw maxw, f hreq minh maxh)
      where f x Nothing Nothing = x
            f _ (Just y) _ = Min y
            f _ _ (Just y) = Max y
  drawI = encap . runWidgetM . drawI

sizeable :: MonadBackend m => Constructor m -> Constructor m
sizeable con w k cs = constructing $ do
  minw <- Just <$> param "minwidth"  <|> return Nothing
  minh <- Just <$> param "minheight" <|> return Nothing
  maxw <- Just <$> param "maxwidth"  <|> return Nothing
  maxh <- Just <$> param "maxheight" <|> return Nothing
  (NewWidget s, w') <- subconstruct $ sindre . con w k cs
  construct (SizeableWidget (minw, minh) (maxw, maxh) s, w')

class MonadBackend m => Param m a where
  moldM :: Value -> m (Maybe a)

data ParamError = NoParam Identifier | BadValue Identifier
                  deriving (Show)

instance Error ParamError where
  strMsg = BadValue

newtype ConstructorM m a = ConstructorM (ErrorT ParamError
                                         (StateT (M.Map Identifier Value) 
                                          (Sindre m))
                                         a)
    deriving ( MonadState (M.Map Identifier Value)
             , MonadError ParamError
             , Monad, Functor, Applicative)

noParam :: String -> ConstructorM m a
noParam = throwError . NoParam

badValue :: String -> ConstructorM m a
badValue = throwError . BadValue

constructing :: MonadBackend m => ConstructorM m (Construction m)
             -> M.Map Identifier Value -> Sindre m (Construction m)
constructing (ConstructorM c) m = do
  (v, m') <- runStateT (runErrorT c) m
  case v of
    Left (NoParam k) -> fail $ "Missing argument '"++k++"'"
    Left (BadValue k) -> fail $ "Bad value for argument '"++k++"'"
    Right _ | m' /= M.empty -> fail "Surplus arguments"
    Right v' -> return v'

subconstruct :: MonadBackend m =>
                (M.Map Identifier Value -> Sindre m (Construction m))
             -> ConstructorM m (Construction m)
subconstruct con = (sindre . con) =<< get <* put M.empty

instance MonadBackend m => Alternative (ConstructorM m) where
  empty = noParam "<none>"
  x <|> y = x `catchError` f
      where f (NoParam k) = y `catchError` g k
            f e           = throwError e
            g k1 (NoParam  _)  = noParam  k1
            g _  (BadValue k2) = badValue k2

instance MonadBackend im => MonadSindre im ConstructorM where
  sindre = ConstructorM . lift . lift

instance (MonadIO m, MonadBackend m) => MonadIO (ConstructorM m) where
  liftIO = back . io

paramAsM :: MonadBackend m => Identifier
         -> (Value -> m (Maybe a)) -> ConstructorM m a
paramAsM k mf = do m <- get
                   case M.lookup k m of
                     Nothing -> noParam k
                     Just v -> do put (k `M.delete` m)
                                  back (mf v) >>=
                                     maybe (badValue k) return

paramM :: (Param m a, MonadBackend m) => Identifier -> ConstructorM m a
paramM k = paramAsM k moldM

paramAs :: MonadBackend m =>
           Identifier -> (Value -> Maybe a) -> ConstructorM m a
paramAs k f = paramAsM k (return . f)

param :: (Mold a, MonadBackend m) => Identifier -> ConstructorM m a
param k = paramAs k mold
