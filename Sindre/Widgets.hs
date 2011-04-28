{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
                      , paramM
                      , paramAs
                      , param
                      , noParam
                      , badValue
                      , method )
    where
  
import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime
import Sindre.Util

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M

xAlign :: Value -> Maybe Align
xAlign (StringV "left")  = Just AlignNeg
xAlign (StringV "right") = Just AlignPos
xAlign (StringV "mid") = Just AlignCenter
xAlign _ = Nothing

yAlign :: Value -> Maybe Align
yAlign (StringV "top") = Just AlignNeg
yAlign (StringV "bot") = Just AlignPos
yAlign (StringV "mid") = Just AlignCenter
yAlign _ = Nothing
  
data Oriented = Oriented {
      mergeSpace :: [SpaceNeed] -> SpaceNeed
    , splitSpace :: Maybe Rectangle -> [SpaceNeed] -> [Maybe Rectangle]
    , children :: [WidgetRef]
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

mkHorizontally :: MonadBackend m => Constructor m
mkHorizontally = sizeable mkHorizontally'
    where mkHorizontally' w _ cs = constructing $ sindre $
            construct (Oriented merge
                       (splitter $ \r -> splitVert r . map fst) (map snd cs), w)
          merge rects = ( sumPrim $ map fst rects
                        , sumSec $ map snd rects )

mkVertically :: MonadBackend m => Constructor m
mkVertically = sizeable mkVertically'
    where mkVertically' w _ cs = constructing $ sindre $
            construct (Oriented merge
                       (splitter $ \r -> splitVert r . map snd) (map snd cs), w)
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
  recvBackEventI e = encap $ runObjectM $ recvBackEventI e
  recvEventI e = encap $ runObjectM $ recvEventI e

instance Widget m s => Widget m (SizeableWidget s) where
  composeI = do
    mw <- gets maxWidth
    mh <- gets maxHeight
    (wreq, hreq) <- encap $ runWidgetM composeI
    return (f wreq mw, f hreq mh)
      where f x Nothing = x
            f (Max x) (Just y) = Max $ min x y
            f _       (Just y) = Max y
  drawI rect = do walign' <- gets walign
                  rect' <- pure liftM <*> constrainRect <$> get <*> pure rect
                  encap $ runWidgetM $ drawI $ liftM2 (adjustRect walign') rect rect'

constrainRect :: SizeableWidget s -> Rectangle -> Rectangle
constrainRect sw rect@(Rectangle _ w h) =
  rect { rectWidth = min maxw w
       , rectHeight = min maxh h }
      where maxw = fromMaybe w $ maxWidth sw
            maxh = fromMaybe h $ maxHeight sw

sizeable :: MonadBackend m => Constructor m -> Constructor m
sizeable con w k cs = constructing $ do
  maxh <- Just <$> param "maxheight" <|> return Nothing
  maxw <- Just <$> param "maxwidth" <|> return Nothing
  xstick <- "halign" `paramAs` xAlign <|> return AlignCenter
  ystick <- "valign" `paramAs` yAlign <|> return AlignCenter
  (NewWidget s, w') <- subconstruct $ sindre . con w k cs
  sindre $ construct (SizeableWidget maxw maxh (xstick, ystick) s, w')

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
    Left (NoParam k) -> error $ "Missing argument '"++k++"'"
    Left (BadValue k) -> error $ "Bad value for argument '"++k++"'"
    Right _ | m' /= M.empty -> error "Surplus arguments"
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

class (MonadBackend m, Object m o) => Method o m a where
  method :: a -> [Value] -> ObjectM o m Value

instance (Mold a, Object m o, MonadBackend m) => Method o m (ObjectM o m a) where
  method x [] = unmold <$> x
  method _ _ = error "Too many arguments"

instance (Mold a, Method o m b, MonadBackend m) => Method o m (a -> b) where
  method f (x:xs) = case mold x of
                      Nothing -> error "Cannot mold argument"
                      Just x' -> f x' `method` xs
  method _ [] = error "Not enough arguments"
