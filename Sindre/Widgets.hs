{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
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
                      , constructing 
                      , paramAs
                      , param 
                      , method )
    where
  
import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime

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
    where mkHorizontally' w cs = constructing $ sindre $
                                 construct (Oriented merge
                                            (\r -> splitVert r . map fst) 
                                            (map snd cs), w)
          merge rects = ( sumPrim $ map fst rects
                        , sumSec $ map snd rects )

mkVertically :: MonadBackend m => Constructor m
mkVertically = sizeable mkVertically'
    where mkVertically' w cs = constructing $ sindre $
                               construct (Oriented merge
                                          (\r -> splitHoriz r . map snd)
                                          (map snd cs), w)
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
  composeI rect = do
    mw <- gets maxWidth
    mh <- gets maxHeight
    (wreq, hreq) <- encap $ runWidgetM $ composeI rect
    return (f wreq mw, f hreq mh)
      where f x Nothing = x
            f (Max x) (Just y) = Max $ min x y
            f _       (Just y) = Max y
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
sizeable con w cs = constructing $ do
  maxh <- Just <$> param "maxheight" <|> return Nothing
  maxw <- Just <$> param "maxwidth" <|> return Nothing
  xstick <- "halign" `paramAs` xAlign <|> return AlignCenter
  ystick <- "valign" `paramAs` yAlign <|> return AlignCenter
  (NewWidget s, w') <- subconstruct $ sindre . con w cs
  sindre $ construct (SizeableWidget maxw maxh (xstick, ystick) s, w')

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
  empty = throwError $ NoParam "<none>"
  x <|> y = x `catchError` f
      where f (NoParam _) = y
            f e           = throwError e

instance MonadBackend im => MonadSindre im ConstructorM where
  sindre = ConstructorM . lift . lift

paramAs :: MonadBackend m =>
           Identifier -> (Value -> Maybe a) -> ConstructorM m a
paramAs k f = do m <- get
                 case M.lookup k m of
                   Nothing -> throwError $ NoParam k
                   Just (f -> Just v) -> do put (k `M.delete` m)
                                            return v
                   Just _ -> throwError $ BadValue k

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
