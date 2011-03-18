{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Visp.Runtime
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  nonportable
--
-- Definitions for the Visp runtime environment.
--
-----------------------------------------------------------------------------

module Visp.Runtime ( MonadVisp(..)
                    , Object(..)
                    , Widget(..)
                    , VispState(..)
                    , SubWidget(..)
                    , WidgetState(..)
                    , WidgetArgs
                    , WidgetBox(..)
                    , VarEnvironment
                    , WidgetRef
                    , SpaceNeed
                    , lookupObj
                    , lookupVal
                    , lookupVar
                    , operate
                    )
    where

import Visp.Visp
import Visp.Util

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type WidgetArgs = M.Map Identifier Value

data VispState m = VispState {
      widgets   :: WidgetBox m
    , varEnv    :: VarEnvironment
  }

type SpaceNeed = Rectangle

class ( Monad m
      , MonadState (VispState m) m
      , Applicative m
      , Monad (InitM m)) => MonadVisp m where
  type SubCfg m :: *
  type SubEvent m :: *
  type InitM m :: * -> *
  type InitVal m :: *
  
  fullRedraw :: m ()
  getEvent :: m Event
  
  printVal :: String -> m ()
  
  quit :: m ()

type VarEnvironment = M.Map Identifier Value

lookupVar :: MonadVisp m => Identifier -> m (Maybe Value)
lookupVar k = M.lookup k <$> gets varEnv

lookupVal :: MonadVisp m => Identifier -> m Value
lookupVal k = fromMaybe e <$> lookupVar k
    where e = (error $ "Undefined variable " ++ k)

lookupObj :: MonadVisp m => Identifier -> m WidgetRef
lookupObj k = do
  bnd <- lookupVal k
  case bnd of
    Reference r -> return r
    _           -> error $ "Unknown object '"++k++"'"

operate :: MonadVisp m => WidgetRef -> (WidgetState m -> m (a, WidgetState m)) -> m a
operate r f = do
  (v, s') <- change' r =<< gets widgets
  modify $ \s -> s { widgets = replace' r (widgets s) s' }
  return v
    where replace' [] (WidgetBox cs _) s' = do
            WidgetBox cs s'
          replace' (r':rs) (WidgetBox cs s) s' = do
            case splitAt r' cs of
              (bef, w:aft) -> do
                WidgetBox (bef++(replace' rs w s'):aft) s
              _            -> error $ "Bad index " ++ show r
          change' [] (WidgetBox _ s) = do
            (v, s') <- f s
            return (v, s')
          change' (r':rs) (WidgetBox cs _) = do
            change' rs (cs !! r')

class MonadVisp m => Object m s where
    callMethod :: s -> Identifier -> [Value] -> m (Value, s)
    callMethod _ m _ = fail $ "Unknown method '" ++ m ++ "'"
    fieldSet   :: s -> Identifier -> Value -> m s
    fieldSet _ f _ = fail $ "Unknown field '" ++ f ++ "'"
    fieldGet   :: s -> Identifier -> m Value
    fieldGet _ f = fail $ "Unknown field '" ++ f ++ "'"

class (Object m s, MonadVisp m) => Widget m s where
    compose      :: s -> Rectangle -> m SpaceNeed
    draw         :: s -> Rectangle -> m (SpaceNeed, s)
    recvSubEvent :: s -> SubEvent m -> m ([Event], s)
    recvSubEvent s _ = return ([], s)
    recvEvent    :: s -> Event -> m ([Event], s)
    recvEvent s _ = return ([], s)

refcall :: MonadVisp m => WidgetRef ->
           (WidgetState m -> m (a, WidgetState m)) -> m (a, WidgetRef)
refcall r f = do v <- operate r f
                 return (v, r)

instance MonadVisp m => Object m WidgetRef where
  callMethod r m vs = refcall r $ \s -> callMethod s m vs
  fieldSet r f v = snd <$> (refcall r $ \s -> ((,) ()) <$> fieldSet s f v)
  fieldGet r f = fst <$> (refcall r $ \s -> (flip (,) s) <$> fieldGet s f)

instance MonadVisp m => Widget m WidgetRef where
  compose r rect = fst <$> (refcall r $ \s -> ((flip (,) s) <$> compose s rect))
  draw r rect = refcall r $ \s -> draw s rect
  recvSubEvent r e = refcall r $ \s -> recvSubEvent s e
  recvEvent r e = refcall r $ \s -> recvEvent s e

data WidgetState m = forall s . (Widget m s, Object m s) =>
                     WidgetState s

inState :: Widget m s => m (a, s) -> m (a, WidgetState m)
inState f = do (v, s') <- f
               return $ (v, WidgetState s')

instance MonadVisp m => Object m (WidgetState m) where
  callMethod (WidgetState s) m vs =
    inState $ callMethod s m vs
  fieldSet (WidgetState s) f v =
    snd <$> (inState $ (,) v <$> fieldSet s f v)
  fieldGet (WidgetState s) = fieldGet s

instance MonadVisp m => Widget m (WidgetState m) where
  compose (WidgetState s) rect = compose s rect
  draw (WidgetState s) rect = inState $ draw s rect
  recvSubEvent (WidgetState s) e = inState $ recvSubEvent s e
  recvEvent (WidgetState s) e = inState $ recvEvent s e

data WidgetBox m = WidgetBox { widgetChildren :: [WidgetBox m]
                             , widgetCont     :: WidgetState m
                             }

allRefs :: WidgetBox m -> [WidgetRef]
allRefs (WidgetBox boxes _) =
  [] : concat (zipWith (\i -> map (i:)) [0..] (map allRefs boxes))

data SubWidget = SubWidget WidgetRef WidgetRef
                 deriving (Show)

subcall :: MonadVisp m => SubWidget ->
           (WidgetState m -> m (a, WidgetState m)) -> m (a, SubWidget)
subcall sw@(SubWidget t s) f = do v <- operate (t++s) f
                                  return (v, sw)

instance MonadVisp m => Object m SubWidget where
  callMethod sw m vs = subcall sw $ \s -> callMethod s m vs
  fieldSet sw f v = snd <$> (subcall sw $ \s -> ((,) ()) <$> fieldSet s f v)
  fieldGet sw f = fst <$> (subcall sw $ \s -> (flip (,) s) <$> fieldGet s f)

instance MonadVisp m => Widget m SubWidget where
  compose sw rect = fst <$> (subcall sw $ \s -> (flip (,) s) <$> compose s rect)
  draw sw r = subcall sw $ \s -> draw s r
  recvSubEvent sw e = subcall sw $ \s -> recvSubEvent s e
  recvEvent sw e = subcall sw $ \s -> recvEvent s e
