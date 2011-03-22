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
                    , DataSlot(..)
                    , VispState(..)
                    , WidgetState(..)
                    , WidgetArgs
                    , VarEnvironment
                    , VarBinding(..)
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
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type WidgetArgs = M.Map Identifier Value

data DataSlot m = WidgetSlot (WidgetState m)

data VispState m = VispState {
      varEnv    :: VarEnvironment
    , objects   :: Array WidgetRef (DataSlot m)
  }

type SpaceNeed = Rectangle
type SpaceUse = [Rectangle]

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

data VarBinding = VarBnd Value
                | ConstBnd Value

type VarEnvironment = M.Map Identifier VarBinding

lookupVar :: MonadVisp m => Identifier -> m (Maybe VarBinding)
lookupVar k = M.lookup k <$> gets varEnv

lookupVal :: MonadVisp m => Identifier -> m Value
lookupVal k = maybe e v <$> lookupVar k
    where e = (error $ "Undefined variable " ++ k)
          v (VarBnd v') = v'
          v (ConstBnd v') = v'

lookupObj :: MonadVisp m => Identifier -> m WidgetRef
lookupObj k = do
  bnd <- lookupVal k
  case bnd of
    Reference r -> return r
    _           -> error $ "Unknown object '"++k++"'"

operate :: MonadVisp m => WidgetRef -> (WidgetState m -> m (a, WidgetState m)) -> m a
operate r f = do
  (v, s') <- change' =<< (!r) <$> gets objects
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v
    where change' (WidgetSlot s) = do
            (v, s') <- f s
            return (v, WidgetSlot s')

class MonadVisp m => Object m s where
    callMethod :: s -> Identifier -> [Value] -> m (Value, s)
    callMethod _ m _ = fail $ "Unknown method '" ++ m ++ "'"
    fieldSet   :: s -> Identifier -> Value -> m s
    fieldSet _ f _ = fail $ "Unknown field '" ++ f ++ "'"
    fieldGet   :: s -> Identifier -> m Value
    fieldGet _ f = fail $ "Unknown field '" ++ f ++ "'"

class (Object m s, MonadVisp m) => Widget m s where
    compose      :: s -> Rectangle -> m SpaceNeed
    draw         :: s -> Rectangle -> m (SpaceUse, s)
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
