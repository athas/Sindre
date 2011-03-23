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
-- Module      :  Sindre.Runtime
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  nonportable
--
-- Definitions for the Sindre runtime environment.
--
-----------------------------------------------------------------------------

module Sindre.Runtime ( MonadSindre(..)
                    , Object(..)
                    , Widget(..)
                    , DataSlot(..)
                    , SindreState(..)
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

import Sindre.Sindre
import Sindre.Util

import System.Exit

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type WidgetArgs = M.Map Identifier Value

data DataSlot m = WidgetSlot (WidgetState m)

data SindreState m = SindreState {
      varEnv    :: VarEnvironment
    , objects   :: Array WidgetRef (DataSlot m)
  }

type SpaceNeed = Rectangle
type SpaceUse = [Rectangle]

class ( Monad m
      , MonadState (SindreState m) m
      , Applicative m
      , Monad (InitM m)) => MonadSindre m where
  type SubCfg m :: *
  type SubEvent m :: *
  type InitM m :: * -> *
  type InitVal m :: *
  
  fullRedraw :: m ()
  getEvent :: m Event
  
  printVal :: String -> m ()
  
  quit :: ExitCode -> m ()

data VarBinding = VarBnd Value
                | ConstBnd Value

type VarEnvironment = M.Map Identifier VarBinding

lookupVar :: MonadSindre m => Identifier -> m (Maybe VarBinding)
lookupVar k = M.lookup k <$> gets varEnv

lookupVal :: MonadSindre m => Identifier -> m Value
lookupVal k = maybe e v <$> lookupVar k
    where e = (error $ "Undefined variable " ++ k)
          v (VarBnd v') = v'
          v (ConstBnd v') = v'

lookupObj :: MonadSindre m => Identifier -> m WidgetRef
lookupObj k = do
  bnd <- lookupVal k
  case bnd of
    Reference r -> return r
    _           -> error $ "Unknown object '"++k++"'"

operate :: MonadSindre m => WidgetRef -> (WidgetState m -> m (a, WidgetState m)) -> m a
operate r f = do
  (v, s') <- change' =<< (!r) <$> gets objects
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v
    where change' (WidgetSlot s) = do
            (v, s') <- f s
            return (v, WidgetSlot s')

class MonadSindre m => Object m s where
    callMethod :: s -> Identifier -> [Value] -> m (Value, s)
    callMethod _ m _ = fail $ "Unknown method '" ++ m ++ "'"
    fieldSet   :: s -> Identifier -> Value -> m s
    fieldSet _ f _ = fail $ "Unknown field '" ++ f ++ "'"
    fieldGet   :: s -> Identifier -> m Value
    fieldGet _ f = fail $ "Unknown field '" ++ f ++ "'"

class (Object m s, MonadSindre m) => Widget m s where
    compose      :: s -> Rectangle -> m SpaceNeed
    draw         :: s -> Rectangle -> m (SpaceUse, s)
    recvSubEvent :: s -> SubEvent m -> m ([Event], s)
    recvSubEvent s _ = return ([], s)
    recvEvent    :: s -> Event -> m ([Event], s)
    recvEvent s _ = return ([], s)

refcall :: MonadSindre m => WidgetRef ->
           (WidgetState m -> m (a, WidgetState m)) -> m (a, WidgetRef)
refcall r f = do v <- operate r f
                 return (v, r)

instance MonadSindre m => Object m WidgetRef where
  callMethod r m vs = refcall r $ \s -> callMethod s m vs
  fieldSet r f v = snd <$> (refcall r $ \s -> ((,) ()) <$> fieldSet s f v)
  fieldGet r f = fst <$> (refcall r $ \s -> (flip (,) s) <$> fieldGet s f)

instance MonadSindre m => Widget m WidgetRef where
  compose r rect = fst <$> (refcall r $ \s -> ((flip (,) s) <$> compose s rect))
  draw r rect = refcall r $ \s -> draw s rect
  recvSubEvent r e = refcall r $ \s -> recvSubEvent s e
  recvEvent r e = refcall r $ \s -> recvEvent s e

data WidgetState m = forall s . (Widget m s, Object m s) =>
                     WidgetState s

inState :: Widget m s => m (a, s) -> m (a, WidgetState m)
inState f = do (v, s') <- f
               return $ (v, WidgetState s')

instance MonadSindre m => Object m (WidgetState m) where
  callMethod (WidgetState s) m vs =
    inState $ callMethod s m vs
  fieldSet (WidgetState s) f v =
    snd <$> (inState $ (,) v <$> fieldSet s f v)
  fieldGet (WidgetState s) = fieldGet s

instance MonadSindre m => Widget m (WidgetState m) where
  compose (WidgetState s) rect = compose s rect
  draw (WidgetState s) rect = inState $ draw s rect
  recvSubEvent (WidgetState s) e = inState $ recvSubEvent s e
  recvEvent (WidgetState s) e = inState $ recvEvent s e
