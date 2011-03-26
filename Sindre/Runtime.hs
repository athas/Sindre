{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Sindre.Runtime ( Sindre(..)
                      , runSindre
                      , getEvent
                      , MonadSindre(..)
                      , EventSender
                      , EventSource(..)
                      , broadcast
                      , changed
                      , MonadSubstrate(..)
                      , subst
                      , Object(..)
                      , ObjectM
                      , runObjectM
                      , fieldSet
                      , fieldGet
                      , callMethod
                      , Widget(..)
                      , WidgetM
                      , runWidgetM
                      , draw
                      , compose
                      , DataSlot(..)
                      , SindreEnv(..)
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

import Debug.Trace

import Control.Applicative
import "monads-fd" Control.Monad.Reader
import "monads-fd" Control.Monad.State
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence((|>), ViewL(..))

type WidgetArgs = M.Map Identifier Value

data DataSlot m = WidgetSlot (WidgetState m)

data WidgetState m = forall s a . (Widget m s, Object m s) =>
                     WidgetState s

data EventSource = WidgetSrc WidgetRef
                 | SubstrSrc

data SindreEnv m = SindreEnv {
      varEnv    :: VarEnvironment
    , widgetRev :: M.Map WidgetRef Identifier
    , objects   :: Array WidgetRef (DataSlot m)
    , evtQueue  :: Q.Seq (EventSource, Event)
  }

type SpaceNeed = Rectangle
type SpaceUse = [Rectangle]

class (Monad m, Functor m, Applicative m) => MonadSubstrate m where
  type SubEvent m :: *
  type InitVal m :: *
  type InitM m :: *
  fullRedraw :: Sindre m ()
  getSubEvent :: Sindre m Event
  printVal :: String -> m ()
  quit :: ExitCode -> m ()

newtype Sindre m a = Sindre (StateT (SindreEnv m) m a)
  deriving (Functor, Monad, MonadState (SindreEnv m), Applicative)

subst :: MonadSubstrate m => m a -> Sindre m a
subst = lift

instance MonadTrans Sindre where
  lift = Sindre . lift

runSindre :: MonadSubstrate m => Sindre m a -> SindreEnv m -> m a
runSindre (Sindre m) s = evalStateT m s

class (MonadSubstrate im, Monad (m im)) => MonadSindre im m where
  sindre :: Sindre im a -> m im a

class MonadSindre im m => EventSender im m where
  source :: m im EventSource

instance MonadSubstrate im => MonadSindre im Sindre where
  sindre = id

newtype ObjectM o m a = ObjectM (ReaderT WidgetRef (StateT o (Sindre m)) a)
    deriving (Functor, Monad, Applicative, MonadState o, MonadReader WidgetRef)

instance MonadSubstrate im => MonadSindre im (ObjectM o) where
  sindre = ObjectM . lift . lift

instance MonadSubstrate im => EventSender im (ObjectM o) where
  source = WidgetSrc <$> ask

runObjectM :: Object m o => ObjectM o m a -> WidgetRef -> o -> Sindre m (a, o)
runObjectM (ObjectM m) wr o = runStateT (runReaderT m wr) o

class MonadSubstrate m => Object m s where
  callMethodI :: Identifier -> [Value] -> ObjectM s m Value
  callMethodI m _ = fail $ "Unknown method '" ++ m ++ "'"
  fieldSetI   :: Identifier -> Value -> ObjectM s m Value
  fieldSetI f _ = fail $ "Unknown field '" ++ f ++ "'"
  fieldGetI   :: Identifier -> ObjectM s m Value
  fieldGetI f = fail $ "Unknown field '" ++ f ++ "'"

newtype WidgetM w m a = WidgetM (ObjectM w m a)
    deriving (Functor, Monad, Applicative, MonadState w,
              MonadReader WidgetRef)

instance MonadSubstrate im => MonadSindre im (WidgetM o) where
  sindre = WidgetM . sindre

instance MonadSubstrate im => EventSender im (WidgetM o) where
  source = WidgetSrc <$> ask

runWidgetM :: Widget m w => WidgetM w m a -> WidgetRef -> w -> Sindre m (a, w)
runWidgetM (WidgetM m) wr w = runObjectM m wr w

class Object m s => Widget m s where
  composeI      :: Rectangle -> WidgetM s m SpaceNeed
  drawI         :: Rectangle -> WidgetM s m SpaceUse
  recvSubEventI :: SubEvent m -> WidgetM s m ()
  recvSubEventI _ = return ()
  recvEventI    :: Event -> WidgetM s m ()
  recvEventI _ = return ()

encapO :: MonadSubstrate im =>
          (forall s . Object im s => WidgetRef -> s -> Sindre im (a, s)) ->
          ObjectM (WidgetState im) im a
encapO m = do WidgetState s <- get
              wr <- ask
              (v, s') <- sindre $ m wr s
              put $ WidgetState s'
              return v

encapW :: MonadSubstrate im =>
          (forall s . Widget im s => WidgetRef -> s -> Sindre im (a, s)) ->
          WidgetM (WidgetState im) im a
encapW m = do WidgetState s <- get
              wr <- ask
              (v, s') <- sindre $ m wr s
              put $ WidgetState s'
              return v

instance MonadSubstrate m => Object m (WidgetState m) where
  callMethodI m vs = encapO $ runObjectM $ callMethodI m vs
  fieldSetI f v = encapO $ runObjectM $ fieldSetI f v
  fieldGetI f = encapO $ runObjectM $ fieldGetI f

instance MonadSubstrate m => Widget m (WidgetState m) where
  composeI rect = encapW $ runWidgetM $ composeI rect
  recvSubEventI e = encapW $ runWidgetM $ recvSubEventI e
  recvEventI e = encapW $ runWidgetM $ recvEventI e
  drawI rect = encapW $ runWidgetM $ drawI rect

data VarBinding = VarBnd Value
                | ConstBnd Value

type VarEnvironment = M.Map Identifier VarBinding

getEvent :: MonadSubstrate m => Sindre m (EventSource, Event)
getEvent = do queue <- gets evtQueue
              case Q.viewl queue of
                e :< queue' -> do modify $ \s -> s { evtQueue = queue' }
                                  return e
                EmptyL      -> (,) SubstrSrc <$> getSubEvent

broadcast :: EventSender im m => Event -> m im ()
broadcast e = do src <- source
                 sindre $ modify $ \s -> s { evtQueue = evtQueue s |> (src, e) }

changed :: EventSender im m => Identifier -> Value -> Value -> m im ()
changed f old new = broadcast $ NamedEvent "changed" [old, new]

type SindreM a = MonadSubstrate m => Sindre m a

lookupVar :: Identifier -> SindreM (Maybe VarBinding)
lookupVar k = M.lookup k <$> gets varEnv

lookupVal :: Identifier -> SindreM Value
lookupVal k = maybe e v <$> lookupVar k
    where e = (error $ "Undefined variable " ++ k)
          v (VarBnd v') = v'
          v (ConstBnd v') = v'

lookupObj :: Identifier -> SindreM WidgetRef
lookupObj k = do
  bnd <- lookupVal k
  case bnd of
    Reference r -> return r
    _           -> error $ "Unknown object '"++k++"'"

operate :: MonadSubstrate m => WidgetRef -> (WidgetState m -> Sindre m (a, WidgetState m)) -> Sindre m a
operate r f = do
  objs <- gets objects
  (v, s') <- change' (objs!r)
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v
    where change' (WidgetSlot s) = do
            (v, s') <- f s
            return (v, WidgetSlot s')

actionO :: MonadSubstrate m => WidgetRef ->
           (forall o . Object m o => ObjectM o m a) -> Sindre m a
actionO r f = operate r $ \(WidgetState s) -> do
                (v, s') <- runObjectM f r s
                return (v, WidgetState s')

callMethod :: MonadSindre im m =>
              WidgetRef -> Identifier -> [Value] -> m im Value
callMethod r m vs = sindre $ actionO r (callMethodI m vs)
fieldSet :: MonadSindre im m =>
            WidgetRef -> Identifier -> Value -> m im Value
fieldSet r f v = do sindre $ actionO r $ do
                      old <- fieldGetI f
                      new <- fieldSetI f v
                      changed f old new
                      return new
                                            
fieldGet :: MonadSindre im m =>
            WidgetRef -> Identifier -> m im Value
fieldGet r f = sindre $ actionO r (fieldGetI f)

actionW :: MonadSubstrate m => WidgetRef ->
           (forall o . Widget m o => WidgetM o m a) -> Sindre m a
actionW r f = operate r $ \(WidgetState s) -> do
                     (v, s') <- runWidgetM f r s
                     return (v, WidgetState s')

compose :: MonadSindre im m =>
           WidgetRef -> Rectangle -> m im SpaceNeed
compose r rect = sindre $ actionW r (composeI rect)
draw :: MonadSindre im m =>
        WidgetRef -> Rectangle -> m im SpaceUse
draw r rect = sindre $ actionW r (drawI rect)
recvSubEvent :: MonadSindre im m =>
                WidgetRef -> SubEvent im -> m im ()
recvSubEvent r ev = sindre $ actionW r (recvSubEventI ev)
recvEvent :: MonadSindre im m =>
             WidgetRef -> Event -> m im ()
recvEvent r ev = sindre $ actionW r (recvEventI ev)
