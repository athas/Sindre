{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
                      , execSindre
                      , getEvent
                      , MonadSindre(..)
                      , EventSender
                      , EventSource(..)
                      , broadcast
                      , changed
                      , MonadSubstrate(..)
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
                      , WidgetArgs
                      , VarEnvironment
                      , VarBinding(..)
                      , WidgetRef
                      , SpaceNeed
                      , lookupObj
                      , lookupVal
                      , lookupVar
                      , lookupFunc
                      , revLookup
                      , ExecutionEnv(ExecutionEnv)
                      , newExecution
                      , returnHere
                      , doReturn
                      , nextHere
                      , doNext
                      )
    where

import Sindre.Sindre
import Sindre.Util

import System.Exit

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont
import Data.Array
import qualified Data.Map as M
import qualified Data.Sequence as Q
import Data.Sequence((|>), ViewL(..))

type WidgetArgs = M.Map Identifier Value

data DataSlot m = forall s . Widget m s => WidgetSlot s
                | forall s . Object m s => ObjectSlot s

data EventSource = ObjectSrc ObjectRef
                 | SubstrSrc
                   deriving (Show)

type Breaker m a = a -> Sindre m ()

data ExecutionEnv m = ExecutionEnv {
      execReturn :: Breaker m Value
    , execNext   :: Breaker m ()
  }

newExecution :: ExecutionEnv m
newExecution = ExecutionEnv {
                 execReturn = fail "Nowhere to return to"
               , execNext = fail "Nowhere to go next"
               }

data SindreEnv m = SindreEnv {
      varEnv    :: VarEnvironment
    , widgetRev :: M.Map WidgetRef Identifier
    , objects   :: Array WidgetRef (DataSlot m)
    , evtQueue  :: Q.Seq (EventSource, Event)
    , functions :: M.Map Identifier Function
  }

type SpaceNeed = Rectangle
type SpaceUse = [Rectangle]

class (Monad m, Functor m, Applicative m) => MonadSubstrate m where
  type SubEvent m :: *
  type InitVal m :: *
  fullRedraw :: Sindre m ()
  getSubEvent :: Sindre m (EventSource, Event)
  printVal :: String -> m ()
  quit :: ExitCode -> m ()

newtype Sindre m a = Sindre (StateT (SindreEnv m)
                             (ReaderT (ExecutionEnv m)
                              (ContT (SindreEnv m) m))
                             a)
  deriving (Functor, Monad, Applicative, MonadCont,
            MonadState (SindreEnv m), MonadReader (ExecutionEnv m))

instance MonadTrans Sindre where
  lift = Sindre . lift . lift . lift

instance MonadIO m => MonadIO (Sindre m) where
  liftIO = Sindre . liftIO

execSindre :: MonadSubstrate m => SindreEnv m -> Sindre m a -> m (SindreEnv m)
execSindre s (Sindre m) = runContT (runReaderT (execStateT m s) newExecution) return

class (MonadSubstrate im, Monad (m im)) => MonadSindre im m where
  sindre :: Sindre im a -> m im a
  subst :: im a -> m im a
  subst = sindre . lift

class MonadSindre im m => EventSender im m where
  source :: m im EventSource

instance MonadSubstrate im => MonadSindre im Sindre where
  sindre = id

setBreak :: MonadSubstrate m => (Breaker m a -> ExecutionEnv m -> ExecutionEnv m) 
         -> Sindre m a -> Sindre m a
setBreak f m = callCC $ flip local m . f

returnHere :: MonadSubstrate m => Sindre m Value -> Sindre m Value
returnHere = setBreak (\breaker env -> env { execReturn = breaker })

doReturn :: MonadSubstrate m => Value -> Sindre m ()
doReturn value = ($ value) =<< asks execReturn

nextHere :: MonadSubstrate m => Sindre m () -> Sindre m ()
nextHere = setBreak (\breaker env -> env { execNext = breaker })

doNext :: MonadSubstrate m => Sindre m ()
doNext = ($ ()) =<< asks execNext

newtype ObjectM o m a = ObjectM (ReaderT WidgetRef (StateT o (Sindre m)) a)
    deriving (Functor, Monad, Applicative, MonadState o, MonadReader WidgetRef)

instance MonadSubstrate im => MonadSindre im (ObjectM o) where
  sindre = ObjectM . lift . lift

instance MonadSubstrate im => EventSender im (ObjectM o) where
  source = ObjectSrc <$> ask

runObjectM :: Object m o => ObjectM o m a -> WidgetRef -> o -> Sindre m (a, o)
runObjectM (ObjectM m) wr = runStateT (runReaderT m wr)

class MonadSubstrate m => Object m s where
  callMethodI :: Identifier -> [Value] -> ObjectM s m Value
  callMethodI m _ = fail $ "Unknown method '" ++ m ++ "'"
  fieldSetI   :: Identifier -> Value -> ObjectM s m Value
  fieldSetI f _ = fail $ "Unknown field '" ++ f ++ "'"
  fieldGetI   :: Identifier -> ObjectM s m Value
  fieldGetI f = fail $ "Unknown field '" ++ f ++ "'"

instance (MonadIO m, MonadSubstrate m) => MonadIO (ObjectM o m) where
  liftIO = sindre . subst . io

newtype WidgetM w m a = WidgetM (ObjectM w m a)
    deriving (Functor, Monad, Applicative, MonadState w,
              MonadReader WidgetRef)

instance MonadSubstrate im => MonadSindre im (WidgetM o) where
  sindre = WidgetM . sindre

instance MonadSubstrate im => EventSender im (WidgetM o) where
  source = ObjectSrc <$> ask

runWidgetM :: Widget m w => WidgetM w m a -> WidgetRef -> w -> Sindre m (a, w)
runWidgetM (WidgetM m) = runObjectM m

class Object m s => Widget m s where
  composeI      :: Rectangle -> WidgetM s m SpaceNeed
  drawI         :: Rectangle -> WidgetM s m SpaceUse
  recvSubEventI :: SubEvent m -> WidgetM s m ()
  recvSubEventI _ = return ()
  recvEventI    :: Event -> WidgetM s m ()
  recvEventI _ = return ()

instance (MonadIO m, MonadSubstrate m) => MonadIO (WidgetM o m) where
  liftIO = sindre . subst . io

data VarBinding = VarBnd Value
                | ConstBnd Value

type VarEnvironment = M.Map Identifier VarBinding

getEvent :: MonadSubstrate m => Sindre m (EventSource, Event)
getEvent = do queue <- gets evtQueue
              case Q.viewl queue of
                e :< queue' -> do modify $ \s -> s { evtQueue = queue' }
                                  return e
                EmptyL      -> getSubEvent

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
    where e = error $ "Undefined variable " ++ k
          v (VarBnd v') = v'
          v (ConstBnd v') = v'

lookupObj :: Identifier -> SindreM WidgetRef
lookupObj k = do
  bnd <- lookupVal k
  case bnd of
    Reference r -> return r
    _           -> error $ "Unknown object '"++k++"'"

lookupFunc :: Identifier -> SindreM Function
lookupFunc k = do
  r <- M.lookup k <$> gets functions
  case r of
    Just f  -> return f
    Nothing -> error $ "Unknown function '"++k++"'"

revLookup :: WidgetRef -> SindreM (Maybe Identifier)
revLookup wr = M.lookup wr <$> gets widgetRev

operateW :: MonadSubstrate m => WidgetRef ->
            (forall o . Widget m o => o -> Sindre m (a, o)) -> Sindre m a
operateW r f = do
  objs <- gets objects
  (v, s') <- case (objs!r) of
               WidgetSlot s -> do (v, s') <- f s
                                  return (v, WidgetSlot s')
               _            -> error "Expected widget"
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v

operateO :: MonadSubstrate m => WidgetRef ->
            (forall o . Object m o => o -> Sindre m (a, o)) -> Sindre m a
operateO r f = do
  objs <- gets objects
  (v, s') <- case (objs!r) of
               WidgetSlot s -> do (v, s') <- f s
                                  return (v, WidgetSlot s')
               ObjectSlot s -> do (v, s') <- f s
                                  return (v, ObjectSlot s')
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v

actionO :: MonadSubstrate m => ObjectRef ->
           (forall o . Object m o => ObjectM o m a) -> Sindre m a
actionO r f = operateO r $ runObjectM f r

callMethod :: MonadSindre im m =>
              WidgetRef -> Identifier -> [Value] -> m im Value
callMethod r m vs = sindre $ actionO r (callMethodI m vs)
fieldSet :: MonadSindre im m =>
            WidgetRef -> Identifier -> Value -> m im Value
fieldSet r f v = sindre $ actionO r $ do
                   old <- fieldGetI f
                   new <- fieldSetI f v
                   changed f old new
                   return new
fieldGet :: MonadSindre im m =>
            ObjectRef -> Identifier -> m im Value
fieldGet r f = sindre $ actionO r (fieldGetI f)

actionW :: MonadSubstrate m => ObjectRef ->
           (forall o . Widget m o => WidgetM o m a) -> Sindre m a
actionW r f = operateW r $ runWidgetM f r

compose :: MonadSindre im m =>
           ObjectRef -> Rectangle -> m im SpaceNeed
compose r rect = sindre $ actionW r (composeI rect)
draw :: MonadSindre im m =>
        ObjectRef -> Rectangle -> m im SpaceUse
draw r rect = sindre $ actionW r (drawI rect)
recvSubEvent :: MonadSindre im m =>
                WidgetRef -> SubEvent im -> m im ()
recvSubEvent r ev = sindre $ actionW r (recvSubEventI ev)
recvEvent :: MonadSindre im m =>
             WidgetRef -> Event -> m im ()
recvEvent r ev = sindre $ actionW r (recvEventI ev)
