{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
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
                      , quitSindre
                      , MonadSindre(..)
                      , EventSender
                      , EventSource(..)
                      , broadcast
                      , changed
                      , MonadBackend(..)
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
                      , recvEvent
                      , DataSlot(..)
                      , SindreEnv(..)
                      , globalVal
                      , setGlobal
                      , revLookup
                      , Execution
                      , execute
                      , execute_
                      , returnHere
                      , doReturn
                      , nextHere
                      , doNext
                      , ScopedExecution(..)
                      , enterScope
                      , lexicalVal
                      , setLexical
                      , eventLoop
                      , EventHandler
                      , Mold(..)
                      , printed
                      , method
                      )
    where

import Sindre.Parser(parseInteger)
import Sindre.Sindre
import Sindre.Util

import System.Exit

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Data.Array
import Data.Maybe
import Data.Monoid
import Data.Sequence((|>), ViewL(..))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Sequence as Q

data DataSlot m = forall s . Widget m s => WidgetSlot s
                | forall s . Object m s => ObjectSlot s

data EventSource = ObjectSrc ObjectRef
                 | BackendSrc
                   deriving (Show)

type Frame = IM.IntMap Value

data SindreEnv m = SindreEnv {
      widgetRev :: M.Map ObjectNum Identifier
    , objects   :: Array ObjectNum (DataSlot m)
    , evtQueue  :: Q.Seq (EventSource, Event)
    , globals   :: IM.IntMap Value
    , execFrame :: Frame
    , rootVal   :: InitVal m
    , guiRoot   :: (Maybe Orient, WidgetRef)
    , kbdFocus  :: WidgetRef
    , arguments :: Arguments
  }

class (Monad m, Functor m, Applicative m) => MonadBackend m where
  type SubEvent m :: *
  type InitVal m :: *
  fullRedraw :: (Maybe String, WidgetRef) -> Sindre m ()
  getSubEvent :: Sindre m (EventSource, Event)
  printVal :: String -> m ()

type QuitFun m = ExitCode -> Sindre m ()

newtype Sindre m a = Sindre (ReaderT (QuitFun m)
                             (StateT (SindreEnv m)
                              (ContT ExitCode m))
                             a)
  deriving (Functor, Monad, Applicative, MonadCont,
            MonadState (SindreEnv m), MonadReader (QuitFun m))

instance MonadTrans Sindre where
  lift = Sindre . lift . lift . lift

instance MonadIO m => MonadIO (Sindre m) where
  liftIO = Sindre . liftIO

instance Monoid (Sindre m ()) where
  mempty = return ()
  mappend = (>>)
  mconcat = sequence_

execSindre :: MonadBackend m => SindreEnv m -> Sindre m a -> m ExitCode
execSindre s (Sindre m) = runContT m' return
    where m' = callCC $ \c -> do
                 let quitc code =
                       Sindre $ lift $ lift $ c code
                 _ <- execStateT (runReaderT m quitc) s
                 return ExitSuccess

quitSindre :: MonadBackend m => ExitCode -> Sindre m ()
quitSindre code = ($ code) =<< ask

class (MonadBackend im, Monad (m im)) => MonadSindre im m where
  sindre :: Sindre im a -> m im a
  back :: im a -> m im a
  back = sindre . lift

class MonadSindre im m => EventSender im m where
  source :: m im EventSource

instance MonadBackend im => MonadSindre im Sindre where
  sindre = id

newtype ObjectM o m a = ObjectM (ReaderT ObjectRef (StateT o (Sindre m)) a)
    deriving (Functor, Monad, Applicative, MonadState o, MonadReader ObjectRef)

instance MonadBackend im => MonadSindre im (ObjectM o) where
  sindre = ObjectM . lift . lift

instance MonadBackend im => EventSender im (ObjectM o) where
  source = ObjectSrc <$> ask

runObjectM :: Object m o => ObjectM o m a -> ObjectRef -> o -> Sindre m (a, o)
runObjectM (ObjectM m) wr = runStateT (runReaderT m wr)

class MonadBackend m => Object m s where
  callMethodI :: Identifier -> [Value] -> ObjectM s m Value
  callMethodI m _ = fail $ "Unknown method '" ++ m ++ "'"
  fieldSetI   :: Identifier -> Value -> ObjectM s m Value
  fieldSetI f _ = fail $ "Unknown field '" ++ f ++ "'"
  fieldGetI   :: Identifier -> ObjectM s m Value
  fieldGetI f = fail $ "Unknown field '" ++ f ++ "'"

instance (MonadIO m, MonadBackend m) => MonadIO (ObjectM o m) where
  liftIO = sindre . back . io

newtype WidgetM w m a = WidgetM (ObjectM w m a)
    deriving (Functor, Monad, Applicative, MonadState w,
              MonadReader WidgetRef)

instance MonadBackend im => MonadSindre im (WidgetM o) where
  sindre = WidgetM . sindre

instance MonadBackend im => EventSender im (WidgetM o) where
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

instance (MonadIO m, MonadBackend m) => MonadIO (WidgetM o m) where
  liftIO = sindre . back . io

getEvent :: MonadBackend m => Sindre m (EventSource, Event)
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

globalVal :: MonadBackend m => IM.Key -> Sindre m Value
globalVal k = IM.findWithDefault falsity k <$> gets globals

setGlobal :: MonadBackend m => IM.Key -> Value -> Sindre m ()
setGlobal k v =
  modify $ \s ->
    s { globals = IM.insert k v $ globals s }

revLookup :: MonadBackend m => WidgetRef -> Sindre m (Maybe Identifier)
revLookup (k, _) = M.lookup k <$> gets widgetRev

operateW :: MonadBackend m => WidgetRef ->
            (forall o . Widget m o => o -> Sindre m (a, o)) -> Sindre m a
operateW (r,_) f = do
  objs <- gets objects
  (v, s') <- case (objs!r) of
               WidgetSlot s -> do (v, s') <- f s
                                  return (v, WidgetSlot s')
               _            -> error "Expected widget"
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v

operateO :: MonadBackend m => ObjectRef ->
            (forall o . Object m o => o -> Sindre m (a, o)) -> Sindre m a
operateO (r,_) f = do
  objs <- gets objects
  (v, s') <- case (objs!r) of
               WidgetSlot s -> do (v, s') <- f s
                                  return (v, WidgetSlot s')
               ObjectSlot s -> do (v, s') <- f s
                                  return (v, ObjectSlot s')
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v

actionO :: MonadBackend m => ObjectRef ->
           (forall o . Object m o => ObjectM o m a) -> Sindre m a
actionO r f = operateO r $ runObjectM f r

callMethod :: MonadSindre im m =>
              ObjectRef -> Identifier -> [Value] -> m im Value
callMethod r m vs = sindre $ actionO r (callMethodI m vs)
fieldSet :: MonadSindre im m =>
            ObjectRef -> Identifier -> Value -> m im Value
fieldSet r f v = sindre $ actionO r $ do
                   old <- fieldGetI f
                   new <- fieldSetI f v
                   changed f old new
                   return new
fieldGet :: MonadSindre im m =>
            ObjectRef -> Identifier -> m im Value
fieldGet r f = sindre $ actionO r (fieldGetI f)

actionW :: MonadBackend m => WidgetRef ->
           (forall o . Widget m o => WidgetM o m a) -> Sindre m a
actionW r f = operateW r $ runWidgetM f r

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

type Breaker m a = (Frame, a -> Execution m ())

data ExecutionEnv m = ExecutionEnv {
      execReturn :: Breaker m Value
    , execNext   :: Breaker m ()
  }

setBreak :: MonadBackend m =>
            (Breaker m a -> ExecutionEnv m -> ExecutionEnv m) 
         -> Execution m a -> Execution m a
setBreak f m = do
  frame <- sindre $ gets execFrame
  callCC $ flip local m . f . (,) frame

doBreak :: MonadBackend m =>
           (ExecutionEnv m -> Breaker m a) -> a -> Execution m ()
doBreak b x = do
  (frame, f) <- asks b
  sindre $ modify $ \s -> s { execFrame = frame }
  f x

returnHere :: MonadBackend m => Execution m Value -> Execution m Value
returnHere = setBreak (\breaker env -> env { execReturn = breaker })

doReturn :: MonadBackend m => Value -> Execution m ()
doReturn = doBreak execReturn

nextHere :: MonadBackend m => Execution m () -> Execution m ()
nextHere = setBreak (\breaker env -> env { execNext = breaker })

doNext :: MonadBackend m => Execution m ()
doNext = doBreak execNext ()

newtype Execution m a = Execution (ReaderT (ExecutionEnv m) (Sindre m) a)
    deriving (Functor, Monad, Applicative, MonadReader (ExecutionEnv m), MonadCont)

execute :: MonadBackend m => Execution m Value -> Sindre m Value
execute m = runReaderT m' env
    where env = ExecutionEnv {
                  execReturn = (IM.empty, fail "Nowhere to return to")
                , execNext   = (IM.empty, fail "Nowhere to go next")
               }
          Execution m' = returnHere m


execute_ :: MonadBackend m => Execution m a -> Sindre m ()
execute_ m = execute (m *> return (IntegerV 0)) >> return ()

instance MonadBackend im => MonadSindre im Execution where
  sindre = Execution . lift

data ScopedExecution m a = ScopedExecution (Execution m a)

enterScope :: MonadBackend m => [Value] -> ScopedExecution m a -> Execution m a
enterScope vs (ScopedExecution ex) = do
  oldframe <- sindre $ gets execFrame
  sindre $ modify $ \s -> s { execFrame = m }
  ex <* sindre (modify $ \s -> s { execFrame = oldframe })
    where m = IM.fromList $ zip [0..] vs

lexicalVal :: MonadBackend m => IM.Key -> Execution m Value
lexicalVal k = IM.findWithDefault falsity k <$> sindre (gets execFrame)

setLexical :: MonadBackend m => IM.Key -> Value -> Execution m ()
setLexical k v = sindre $ modify $ \s ->
  s { execFrame = IM.insert k v $ execFrame s }


type EventHandler m = (EventSource, Event) -> Execution m ()

eventLoop :: MonadBackend m => EventHandler m -> Sindre m ()
eventLoop handler = forever $ do
  fullRedraw =<< gets guiRoot
  ev <- getEvent
  execute $ do
    nextHere $ handler ev
    return falsity

class Mold a where
  mold :: Value -> Maybe a
  unmold :: a -> Value

objStr :: ObjectRef -> String
objStr (r, c) = "#<" ++ c ++ " at "++show r++">"

instance Mold String where
  mold (IntegerV v) = Just $ show v
  mold (Reference v) = Just $ objStr v
  mold (Dict m) = Just $ "#<dictionary with "++show (M.size m)++" entries>"
  mold (StringV v) = Just v
  unmold = StringV

instance Mold Integer where
  mold (Reference (v', _)) = Just $ fi v'
  mold (IntegerV x) = Just x
  mold (StringV s) = parseInteger s
  mold _ = Nothing
  unmold = IntegerV

instance Mold () where
  mold   _ = Just ()
  unmold _ = IntegerV 0

printed :: MonadBackend m => Value -> Sindre m String
printed (Reference v) = fromMaybe (objStr v) <$> revLookup v
printed v = return $ fromMaybe "#<unprintable object>" $ mold v

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
