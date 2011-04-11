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
                      , WidgetRef
                      , SpaceNeed
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
                 | SubstrSrc
                   deriving (Show)

type Frame = IM.IntMap Value

data SindreEnv m = SindreEnv {
      widgetRev :: M.Map WidgetRef Identifier
    , objects   :: Array WidgetRef (DataSlot m)
    , evtQueue  :: Q.Seq (EventSource, Event)
    , globals   :: IM.IntMap Value
    , execFrame :: Frame
    , rootVal   :: InitVal m
    , arguments :: Arguments
  }

type SpaceNeed = Rectangle
type SpaceUse = [Rectangle]

class (Monad m, Functor m, Applicative m) => MonadSubstrate m where
  type SubEvent m :: *
  type InitVal m :: *
  fullRedraw :: Sindre m ()
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

execSindre :: MonadSubstrate m => SindreEnv m -> Sindre m a -> m ExitCode
execSindre s (Sindre m) = runContT m' return
    where m' = callCC $ \c -> do
                 let quitc code =
                       Sindre $ lift $ lift $ c code
                 _ <- execStateT (runReaderT m quitc) s
                 return ExitSuccess

quitSindre :: MonadSubstrate m => ExitCode -> Sindre m ()
quitSindre code = ($ code) =<< ask

class (MonadSubstrate im, Monad (m im)) => MonadSindre im m where
  sindre :: Sindre im a -> m im a
  subst :: im a -> m im a
  subst = sindre . lift

class MonadSindre im m => EventSender im m where
  source :: m im EventSource

instance MonadSubstrate im => MonadSindre im Sindre where
  sindre = id

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

globalVal :: MonadSubstrate m => IM.Key -> Sindre m Value
globalVal k = IM.findWithDefault falsity k <$> gets globals

setGlobal :: MonadSubstrate m => IM.Key -> Value -> Sindre m ()
setGlobal k v =
  modify $ \s ->
    s { globals = IM.insert k v $ globals s }

revLookup :: MonadSubstrate m => WidgetRef -> Sindre m (Maybe Identifier)
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

type Breaker m a = (Frame, a -> Execution m ())

data ExecutionEnv m = ExecutionEnv {
      execReturn :: Breaker m Value
    , execNext   :: Breaker m ()
  }

setBreak :: MonadSubstrate m =>
            (Breaker m a -> ExecutionEnv m -> ExecutionEnv m) 
         -> Execution m a -> Execution m a
setBreak f m = do
  frame <- sindre $ gets execFrame
  callCC $ flip local m . f . (,) frame

doBreak :: MonadSubstrate m =>
           (ExecutionEnv m -> Breaker m a) -> a -> Execution m ()
doBreak b x = do
  (frame, f) <- asks b
  sindre $ modify $ \s -> s { execFrame = frame }
  f x

returnHere :: MonadSubstrate m => Execution m Value -> Execution m Value
returnHere = setBreak (\breaker env -> env { execReturn = breaker })

doReturn :: MonadSubstrate m => Value -> Execution m ()
doReturn = doBreak execReturn

nextHere :: MonadSubstrate m => Execution m () -> Execution m ()
nextHere = setBreak (\breaker env -> env { execNext = breaker })

doNext :: MonadSubstrate m => Execution m ()
doNext = doBreak execNext ()

newtype Execution m a = Execution (ReaderT (ExecutionEnv m) (Sindre m) a)
    deriving (Functor, Monad, Applicative, MonadReader (ExecutionEnv m), MonadCont)

execute :: MonadSubstrate m => Execution m Value -> Sindre m Value
execute m = runReaderT m' env
    where env = ExecutionEnv {
                  execReturn = (IM.empty, fail "Nowhere to return to")
                , execNext   = (IM.empty, fail "Nowhere to go next")
               }
          Execution m' = returnHere m


execute_ :: MonadSubstrate m => Execution m a -> Sindre m ()
execute_ m = execute (m *> return (IntegerV 0)) >> return ()

instance MonadSubstrate im => MonadSindre im Execution where
  sindre = Execution . lift

data ScopedExecution m a = ScopedExecution (Execution m a)

enterScope :: MonadSubstrate m => [Value] -> ScopedExecution m a -> Execution m a
enterScope vs (ScopedExecution ex) = do
  oldframe <- sindre $ gets execFrame
  sindre $ modify $ \s -> s { execFrame = m }
  ex <* sindre (modify $ \s -> s { execFrame = oldframe })
    where m = IM.fromList $ zip [0..] vs

lexicalVal :: MonadSubstrate m => IM.Key -> Execution m Value
lexicalVal k = IM.findWithDefault falsity k <$> sindre (gets execFrame)

setLexical :: MonadSubstrate m => IM.Key -> Value -> Execution m ()
setLexical k v = sindre $ modify $ \s ->
  s { execFrame = IM.insert k v $ execFrame s }


type EventHandler m = (EventSource, Event) -> Execution m ()

eventLoop :: MonadSubstrate m => EventHandler m -> Sindre m ()
eventLoop handler = forever $ do
  fullRedraw
  ev <- getEvent
  execute $ do
    nextHere $ handler ev
    return falsity

class Mold a where
  mold :: Value -> Maybe a

instance Mold String where
  mold (IntegerV v) = Just $ show v
  mold (Reference r) = Just $ "#<object at "++show r++">"
  mold (Dict m) = Just $ "#<dictionary with "++show (M.size m)++" entries>"
  mold (StringV v) = Just $ v

instance Mold Integer where
  mold (Reference v') = Just $ fi v'
  mold (IntegerV x) = Just x
  mold (StringV s) = parseInteger s
  mold _ = Nothing

printed :: MonadSubstrate m => Value -> Sindre m String
printed v@(Reference v') = fromMaybe (show v) <$> revLookup v'
printed v = return $ fromMaybe "#<unprintable object>" $ mold v
