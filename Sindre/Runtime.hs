{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Runtime
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Definitions for the Sindre runtime environment.
--
-----------------------------------------------------------------------------
module Sindre.Runtime ( Sindre
                      , execSindre
                      , quitSindre
                      , MonadSindre(..)
                      , broadcast
                      , changed
                      , redraw
                      , fullRedraw
                      , setRootPosition
                      , MonadBackend(..)
                      , Object(..)
                      , ObjectM
                      , fieldSet
                      , fieldGet
                      , callMethod
                      , Widget(..)
                      , draw
                      , compose
                      , recvEvent
                      , DataSlot(..)
                      , WidgetState(..)
                      , SindreEnv(..)
                      , newEnv
                      , globalVal
                      , setGlobal
                      , Execution
                      , execute
                      , execute_
                      , returnHere
                      , doReturn
                      , nextHere
                      , doNext
                      , breakHere
                      , doBreak
                      , contHere
                      , doCont
                      , setScope
                      , enterScope
                      , lexicalVal
                      , setLexical
                      , eventLoop
                      , EventHandler
                      , Mold(..)
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
import qualified Data.Set as S
import qualified Data.Sequence as Q
import qualified Data.Text as T

data WidgetState = WidgetState { constraints :: Constraints
                               , dimensions  :: Rectangle
                               }

data DataSlot m = forall s . Widget m s => WidgetSlot s WidgetState
                | forall s . Object m s => ObjectSlot s

type Frame = IM.IntMap Value

data Redraw = RedrawAll | RedrawSome (S.Set WidgetRef)

data SindreEnv m = SindreEnv {
    objects     :: Array ObjectNum (DataSlot m)
  , evtQueue    :: Q.Seq Event
  , globals     :: IM.IntMap Value
  , execFrame   :: Frame
  , kbdFocus    :: WidgetRef
  , rootWidget  :: (Maybe (RootPosition m), WidgetRef)
  , arguments   :: Arguments
  , needsRedraw :: Redraw
  }

newEnv :: WidgetRef -> Arguments -> SindreEnv m
newEnv rootwr argv =
  SindreEnv { objects   = array (0, -1) []
            , evtQueue  = Q.empty
            , globals   = IM.empty
            , execFrame = IM.empty
            , kbdFocus  = rootwr
            , rootWidget = (Nothing, rootwr)
            , arguments = argv
            , needsRedraw = RedrawAll
            }

-- | A monad that can be used as the layer beneath 'Sindre'.
class (Monad m, Functor m, Applicative m, Mold (RootPosition m)) => MonadBackend m where
  type BackEvent m :: *
  type RootPosition m :: *
  redrawRoot :: Sindre m ()
  redrawRegion :: [Rectangle] -> Sindre m ()
  getBackEvent :: Sindre m (Maybe Event)
  waitForBackEvent :: Sindre m Event
  printVal :: String -> m ()

type QuitFun m = ExitCode -> Sindre m ()

-- | The main monad in which a Sindre program executes.  More
-- specialised monads, such as 'Execution' are used for specific
-- purposes, but they all run on top of the Sindre monad.
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

-- | @execSindre e m@ executes the action @m@ in environment @e@,
-- returning the exit code of @m@.
execSindre :: MonadBackend m => SindreEnv m -> Sindre m a -> m ExitCode
execSindre s (Sindre m) = runContT m' return
    where m' = callCC $ \c -> do
                 let quitc code =
                       Sindre $ lift $ lift $ c code
                 _ <- execStateT (runReaderT m quitc) s
                 return ExitSuccess

-- | Immediately return from 'execSindre', returning the given exit
-- code.
quitSindre :: MonadBackend m => ExitCode -> Sindre m ()
quitSindre code = ($ code) =<< ask

-- | @MonadSindre im m@ is the class of monads @m@ that run on top of
-- 'Sindre' with backend @im@, and can thus access Sindre
-- functionality.
class (MonadBackend im, Monad (m im)) => MonadSindre im m where
  -- | Lift a 'Sindre' operation into this monad.
  sindre :: Sindre im a -> m im a
  -- | Lift a backend operation into this monad.
  back :: im a -> m im a
  back = sindre . lift

instance MonadBackend im => MonadSindre im Sindre where
  sindre = id

newtype ObjectM o m a = ObjectM (ReaderT ObjectRef (StateT o (Sindre m)) a)
    deriving (Functor, Monad, Applicative, MonadState o, MonadReader ObjectRef)

instance MonadBackend im => MonadSindre im (ObjectM o) where
  sindre = ObjectM . lift . lift

runObjectM :: Object m o => ObjectM o m a -> ObjectRef -> o -> Sindre m (a, o)
runObjectM (ObjectM m) wr = runStateT (runReaderT m wr)

class MonadBackend m => Object m s where
  callMethodI :: Identifier -> [Value] -> ObjectM s m Value
  callMethodI m _ = fail $ "Unknown method '" ++ m ++ "'"
  fieldSetI   :: Identifier -> Value -> ObjectM s m Value
  fieldSetI f _ = fail $ "Unknown field '" ++ f ++ "'"
  fieldGetI   :: Identifier -> ObjectM s m Value
  fieldGetI f = fail $ "Unknown field '" ++ f ++ "'"
  recvEventI    :: Event -> ObjectM s m ()
  recvEventI _ = return ()

instance (MonadIO m, MonadBackend m) => MonadIO (ObjectM o m) where
  liftIO = sindre . back . io

class Object m s => Widget m s where
  composeI      :: ObjectM s m SpaceNeed
  drawI         :: Rectangle -> ObjectM s m SpaceUse

popQueue :: Sindre m (Maybe Event)
popQueue = do queue <- gets evtQueue
              case Q.viewl queue of
                e :< queue' -> do modify $ \s -> s { evtQueue = queue' }
                                  return $ Just e
                EmptyL      -> return Nothing

getEvent :: MonadBackend m => Sindre m (Maybe Event)
getEvent = liftM2 mplus getBackEvent popQueue

waitForEvent :: MonadBackend m => Sindre m Event
waitForEvent = liftM2 fromMaybe waitForBackEvent popQueue

broadcast :: MonadBackend im => Event -> ObjectM o im ()
broadcast e = sindre $ modify $ \s -> s { evtQueue = evtQueue s |> e }

changed :: MonadBackend im =>
           Identifier -> Value -> Value -> ObjectM o im ()
changed f old new = do
  this <- ask
  broadcast $ NamedEvent "changed" [old, new] $ FieldSrc this f

redraw :: (MonadBackend im, Widget im s) => ObjectM s im ()
redraw = do r <- ask
            sindre $ modify $ \s ->
              s { needsRedraw = needsRedraw s `add` r }
            fullRedraw
    where add RedrawAll      _ = RedrawAll
          add (RedrawSome s) w = RedrawSome $ w `S.insert` s

fullRedraw :: MonadSindre im m => m im ()
fullRedraw = sindre $ modify $ \s ->
             case needsRedraw s of
               RedrawAll  -> s
               _          -> s { needsRedraw = RedrawAll }

setRootPosition :: MonadBackend m => Value -> Sindre m ()
setRootPosition v =
  case mold v of
    Nothing -> fail $ "Value " ++ show v ++ " not a valid root widget position."
    Just v' -> modify $ \s -> s { rootWidget = (Just v', snd $ rootWidget s) }

globalVal :: MonadBackend m => IM.Key -> Sindre m Value
globalVal k = IM.findWithDefault falsity k <$> gets globals

setGlobal :: MonadBackend m => IM.Key -> Value -> Sindre m ()
setGlobal k v =
  modify $ \s ->
    s { globals = IM.insert k v $ globals s }

operateW :: MonadBackend m => WidgetRef ->
            (forall o . Widget m o => o -> WidgetState -> Sindre m (a, o, WidgetState))
         -> Sindre m a
operateW (r,_,_) f = do
  objs <- gets objects
  (v, s') <- case objs!r of
               WidgetSlot o s -> do (v, o', s') <- f o s
                                    return (v, WidgetSlot o' s')
               _            -> fail "Expected widget"
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v

operateO :: MonadBackend m => ObjectRef ->
            (forall o . Object m o => o -> Sindre m (a, o)) -> Sindre m a
operateO (r,_,_) f = do
  objs <- gets objects
  (v, s') <- case objs!r of
               WidgetSlot s sz -> do (v, s') <- f s
                                     return (v, WidgetSlot s' sz)
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
fieldGet :: MonadSindre im m => ObjectRef -> Identifier -> m im Value
fieldGet r f = sindre $ actionO r (fieldGetI f)
recvEvent :: MonadSindre im m => WidgetRef -> Event -> m im ()
recvEvent r ev = sindre $ actionO r (recvEventI ev)

compose :: MonadSindre im m => WidgetRef -> m im SpaceNeed
compose r = sindre $ operateW r $ \w s -> do
  (need, w') <- runObjectM composeI r w
  return (constrainNeed need $ constraints s, w', s)
draw :: MonadSindre im m =>
        WidgetRef -> Maybe Rectangle -> m im SpaceUse
draw r rect = sindre $ operateW r $ \w s -> do
  let rect' = fromMaybe (dimensions s) rect
  (use, w') <- runObjectM (drawI rect') r w
  return (use, w', s { dimensions = rect' })

type Jumper m a = a -> Execution m ()

data ExecutionEnv m = ExecutionEnv {
      execReturn :: Jumper m Value
    , execNext   :: Jumper m ()
    , execBreak  :: Jumper m ()
    , execCont   :: Jumper m ()
  }

setJump :: MonadBackend m =>
            (Jumper m a -> ExecutionEnv m -> ExecutionEnv m) 
         -> Execution m a -> Execution m a
setJump f m = callCC $ flip local m . f

doJump :: MonadBackend m =>
           (ExecutionEnv m -> Jumper m a) -> a -> Execution m ()
doJump b x = join $ asks b <*> pure x

returnHere :: MonadBackend m => Execution m Value -> Execution m Value
returnHere = setJump (\breaker env -> env { execReturn = breaker })

doReturn :: MonadBackend m => Value -> Execution m ()
doReturn = doJump execReturn

nextHere :: MonadBackend m => Execution m () -> Execution m ()
nextHere = setJump (\breaker env -> env { execNext = breaker })

doNext :: MonadBackend m => Execution m ()
doNext = doJump execNext ()

breakHere :: MonadBackend m => Execution m () -> Execution m ()
breakHere = setJump (\breaker env -> env { execBreak = breaker })

doBreak :: MonadBackend m => Execution m ()
doBreak = doJump execBreak ()

contHere :: MonadBackend m => Execution m () -> Execution m ()
contHere = setJump (\breaker env -> env { execCont = breaker })

doCont :: MonadBackend m => Execution m ()
doCont = doJump execCont ()

newtype Execution m a = Execution (ReaderT (ExecutionEnv m) (Sindre m) a)
    deriving (Functor, Monad, Applicative, MonadReader (ExecutionEnv m), MonadCont)

execute :: MonadBackend m => Execution m Value -> Sindre m Value
execute m = runReaderT m' env
    where env = ExecutionEnv {
                  execReturn = fail "Nowhere to return to"
                , execNext   = fail "Nowhere to go next"
                , execBreak  = fail "Not in a loop"
                , execCont   = fail "Not in a loop"
               }
          Execution m' = returnHere m


execute_ :: MonadBackend m => Execution m a -> Sindre m ()
execute_ m = execute (m *> return (Number 0)) >> return ()

instance MonadBackend im => MonadSindre im Execution where
  sindre = Execution . lift

setScope :: MonadBackend m => [Value] -> Execution m a -> Execution m a
setScope vs ex =
  sindre (modify $ \s -> s { execFrame = m }) >> ex
    where m = IM.fromList $ zip [0..] vs

enterScope :: MonadBackend m => [Value] -> Execution m a -> Execution m a
enterScope vs se = do
  oldframe <- sindre $ gets execFrame
  setScope vs se <* sindre (modify $ \s -> s { execFrame = oldframe })

lexicalVal :: MonadBackend m => IM.Key -> Execution m Value
lexicalVal k = IM.findWithDefault falsity k <$> sindre (gets execFrame)

setLexical :: MonadBackend m => IM.Key -> Value -> Execution m ()
setLexical k v = sindre $ modify $ \s ->
  s { execFrame = IM.insert k v $ execFrame s }

type EventHandler m = Event -> Execution m ()

eventLoop :: MonadBackend m => EventHandler m -> Sindre m ()
eventLoop handler = do
  let redraw_ RedrawAll      = redrawRoot
      redraw_ (RedrawSome s) = concat <$> mapM (`draw` Nothing) (S.toList s)
                               >>= redrawRegion
  forever $ do
    process
    redraw_ =<< gets needsRedraw
    modify $ \s -> s { needsRedraw = RedrawSome S.empty }
    handle =<< waitForEvent
  where handle ev = execute $ nextHere (handler ev) >> return falsity
        process = do ev <- getEvent
                     case ev of
                       Just ev' -> handle ev' >> process
                       Nothing  -> return ()

class Mold a where
  mold :: Value -> Maybe a
  unmold :: a -> Value

instance Mold Value where
  mold = Just
  unmold = id

instance Mold String where
  mold = Just . show
  unmold = string

instance Mold T.Text where
  mold = Just . T.pack . show
  unmold = StringV

instance Mold Double where
  mold (Reference (v', _, _)) = Just $ fi v'
  mold (Number x) = Just x
  mold s = parseInteger (show s)
  unmold = Number

instance Mold Integer where
  mold (Reference (v', _, _)) = Just $ fi v'
  mold (Number x) = Just $ round x
  mold s = round <$> parseInteger (show s)
  unmold = Number . fromInteger

instance Mold Int where
  mold = liftM (fi :: Integer -> Int) . mold
  unmold = Number . fromIntegral

instance Mold Bool where
  mold = Just . true
  unmold False = falsity
  unmold True = truth

instance Mold () where
  mold   _ = Just ()
  unmold _ = Number 0

aligns :: [(String, (Align, Align))]
aligns = [ ("top",      (AlignCenter, AlignNeg))
         , ("topleft",  (AlignNeg, AlignNeg))
         , ("topright", (AlignPos, AlignNeg))
         , ("bot",      (AlignCenter, AlignPos))
         , ("botleft",  (AlignNeg, AlignPos))
         , ("botright", (AlignPos, AlignPos))
         , ("mid",      (AlignCenter, AlignCenter))
         , ("midleft",  (AlignNeg, AlignCenter))
         , ("midright", (AlignPos, AlignCenter))]

instance Mold (Align, Align) where
  mold s = mold s >>= flip lookup aligns
  unmold a = maybe (Number 0) string $
             lookup a (map (uncurry $ flip (,)) aligns)
