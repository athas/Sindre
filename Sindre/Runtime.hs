{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
                      , NewObject
                      , newObject
                      , NewWidget
                      , newWidget
                      , DataSlot
                      , instWidget
                      , instObject
                      , FieldDesc(..)
                      , fieldName
                      , getField
                      , field
                      , Field
                      , Method
                      , ObjectM
                      , setFieldByRef
                      , getFieldByRef
                      , callMethodByRef
                      , recvEventByRef
                      , draw
                      , compose
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
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import qualified Data.Text as T

-- | A typed description of a field, which may be read-write or
-- read-only.  When constructing the actual widget, you must turn
-- these into real 'Field's by using the 'field' function.  A
-- description of a field consists of a name and monadic actions for
-- reading and optionally writing to the field.
data FieldDesc s im v = ReadWriteField Identifier (ObjectM s im v) (v -> ObjectM s im ())
                      | ReadOnlyField Identifier (ObjectM s im v)

fieldName :: FieldDesc s im v -> Identifier
fieldName (ReadWriteField n _ _) = n
fieldName (ReadOnlyField n _) = n

getField :: FieldDesc s im v -> ObjectM s im v
getField (ReadWriteField _ g _) = g
getField (ReadOnlyField _ g)  = g

-- | An opaque notion of a field.  These are for internal use in the
-- Sindre runtime.
data Field s im = Field { fieldID :: Identifier
                        , fieldGetter :: ObjectM s im Value
                        , fieldSetter :: Value -> ObjectM s im ()
                        }

-- | Turn a Haskell-typed high-level field description into a
-- 'Value'-typed field.
field :: (MonadFail im, Mold v) => FieldDesc s im v -> Field s im
field (ReadOnlyField name bgetter) = Field name (unmold <$> bgetter) problem
  where problem = const $ fail "Field is read-only"
field (ReadWriteField name bgetter bsetter) =
  Field name (unmold <$> bgetter) setter
  where setter v = maybe problem bsetter $ mold v
          where problem = fail $ "Cannot convert " ++ show v ++ " to expected type"

-- | A method takes as arguments a list of 'Value's and returns
-- another 'Value'.  You probably do not want to call these directly
-- from Haskell code, as they are dynamically typed.  See
-- 'Sindre.Lib.function' for a convenient way to turn a Haskell
-- function into a suitable method.
type Method s im = [Value] -> ObjectM s im Value

-- | Container describing a newly created widget.
data NewWidget im = forall s . NewWidget (Object s im)
                                (ObjectM s im SpaceNeed)
                                (Rectangle -> ObjectM s im SpaceUse)
-- | Container describing a newly created object.
data NewObject im = forall s . NewObject (Object s im)

newWidget :: s
          -> M.Map Identifier (Method s im)
          -> [Field s im]
          -> (Event -> ObjectM s im ())
          -> ObjectM s im SpaceNeed
          -> (Rectangle -> ObjectM s im SpaceUse)
          -> NewWidget im
newWidget s ms fs h =
  NewWidget $ Object s ms (M.fromList $ zip (map fieldID fs) fs) h

newObject :: s
          -> M.Map Identifier (Method s im)
          -> [Field s im]
          -> (Event -> ObjectM s im ())
          -> NewObject im
newObject s ms fs h =
  NewObject $ Object s ms (M.fromList $ zip (map fieldID fs) fs) h

data Object s im = Object { objectState   :: s
                          , objectMethods :: M.Map Identifier (Method s im)
                          , objectFields  :: M.Map Identifier (Field s im)
                          , objectHandler :: Event -> ObjectM s im () }

data Widget s im = Widget { widgetObject      :: Object s im
                          , widgetCompose     :: ObjectM s im SpaceNeed
                          , widgetDraw        :: Rectangle -> ObjectM s im SpaceUse
                          , widgetConstraints :: Constraints
                          , widgetDimensions  :: Rectangle }

widgetState :: Widget s im -> s
widgetState = objectState . widgetObject

data DataSlot im = forall s . WidgetSlot (Widget s im)
                 | forall s . ObjectSlot (Object s im)

instWidget :: NewWidget im -> Constraints -> DataSlot im
instWidget (NewWidget s c d) con = WidgetSlot $ Widget s c d con mempty

instObject :: NewObject im -> DataSlot im
instObject (NewObject o) = ObjectSlot o

callMethodI :: MonadFail im => Identifier -> [Value] -> ObjectRef -> Object s im -> Sindre im (Value, s)
callMethodI m vs k s = case M.lookup m $ objectMethods s of
                         Nothing -> fail "No such method"
                         Just m' -> runObjectM (m' vs) k $ objectState s

getFieldI :: MonadFail im => Identifier -> ObjectRef -> Object s im -> Sindre im (Value, s)
getFieldI f k s = case M.lookup f $ objectFields s of
                    Nothing -> fail "No such field"
                    Just f'  -> runObjectM (fieldGetter f') k $ objectState s

setFieldI :: MonadFail im => Identifier -> Value -> ObjectRef -> Object s im -> Sindre im (Value, s)
setFieldI f v k s = case M.lookup f $ objectFields s of
                      Nothing -> fail "No such field"
                      Just f' -> runObjectM (setget f') k $ objectState s
  where setget f' = fieldSetter f' v >> fieldGetter f'

recvEventI :: Event -> ObjectRef -> Object s im -> Sindre im ((), s)
recvEventI e k s = runObjectM (objectHandler s e) k $ objectState s

composeI :: ObjectRef -> Widget s im -> Sindre im (SpaceNeed, s)
composeI k s = runObjectM (widgetCompose s) k $ objectState $ widgetObject s

drawI :: Rectangle -> ObjectRef -> Widget s im -> Sindre im (SpaceUse, s)
drawI r k s = runObjectM (widgetDraw s r) k $ widgetState s

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
class (MonadIO m, MonadFail m, Mold (RootPosition m)) => MonadBackend m where
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
  deriving (Functor, Monad, Applicative, MonadFail, MonadCont,
            MonadState (SindreEnv m), MonadReader (QuitFun m))

instance MonadTrans Sindre where
  lift = Sindre . lift . lift . lift

instance MonadIO m => MonadIO (Sindre m) where
  liftIO = Sindre . liftIO

instance Monoid (Sindre m ()) where
  mempty = return ()
  mconcat = sequence_

instance Semigroup (Sindre m ()) where
  (<>) = (>>)

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
class (MonadBackend im, MonadFail (m im), MonadFail im) => MonadSindre im m where
  -- | Lift a 'Sindre' operation into this monad.
  sindre :: Sindre im a -> m im a
  -- | Lift a backend operation into this monad.
  back :: im a -> m im a
  back = sindre . lift

instance MonadBackend im => MonadSindre im Sindre where
  sindre = id

newtype ObjectM s im a = ObjectM (ReaderT ObjectRef (StateT s (Sindre im)) a)
    deriving (Functor, Monad, MonadFail, Applicative, MonadState s, MonadReader ObjectRef)

instance MonadBackend im => MonadSindre im (ObjectM o) where
  sindre = ObjectM . lift . lift

runObjectM :: ObjectM s im a -> ObjectRef -> s -> Sindre im (a, s)
runObjectM (ObjectM m) wr = runStateT (runReaderT m wr)

instance (MonadIO m, MonadBackend m) => MonadIO (ObjectM o m) where
  liftIO = sindre . back . io

popQueue :: Sindre m (Maybe Event)
popQueue = do queue <- gets evtQueue
              case Q.viewl queue of
                e :< queue' -> do modify $ \s -> s { evtQueue = queue' }
                                  return $ Just e
                EmptyL      -> return Nothing

getEvent :: MonadBackend m => Sindre m (Maybe Event)
getEvent = maybe popQueue (return . Just) =<< getBackEvent

waitForEvent :: MonadBackend m => Sindre m Event
waitForEvent = maybe waitForBackEvent return =<< popQueue

broadcast :: MonadBackend im => Event -> ObjectM o im ()
broadcast e = sindre $ modify $ \s -> s { evtQueue = evtQueue s |> e }

changed :: MonadBackend im =>
           Identifier -> Value -> Value -> ObjectM o im ()
changed f old new = do
  this <- ask
  broadcast $ NamedEvent "changed" [old, new] $ FieldSrc this f

redraw :: MonadBackend im => ObjectM s im ()
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

compose :: MonadSindre im m => WidgetRef -> m im SpaceNeed
compose k = sindre $ operateW k $ \w -> do
  (need, w') <- onStateW (composeI k) w
  return (constrainNeed need $ widgetConstraints w', w')
draw :: MonadSindre im m =>
        WidgetRef -> Maybe Rectangle -> m im SpaceUse
draw k rect = sindre $ operateW k $ \w -> do
  let rect' = fromMaybe (widgetDimensions w) rect
  (use, w') <- onStateW (drawI rect' k) w
  return (use, w' { widgetDimensions = rect' })

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
    deriving (Functor, Monad, MonadFail, Applicative, MonadReader (ExecutionEnv m), MonadCont)

execute :: MonadBackend m => Execution m Value -> Sindre m Value
execute m = runReaderT m' env
    where env = ExecutionEnv {
                  execReturn = const $ fail "Nowhere to return to"
                , execNext   = const $ fail "Nowhere to go next"
                , execBreak  = const $ fail "Not in a loop"
                , execCont   = const $ fail "Not in a loop"
               }
          Execution m' = returnHere m

execute_ :: MonadBackend m => Execution m a -> Sindre m ()
execute_ m = void $ execute (m *> return (Number 0))

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

operateW :: MonadBackend im => WidgetRef ->
            (forall s . Widget s im -> Sindre im (a, Widget s im))
         -> Sindre im a
operateW (r,_,_) f = do
  objs <- gets objects
  (v, s') <- case objs!r of
               WidgetSlot s -> do (v, s') <- f s
                                  return (v, WidgetSlot s')
               _            -> fail "Expected widget"
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v

operateO :: MonadBackend im => ObjectRef ->
            (forall s . Object s im -> Sindre im (a, Object s im)) -> Sindre im a
operateO (r,_,_) f = do
  objs <- gets objects
  (v, s') <- case objs!r of
               WidgetSlot s -> do (v, s') <- f $ widgetObject s
                                  return (v, WidgetSlot s { widgetObject = s' })
               ObjectSlot s -> do (v, s') <- f s
                                  return (v, ObjectSlot s')
  modify $ \s -> s { objects = objects s // [(r, s')] }
  return v

onState :: (Object s im -> Sindre im (a, s)) -> Object s im -> Sindre im (a, Object s im)
onState f s = do (v, s') <- f s
                 return (v, s { objectState = s' })

onStateW :: (Widget s im -> Sindre im (a, s)) -> Widget s im -> Sindre im (a, Widget s im)
onStateW f s = do (v, os) <- f s
                  return (v, s { widgetObject = (widgetObject s)
                                                { objectState = os }})

callMethodByRef :: MonadBackend im => ObjectRef -> Identifier -> [Value] -> Execution im Value
callMethodByRef k m vs = sindre $ operateO k $ onState $ callMethodI m vs k
setFieldByRef :: MonadBackend im => ObjectRef -> Identifier -> Value -> Execution im Value
setFieldByRef k f v = sindre $ operateO k $ \s -> do
  (old, s') <- onState (getFieldI f k) s
  (new, s'') <- onState (setFieldI f v k) s'
  ((), os) <- runObjectM (changed f old new) k $ objectState s''
  return (new, s'' { objectState = os })
getFieldByRef :: MonadBackend im => ObjectRef -> Identifier -> Execution im Value
getFieldByRef k f = sindre $ operateO k $ onState $ getFieldI f k
recvEventByRef :: MonadBackend im => WidgetRef -> Event -> Execution im ()
recvEventByRef k ev = sindre $ operateO k $ onState $ recvEventI ev k

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

instance Mold a => Mold (Maybe a) where
  mold = liftM Just . mold
  unmold = maybe falsity unmold

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
