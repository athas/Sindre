{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.X11
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- X11 substrate for Sindre.
--
-----------------------------------------------------------------------------

module Sindre.X11( SindreX11M
                 , SindreX11Conf(..)
                 , runSindreX11
                 , sindreX11
                 , mkDial
                 , mkLabel
                 , mkTextField
                 , mkOutStream
                 , mkInStream
                 , mkList
                 )
    where

import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime
import Sindre.Util

import Graphics.X11.Xlib hiding ( refreshKeyboardMapping
                                , Rectangle )
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras hiding (Event, getEvent)
import qualified Graphics.X11.Xlib.Extras as X
import Graphics.X11.Xinerama
import Graphics.X11.Xshape
import Graphics.X11.Xim

import System.Exit
import System.IO
import System.Posix.Types
import System.Locale.SetLocale(setLocale, Category(..))

import Control.Concurrent
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Char(isPrint)
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

setLocaleAndCheck :: IO ()
setLocaleAndCheck = do
	ret <- setLocale LC_ALL Nothing
	case ret of
		Nothing -> putStrLn "Can't set locale." >> exitFailure
		_	-> return ()

supportsLocaleAndCheck :: IO ()
supportsLocaleAndCheck = do
	sl <- supportsLocale
	unless sl $ putStrLn "Current locale is not supported" >> exitFailure

fromXRect :: X.Rectangle -> Rectangle
fromXRect r =
    Rectangle { rectCorner = (fi $ rect_x r, fi $ rect_y r)
              , rectWidth = fi $ rect_width r
              , rectHeight = fi $ rect_height r }

type EventThunk = Sindre SindreX11M (Maybe (EventSource, Event))

data SindreX11Conf = SindreX11Conf {
      sindreDisplay    :: Display
    , sindreScreen     :: Screen
    , sindreRoot       :: Window
    , sindreScreenSize :: Rectangle
    , sindreFont       :: FontStruct
    , sindreIM         :: XIM
    , sindreIC         :: XIC
    , sindreXlock      :: Xlock
    , sindreEvtVar     :: MVar EventThunk
    }

newtype SindreX11M a = SindreX11M (ReaderT SindreX11Conf IO a)
  deriving (Functor, Monad, MonadIO, MonadReader SindreX11Conf, Applicative)

runSindreX11 :: SindreX11M a -> SindreX11Conf -> IO a
runSindreX11 (SindreX11M m) = runReaderT m

asOrient :: String -> Sindre m (Align, Align)
asOrient "top"  = return (AlignCenter, AlignNeg)
asOrient "mid"  = return (AlignCenter, AlignCenter)
asOrient "bot"  = return (AlignCenter, AlignPos)
asOrient orient = fail $ "Unknown orientation: '"++orient++"'"

instance MonadSubstrate SindreX11M where
  type SubEvent SindreX11M = (KeySym, String, X.Event)
  type InitVal SindreX11M = Window
  
  fullRedraw (orient, rootwr) = do
    orient' <- maybe (return (AlignCenter, AlignCenter)) asOrient orient
    screen <- subst $ asks sindreScreenSize
    root <- subst $ asks sindreRoot
    dpy  <- subst $ asks sindreDisplay
    reqs <- compose rootwr screen
    usage <- draw rootwr $ adjustRect orient' screen $ fitRect screen reqs
    subst $ io $ do
      pm <- createPixmap dpy root (fi $ rectWidth screen) (fi $ rectHeight screen) 1
      maskgc <- createGC dpy pm
      setForeground dpy maskgc 0
      unmaskgc <- createGC dpy pm
      setForeground dpy unmaskgc 1
      fillRectangle dpy pm maskgc 0 0 (fi $ rectWidth screen) (fi $ rectHeight screen)
      forM_ usage $ \rect ->
        fillRectangle dpy pm unmaskgc
         (fi $ fst $ rectCorner rect)
         (fi $ snd $ rectCorner rect)
         (fi $ rectWidth rect)
         (fi $ rectHeight rect)
      io $ xshapeCombineMask dpy root shapeBounding 0 0 pm shapeSet
      freeGC dpy maskgc
      sync dpy False
    return ()
  
  getSubEvent = do
    subst unlockX
    evvar <- subst $ asks sindreEvtVar
    evm <- io $ takeMVar evvar
    ev  <- evm
    subst lockX
    maybe getSubEvent return ev
  
  printVal s = io $ putStr s *> hFlush stdout

getModifiers :: KeyMask -> S.Set KeyModifier
getModifiers m = foldl add S.empty modifiers
    where add s (x, mods) | x .&. m /= 0 = S.insert mods s
                          | otherwise    = s
          modifiers = [ (controlMask, Control)
                      , (mod1Mask, Meta)]

mkWindow :: Window -> Position
         -> Position -> Dimension -> Dimension -> SindreX11M Window
mkWindow rw x y w h = do
  dpy <- asks sindreDisplay
  s   <- asks sindreScreen
  let visual   = defaultVisualOfScreen s
      attrmask = cWBackPixel
      white    = whitePixelOfScreen s
  io $ allocaSetWindowAttributes $ \attrs -> do
    set_background_pixel attrs white
    win <- createWindow dpy rw x y w h 0 copyFromParent
                 inputOutput visual attrmask attrs
    sync dpy False
    return win
                 
setupDisplay :: String -> IO Display
setupDisplay dstr =
  openDisplay dstr `Prelude.catch` \_ ->
    error $ "Cannot open display \"" ++ dstr ++ "\"."

grabInput :: Display -> Window -> IO GrabStatus
grabInput dpy win = do
  grabButton dpy button1 anyModifier win True buttonReleaseMask grabModeAsync grabModeAsync none none
  grab (1000 :: Int)
  where grab 0 = return alreadyGrabbed
        grab n = do status <- grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
                    if status /= grabSuccess
                      then threadDelay 1000 >> grab (n-1)
                      else return status

findRectangle :: Display -> Window -> IO X.Rectangle
findRectangle dpy rootw = do
  (_, _, _, x, y, _, _, _) <- queryPointer dpy rootw
  let hasPointer rect = fi x >= rect_x rect &&
                        fi (rect_width rect) + rect_x rect > fi x &&
                        fi y >= rect_y rect &&
                        fi (rect_height rect) + rect_y rect > fi y
  fromJust <$> find hasPointer <$> getScreenInfo dpy

mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow dpy s rw x y w h = do
  let visual   = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
      black    = blackPixelOfScreen s
      white    = whitePixelOfScreen s
  io $ allocaSetWindowAttributes $ \attrs -> do
    set_override_redirect attrs True
    set_background_pixel attrs white
    set_border_pixel attrs black
    createWindow dpy rw x y w h 0 copyFromParent
                 inputOutput visual attrmask attrs

type Xlock = MVar ()

lockXlock :: MonadIO m => Xlock -> m ()
lockXlock xlock = io $ takeMVar xlock
lockX :: SindreX11M ()
lockX = do xlock <- asks sindreXlock
           io $ takeMVar xlock

unlockXlock :: MonadIO m => Xlock -> m ()
unlockXlock xlock = io $ putMVar xlock ()
unlockX :: SindreX11M ()
unlockX = do xlock <- asks sindreXlock
             io $ putMVar xlock ()

getX11Event :: Display -> XIC -> IO (KeySym, String, X.Event)
getX11Event dpy ic = do
  (string,keysym,event) <-
    allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- X.getEvent e
      (ks,s) <- if ev_event_type ev == keyPress
                then utf8LookupString ic e
                else return (Nothing, Nothing)
      return (ks,s,ev)
  return ( fromMaybe xK_VoidSymbol keysym
         , fromMaybe "" string
         , event)

processX11Event :: (KeySym, String, X.Event) -> EventThunk
processX11Event (ks, s, KeyEvent {ev_event_type = t, ev_state = m })
    | t == keyPress = do
      let v = (SubstrSrc,) <$>
              (KeyPress . (getModifiers m,)) <$>
              case s of
                _ | s `elem` ["\127", "\8", "\13", "", "\27"] ->
                     Just $ CtrlKey $ keysymToString ks
                [c] | not (isPrint c) ->
                     Just $ CharKey $ head $ keysymToString ks
                (c:_)  -> Just $ CharKey c
                _ -> Nothing
      return v
processX11Event (_, _, ExposeEvent { ev_count = 0 }) = do
  fullRedraw =<< gets guiRoot
  return Nothing
processX11Event  _ = return Nothing

eventReader :: Display -> XIC -> MVar EventThunk ->
               Xlock -> IO ()
eventReader dpy ic evvar xlock = forever $ do
    lockXlock xlock
    cnt <- eventsQueued dpy queuedAfterFlush
    when (cnt == 0) $ do
      -- The following two lines have a race condition.
      unlockXlock xlock
      threadWaitRead $ Fd $ connectionNumber dpy
      lockXlock xlock
    xev <- getX11Event dpy ic
    unlockXlock xlock
    putMVar evvar $ processX11Event xev

sindreX11Cfg :: String -> IO SindreX11Conf
sindreX11Cfg dstr = do
  setLocaleAndCheck
  supportsLocaleAndCheck
  _ <- setLocaleModifiers ""
  dpy <- setupDisplay dstr
  let scr = defaultScreenOfDisplay dpy
  rect <- findRectangle dpy (rootWindowOfScreen scr)
  win <- mkUnmanagedWindow dpy scr (rootWindowOfScreen scr)
         (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)
  fstruct <- loadQueryFont dpy "fixed"
  _ <- mapRaised dpy win
  status <- grabInput dpy win
  unless (status == grabSuccess) $
    error "Could not establish keyboard grab"
  im <- openIM dpy Nothing Nothing Nothing
  ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
  evvar <- newEmptyMVar
  xlock <- newMVar ()
  _ <- forkIO $ eventReader dpy ic evvar xlock
  return SindreX11Conf { sindreDisplay = dpy
                       , sindreScreen = scr
                       , sindreRoot = win 
                       , sindreScreenSize = fromXRect rect
                       , sindreFont = fstruct
                       , sindreIM = im
                       , sindreIC = ic
                       , sindreEvtVar = evvar
                       , sindreXlock = xlock }

sindreX11 :: Program -> ClassMap SindreX11M -> ObjectMap SindreX11M 
          -> String -> ( [SindreOption]
                       , Arguments -> IO ExitCode)
sindreX11 prog cm om dstr =
  case compileSindre prog cm om  of
    Left s -> error s
    Right (opts, prog') ->
      let m args = do
            cfg <- sindreX11Cfg dstr
            runSindreX11 (lockX >> prog' args (sindreRoot cfg)) cfg
      in (opts, m)

data OutStream = OutStream Handle

methWrite :: String -> ObjectM OutStream SindreX11M ()
methWrite s = do OutStream h <- get 
                 io $ hPutStr h s
                 io $ hFlush h

instance Object SindreX11M OutStream where
  callMethodI "write" = method methWrite
  callMethodI _ = error "Unknown method"

mkOutStream :: Handle -> ObjectRef -> SindreX11M (NewObject SindreX11M)
mkOutStream = const . return . NewObject . OutStream

data InStream = InStream Handle

instance (MonadIO m, MonadSubstrate m) => Object m InStream where

mkInStream :: Handle -> ObjectRef -> SindreX11M (NewObject SindreX11M)
mkInStream h r = do evvar <- asks sindreEvtVar
                    _ <- io $ forkIO $ getHandleEvent evvar
                    return $ NewObject $ InStream h
    where getHandleEvent :: MVar EventThunk -> IO ()
          getHandleEvent evvar = forever loop `catch`
                      (\_ -> putEv $ NamedEvent "eof" [])
              where loop = do line <- hGetLine h
                              putEv $ NamedEvent "line" [StringV line]
                    putEv ev = putMVar evvar $
                                 return $ Just (ObjectSrc r, ev)

drawing :: (a -> Window) 
        -> (Rectangle -> Display -> GC -> WidgetM a SindreX11M SpaceUse)
        -> Rectangle -> WidgetM a SindreX11M SpaceUse
drawing wf m r = do
  dpy <- sindre $ subst $ asks sindreDisplay
  scr <- sindre $ subst $ asks sindreScreen
  win <- gets wf
  gc <- io $ do
    moveResizeWindow dpy win
      (fi $ fst $ rectCorner r) (fi $ snd $ rectCorner r)
      (fi $ max 1 $ rectWidth r) (fi $ max 1 $ rectHeight r)
    gc <- createGC dpy win
    setForeground dpy gc $ whitePixelOfScreen scr
    setBackground dpy gc $ blackPixelOfScreen scr
    fillRectangle dpy win gc
      0 0 (fi $ rectWidth r) (fi $ rectHeight r)
    setForeground dpy gc $ blackPixelOfScreen scr
    setBackground dpy gc $ whitePixelOfScreen scr
    return gc
  m r dpy gc <* io (freeGC dpy gc)  
                
data Dial = Dial { dialMax :: Integer
                 , dialVal :: Integer
                 , dialWin :: Window
                 }

instance Object SindreX11M Dial where
    fieldSetI "value" v = do
      modify $ \s -> s { dialVal = clamp 0 (fromJust $ mold v) (dialMax s) }
      IntegerV <$> gets dialVal
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "value" = IntegerV <$> gets dialVal
    fieldGetI _       = return $ IntegerV 0

instance Widget SindreX11M Dial where
    composeI _ = return (Unlimited, Unlimited)
    drawI r = do
      dpy <- sindre $ subst $ asks sindreDisplay
      scr <- sindre $ subst $ asks sindreScreen
      win <- gets dialWin
      val <- gets dialVal
      maxval <- gets dialMax
      let unitAng = 2*pi / fi maxval
          angle :: Double
          angle   = (-unitAng) * fi val
      io $ do
        moveResizeWindow dpy win
          (fi $ fst $ rectCorner r) (fi $ snd $ rectCorner r)
          (fi $ max 1 $ rectWidth r) (fi $ max 1 $ rectHeight r)
        gc <- createGC dpy win
        setForeground dpy gc $ whitePixelOfScreen scr
        setBackground dpy gc $ blackPixelOfScreen scr
        fillRectangle dpy win gc
                  0 0 (fi $ rectWidth r) (fi $ rectHeight r)
        setForeground dpy gc $ blackPixelOfScreen scr
        setBackground dpy gc $ whitePixelOfScreen scr
        drawArc dpy win gc (fi cornerX) (fi cornerY) 
          (fi dim) (fi dim) 0 (360*64)
        fillArc dpy win gc (fi cornerX) (fi cornerY) 
          (fi dim) (fi dim) (90*64) (round $ angle * (180/pi) * 64)
        drawRectangle dpy win gc
                  (fi cornerX) (fi cornerY)
                  (fi dim) (fi dim)
        freeGC dpy gc
      return [r]
      where dim = min (rectWidth r) (rectHeight r) - 1
            cornerX = (rectWidth r - dim) `div` 2
            cornerY = (rectHeight r - dim) `div` 2
    
    recvEventI (KeyPress (_, CharKey 'n')) = do
      v <- gets dialVal
      modify $ \s -> s { dialVal = clamp 0 (v+1) (dialMax s) }
      changed "value" (IntegerV v) (IntegerV $ v+1)
    recvEventI (KeyPress (_, CharKey 'p')) = do
      v <- gets dialVal
      modify $ \s -> s { dialVal = clamp 0 (v-1) (dialMax s) }
      changed "value" (IntegerV v) (IntegerV $ v-1)
    recvEventI _ = return ()

mkDial' :: Window -> Integer -> Construction SindreX11M
mkDial' w maxv = do
  dpy <- asks sindreDisplay
  win <- mkWindow w 1 1 1 1
  io $ mapWindow dpy win
  io $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
  construct (Dial maxv 0 win, win)

mkDial :: Constructor SindreX11M
mkDial w (M.toList . M.map mold -> [("max", Just maxv)]) [] =
  mkDial' w maxv
mkDial w m [] | m == M.empty = mkDial' w 12
mkDial _ _ [] = error "Dials take at most one integer argument"
mkDial _ m _ | m /= M.empty = error "Dials do not have children"
mkDial _ _ _ = error "Invalid initial argument"

data Label = Label { labelText :: String 
                   , labelWin :: Window
                   , labelAlign :: Align
                   }

instance Object SindreX11M Label where
    fieldSetI "label" v = do
      modify $ \s -> s { labelText = fromMaybe "" $ mold v }
      StringV <$> gets labelText
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "label" = StringV <$> gets labelText
    fieldGetI _       = return $ IntegerV 0

instance Widget SindreX11M Label where
    composeI _ = do
      fstruct <- sindre $ subst $ asks sindreFont
      text <- gets labelText
      let (_, a, d, _) = textExtents fstruct text
          w = textWidth fstruct text
          h = a+d
      return (Max $ fi w, Min $ fi h + padding * 2)
        where padding = 2
    drawI = drawing labelWin $ \r dpy gc -> do
      fstruct <- sindre $ subst $ asks sindreFont
      label <- gets labelText
      win <- gets labelWin
      just <- gets labelAlign
      io $ do
        let (_, a, d, _) = textExtents fstruct label
            w = textWidth fstruct label
            h = a+d
        drawString dpy win gc
                   (align just 0 w (fi $ rectWidth r))
                   (a + align AlignCenter 0 h (fi $ rectHeight r))
                   label
      return [r]

mkLabel' :: Window -> String -> Construction SindreX11M
mkLabel' w label = do
  dpy <- asks sindreDisplay
  win <- mkWindow w 1 1 1 1
  io $ mapWindow dpy win
  io $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
  construct (Label label win AlignCenter, win)

mkLabel :: Constructor SindreX11M
mkLabel w (M.toList . M.map mold -> [("label", Just label)]) [] =
  mkLabel' w label
mkLabel w m [] | m == M.empty = mkLabel' w ""
mkLabel _ _ [] = error "Labels take at most one string argument"
mkLabel _ m _ | m /= M.empty = error "Labels do not have children"
mkLabel _ _ _ = error "Invalid initial argument"

                
data TextField = TextField { fieldText :: String
                           , fieldPoint :: Int
                           , fieldWin :: Window 
                           , fieldAlign :: Align}

instance Object SindreX11M TextField where
    fieldSetI "value" v = do
      modify $ \s -> s { fieldText = fromJust $ mold v }
      StringV <$> gets fieldText
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "value" = StringV <$> gets fieldText
    fieldGetI _       = return $ IntegerV 0

instance Widget SindreX11M TextField where
    composeI _ = do
      fstruct <- sindre $ subst $ asks sindreFont
      text <- gets fieldText
      let (_, a, d, _) = textExtents fstruct text
          w = textWidth fstruct text
          h = a+d
      return (Max $ fi w + 3, Max $ fi h + padding * 2)
        where padding = 2
    drawI = drawing fieldWin $ \r dpy gc -> do
      text <- gets fieldText
      win <- gets fieldWin
      just <- gets fieldAlign
      p <- gets fieldPoint
      fstruct <- sindre $ subst $ asks sindreFont
      io $ do
        let (_, a, d, _) = textExtents fstruct text
            w = textWidth fstruct text
            h = a+d
            w' = textWidth fstruct $ take p text
            x = align just 0 w (fi (rectWidth r) - padding*2) + padding
            y = align AlignCenter 0 h (fi (rectHeight r) - padding*2) + padding
        drawString dpy win gc x (a+y) text
        drawLine dpy win gc (x+w') (y-padding) (x+w') (y+padding+h)
      return [r]
        where padding = 2
    
    recvEventI (KeyPress (S.toList -> [], CharKey c)) = do
      v <- gets fieldText
      p <- gets fieldPoint
      let v' = take p v ++ [c] ++ drop p v
      modify $ \s -> s { fieldText = v', fieldPoint = p+1 }
      changed "value" (StringV v) (StringV v')
    recvEventI (KeyPress (S.toList -> [], CtrlKey "BackSpace")) = do
      p <- gets fieldPoint
      when (p > 0) $ do
        v <- gets fieldText
        let v' = take (p-1) v ++ drop p v
        modify $ \s -> s { fieldText = v', fieldPoint = p-1 }
        changed "value" (StringV v) (StringV $ v')
    recvEventI (KeyPress (S.toList -> [], CtrlKey "Right")) = do
      movePoint 1
    recvEventI (KeyPress (S.toList -> [], CtrlKey "Left")) = do
      movePoint (-1)
    recvEventI (KeyPress (S.toList -> [Control], CharKey 'w')) = do
      v <- gets fieldText
      modify $ \s -> s { fieldText = "" }
      changed "value" (StringV v) (StringV "")
    recvEventI _ = return ()

movePoint :: Int -> WidgetM TextField m ()
movePoint d = do ep <- gets fieldPoint
                 n <- length <$> gets fieldText
                 modify $ \s -> s { fieldPoint = clamp 0 (ep+d) n }

mkTextField' :: Window -> String -> Construction SindreX11M
mkTextField' w v = do
  dpy <- asks sindreDisplay
  win <- mkWindow w 1 1 1 1
  io $ mapWindow dpy win
  io $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
  construct (TextField v 0 win AlignNeg, win)

mkTextField :: Constructor SindreX11M
mkTextField w (M.toList . M.map mold -> [("value", Just value)]) [] =
  mkTextField' w value
mkTextField w m [] | m == M.empty = mkTextField' w ""
mkTextField _ _ [] = error "TextFields take at most one string argument"
mkTextField _ m _ | m /= M.empty = error "TextFields do not have children"
mkTextField _ _ _ = error "Invalid initial argument"

data List = List { listElems :: [String] 
                 , listWin :: Window 
                 , listFilter :: String
                 , listFiltered :: [String] 
                 , listSel :: Int }

methInsert :: String -> ObjectM List SindreX11M ()
methInsert v =
  modify $ \s -> s { listElems = v `insert` listElems s 
                   , listFiltered = if listFilter s `isInfixOf` v
                                    then v `insert` listFiltered s
                                    else listFiltered s }

methFilter :: String -> ObjectM List SindreX11M ()
methFilter f = do
  v' <- filter (isInfixOf f) <$> gets listElems
  modify $ \s -> s { listFilter = f
                   , listFiltered = v'
                   , listSel = clamp 0 (listSel s) $ length v' }

methNext :: ObjectM List SindreX11M ()
methPrev :: ObjectM List SindreX11M ()
(methNext, methPrev) = (move 1, move (-1))
    where move d = modify $ \s ->
            s { listSel = clamp 0 (listSel s + d)
                          $ length (listFiltered s) - 1 }

instance Object SindreX11M List where
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "selected" = do
      elems <- gets listFiltered
      sel <- gets listSel
      case drop sel elems of
        []    -> return $ IntegerV 0
        (e:_) -> return $ StringV e
    fieldGetI _ = return $ IntegerV 0
    callMethodI "insert" = method methInsert
    callMethodI "filter" = method methFilter
    callMethodI "next" = method methNext
    callMethodI "prev" = method methPrev
    callMethodI m = fail $ "Unknown method '" ++ m ++ "'"

instance Widget SindreX11M List where
    composeI _ = do
      fstruct <- sindre $ subst $ asks sindreFont
      let (_, a, d, _) = textExtents fstruct ""
          h = a+d
      return (Unlimited, Min $ fi h + padding * 2)
        where padding = 2
    drawI = drawing listWin $ \r dpy gc -> do
      fstruct <- sindre $ subst $ asks sindreFont
      elems <- gets listFiltered
      win <- gets listWin
      sel <- gets listSel
      io $ do
        let printElems [] _ _ = return ()
            printElems ((i, e):es) x left = do
              let (_, a, d, _) = textExtents fstruct e
                  w = textWidth fstruct e
                  h = a+d
                  y = align AlignCenter 0 h
                      (fi (rectHeight r) - ypadding*2) + ypadding
              when (left >= w) $ do
                when (i == sel) $ do
                  drawRectangle dpy win gc x y (fi w) (fi h)
                drawString dpy win gc x (y+a) e
                printElems es (x + w + spacing) $ left - w - spacing
        printElems (zip [0..] elems) 0 $ fi $ rectWidth r
      return [r]
        where ypadding = 2
              spacing = 10

mkList' :: Window -> Construction SindreX11M
mkList' w = do
  dpy <- asks sindreDisplay
  win <- mkWindow w 1 1 1 1
  io $ mapWindow dpy win
  io $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
  construct (List [] win "" [] 0, win)

mkList :: Constructor SindreX11M
mkList w m [] | m == M.empty = mkList' w
mkList _ m _ | m /= M.empty = error "Lists do not have children"
mkList _ _ [] = error "Lists do not take arguments"
mkList _ _ _ = error "Invalid initial argument"
