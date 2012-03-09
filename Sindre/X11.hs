{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.X11
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  unportable
--
-- X11 backend for Sindre.  For internationalised keyboard input to
-- work, make sure the locale is correctly set.
--
-----------------------------------------------------------------------------
module Sindre.X11( SindreX11M
                 , SindreX11Conf(sindreDisplay, sindreXftMgr)
                 , sindreX11override
                 , sindreX11dock
                 , sindreX11
                 , xopt
                 , VisualOpts(..)
                 , visualOpts
                 , allocColor
                 , drawing
                 , drawing'
                 , X11Field
                 , Drawer(..)
                 , setFgColor
                 , setBgColor
                 , textExtents
                 , drawText
                 , mkDial
                 , mkLabel
                 , mkBlank
                 , mkTextField
                 , mkInStream
                 , mkHList
                 , mkVList
                 )
    where

import Sindre.Sindre
import Sindre.Compiler (badValue, moldM, Constructor, ConstructorM, Param,
                        param, noParam, paramM)
import Sindre.Formatting
import Sindre.KeyVal ((<$?>), (<||>))
import qualified Sindre.KeyVal as KV
import Sindre.Lib
import Sindre.Runtime
import Sindre.Util
import Sindre.Widgets

import Graphics.X11.Xlib hiding ( refreshKeyboardMapping
                                , Rectangle
                                , badValue
                                , resourceManagerString
                                , textWidth
                                , allocColor
                                , textExtents )
import Graphics.X11.XRM
import qualified Graphics.X11.Xft as Xft
import Graphics.X11.Xim
import Graphics.X11.Xinerama
import Graphics.X11.Xlib.Extras hiding (Event, getEvent)
import Graphics.X11.Xshape
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X

import System.Environment
import System.Exit
import System.IO
import System.Posix.Types

import Control.Arrow(first,second)
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Char hiding (Control)
import Data.Maybe
import Data.List
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Monoid
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E

import Prelude hiding (catch)

fromXRect :: X.Rectangle -> Rectangle
fromXRect r =
    Rectangle { rectX = fi $ rect_x r
              , rectY = fi $ rect_y r
              , rectWidth = fi $ rect_width r
              , rectHeight = fi $ rect_height r }

type EventThunk = Sindre SindreX11M (Maybe Event)

data Surface = Surface {
    surfaceBounds   :: Rectangle
  , surfaceShape    :: Drawable
  , surfaceMaskGC   :: GC
  , surfaceUnmaskGC :: GC
  , surfaceCanvas   :: Drawable
  , surfaceWindow   :: Window
  , surfaceWindowGC :: GC
  , surfaceScreen   :: Screen
  , surfaceXftDraw  :: Xft.Draw
  }

newSurfaceWithGC :: Display -> Xft.XftMgr -> Screen -> Window -> GC -> Rectangle -> IO Surface
newSurfaceWithGC dpy mgr scr win wingc r = do
  pm       <- createPixmap dpy win (fi $ rectWidth r) (fi $ rectHeight r) 1
  maskgc   <- createGC dpy pm
  setForeground dpy maskgc 0
  unmaskgc <- createGC dpy pm
  setForeground dpy unmaskgc 1
  canvas   <- createPixmap dpy win (fi $ rectWidth r) (fi $ rectHeight r) $
              defaultDepthOfScreen scr
  drw      <- Xft.openDraw mgr canvas (defaultVisualOfScreen scr)
              (defaultColormap dpy $ defaultScreen dpy)
  drw'     <- maybe (fail "Could not allocate Xft drawable") return drw
  return $ Surface r { rectX = 0, rectY = 0 } pm maskgc
                   unmaskgc canvas win wingc scr drw'

newSurface :: Display -> Xft.XftMgr -> Screen -> Window -> Rectangle -> IO Surface
newSurface dpy mgr scr win r = do wingc <- createGC dpy win
                                  setGraphicsExposures dpy wingc False
                                  newSurfaceWithGC dpy mgr scr win wingc r

resizeSurface :: Display -> Xft.XftMgr -> Surface -> Rectangle -> IO Surface
resizeSurface dpy mgr s r = do
  mapM_ (freeGC dpy) [surfaceMaskGC s, surfaceUnmaskGC s]
  mapM_ (freePixmap dpy) [surfaceShape s, surfaceCanvas s]
  newSurfaceWithGC dpy mgr (surfaceScreen s) (surfaceWindow s) (surfaceWindowGC s) r

setShape :: Display -> Surface -> [Rectangle] -> IO ()
setShape dpy s rects = do
  fillRectangle dpy (surfaceShape s) (surfaceMaskGC s) 0 0
   (fi $ rectWidth $ surfaceBounds s) (fi $ rectHeight $ surfaceBounds s)
  forM_ rects $ \rect ->
    fillRectangle dpy (surfaceShape s) (surfaceUnmaskGC s)
      (fi $ rectX rect)
      (fi $ rectY rect)
      (fi $ rectWidth rect)
      (fi $ rectHeight rect)
  xshapeCombineMask dpy (surfaceWindow s) shapeBounding
    0 0 (surfaceShape s) shapeSet

copySurface :: Display -> Surface -> [Rectangle] -> IO ()
copySurface dpy s rects = do
  let Rectangle{..} = mconcat rects
  copyArea dpy (surfaceCanvas s) (surfaceWindow s) (surfaceWindowGC s)
    (fi rectX) (fi rectY) (fi rectWidth) (fi rectHeight) (fi rectX) (fi rectY)

-- | The read-only configuration of the X11 backend, created during
-- backend initialisation.
data SindreX11Conf = SindreX11Conf {
    sindreDisplay    :: Display
  -- ^ The display that we are connected to.
  , sindreVisualOpts :: VisualOpts
  -- ^ The default visual options (color, font, etc) used if no
  -- others are specified for a widget.
  , sindreRMDB       :: Maybe RMDatabase
  -- ^ The X11 resource database (Xdefaults/Xresources).
  , sindreXlock      :: Xlock
  -- ^ Synchronisation lock for Xlib access.
  , sindreEvtVar     :: MVar EventThunk
  -- ^ Channel through which events are sent by other threads to the
  -- Sindre command loop.
  , sindreReshape    :: [Rectangle] -> SindreX11M ()
  -- ^ Function to set the shape of the X11 window to the union of the
  -- given rectangles.
  , sindreXftMgr     :: Xft.XftMgr
  -- ^ Bookkeeping primitive for Xft font handling.
  }

-- | Sindre backend using Xlib.
newtype SindreX11M a = SindreX11M (ReaderT SindreX11Conf (StateT Surface IO) a)
  deriving ( Functor, Monad, MonadIO
           , MonadReader SindreX11Conf, MonadState Surface, Applicative)

runSindreX11 :: SindreX11M a -> SindreX11Conf -> Surface -> IO a
runSindreX11 (SindreX11M m) = evalStateT . runReaderT m

instance MonadBackend SindreX11M where
  type BackEvent SindreX11M = (KeySym, String, X.Event)
  type RootPosition SindreX11M = (Align, Align)

  redrawRoot = do
    SindreX11Conf{ sindreReshape=reshape } <- back ask
    sur <- back get
    (orient, rootwr) <- gets rootWidget
    reqs <- compose rootwr
    let winsize = surfaceBounds sur
        orient' = fromMaybe (AlignCenter, AlignCenter) orient
        rect = adjustRect orient' winsize $ fitRect winsize reqs
    usage <- draw rootwr $ Just rect
    back $ reshape usage
    redrawRegion usage

  redrawRegion rects = back $ do
    SindreX11Conf{ sindreDisplay=dpy } <- ask
    sur <- get
    io $ copySurface dpy sur rects >> sync dpy False
  
  waitForBackEvent = do
    back unlockX
    evvar <- back $ asks sindreEvtVar
    evm <- io $ takeMVar evvar
    ev  <- evm
    back lockX
    maybe waitForBackEvent return ev
  
  getBackEvent = do
    io yield
    back (io . tryTakeMVar =<< asks sindreEvtVar) >>=
         fromMaybe (return Nothing)

  printVal s = io $ putStr s *> hFlush stdout

textExtents :: Xft.Font -> String -> SindreX11M (Int, Int)
textExtents font s = do dpy <- asks sindreDisplay
                        w  <- io $ Xft.textWidth dpy font s
                        return (w, Xft.height font)

drawText :: (Integral x, Integral y, Integral z) => Xft.Color -> Xft.Font
         -> x -> y -> z -> String -> SindreX11M ()
drawText col font x y h str = do
  drw <- gets surfaceXftDraw
  (_,h') <- textExtents font str
  let y' = Xft.ascent font + align AlignCenter (fi y) h' (fi y+fi h)
  io $ Xft.drawString drw col font x y' str

drawFmt :: Drawer -> Rectangle -> FormatString -> SindreX11M ()
drawFmt d Rectangle{..} fs = do
  mgr <- asks sindreXftMgr
  drw <- gets surfaceXftDraw
  d'' <- case startBg fs of Nothing -> return d
                            Just col -> io . setBgColor d =<< allocColor mgr col
  io $ Xft.drawRect drw (drawerBgColor d'') rectX rectY (padding::Int) rectHeight
  let proc (x,d') (Fg fg) = do col <- allocColor mgr fg
                               return (x, d' { drawerFgColor = col })
      proc (x,d') (DefFg) = return (x, d' { drawerFgColor = drawerFgColor d })
      proc (x,d') (Bg bg) = do col <- allocColor mgr bg
                               return (x, d' { drawerBgColor = col })
      proc (x,d') (DefBg) = return (x, d' { drawerBgColor = drawerBgColor d })
      proc (x,d') (Text t) = do
        let s = T.unpack t
        (w,_) <- textExtents (drawerFont d') s
        io $ Xft.drawRect drw (drawerBgColor d') x rectY w rectHeight
        drawText (drawerFgColor d') (drawerFont d')
          x (rectY + padding) (rectHeight - padding) s
        return (x+w, d')
  (endx, d') <- foldM proc (fi rectX + padding, d'') fs
  io $ Xft.drawRect drw (drawerBgColor d') endx rectY
         (max 0 $ fi rectX + fi rectWidth - endx) rectHeight

fmtSize :: Xft.Font -> FormatString -> SindreX11M Rectangle
fmtSize font s = do
  (w,h) <- textExtents font s'
  return $ Rectangle 0 0 (fi w + 2 * padding) (fi h + 2 * padding)
  where s' = T.unpack $ textContents s

getModifiers :: KeyMask -> S.Set KeyModifier
getModifiers m = foldl add S.empty modifiers
    where add s (x, mods) | x .&. m /= 0 = S.insert mods s
                          | otherwise    = s
          modifiers = [ (controlMask, Control)
                      , (mod1Mask, Meta)
                      , (shiftMask, Shift) ]

setupDisplay :: String -> IO Display
setupDisplay dstr =
  openDisplay dstr `catch` \e ->
    error $ "Cannot open display \"" ++ dstr ++ "\": "
            ++ show (e :: IOException)

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
  (win, _) <- getInputFocus dpy
  (x,y) <- if rootw == win then windowWithPointer
                           else windowWithFocus =<< getWindowAttributes dpy win
  let contains rect = fi x >= rect_x rect &&
                      fi (rect_width rect) + rect_x rect > fi x &&
                      fi y >= rect_y rect &&
                      fi (rect_height rect) + rect_y rect > fi y
  fromJust <$> find contains <$> getScreenInfo dpy
  where windowWithPointer = do
          (_, _, _, x, y, _, _, _) <- queryPointer dpy rootw
          return (x,y)
        windowWithFocus attr = do
          (_, x, y, _) <- translateCoordinates dpy rootw rootw
                          (fi $ wa_x attr) (fi $ wa_y attr)
          return (fi x,fi y)

mkWindow :: Display -> Screen -> Window -> Bool -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkWindow dpy s rw o x y w h = do
  let visual   = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
      black    = blackPixelOfScreen s
      white    = whitePixelOfScreen s
  io $ allocaSetWindowAttributes $ \attrs -> do
    set_override_redirect attrs o
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

getX11Event :: Display -> Window -> XIC -> IO (KeySym, String, X.Event)
getX11Event dpy win ic = do
  (str,keysym,event) <-
    allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- X.getEvent e
      (ks,s) <- ifM ((ev_event_type ev /= keyPress ||) <$>
                     filterEvent e win)
                    (return (Nothing, Nothing))
                    (utf8LookupString ic e)
      return (ks,s,ev)
  return ( fromMaybe xK_VoidSymbol keysym
         , fromMaybe "" str
         , event)

processX11Event :: (KeySym, String, X.Event) -> EventThunk
processX11Event (ks, s, KeyEvent {ev_event_type = t, ev_state = m })
    | t == keyPress =
      return $ (KeyPress . mods) <$>
             case s of
               _ | s `elem` ["\127", "\8", "\13", "", "\27"] ->
                 Just $ CtrlKey $ keysymToString ks
               [c] | not (isPrint c) ->
                 case keysymToString ks of
                   [ks'] -> Just $ CharKey ks'
                   ks'   -> Just $ CtrlKey ks'
               [c]  -> Just $ CharKey c
               _ -> Nothing
      where mods (CharKey c) = (Shift `S.delete` getModifiers m, CharKey c)
            mods (CtrlKey c) = (getModifiers m, CtrlKey c)
processX11Event (_, _, ExposeEvent { ev_x = x, ev_y = y
                                   , ev_width = w, ev_height = h }) =
  redrawRegion [Rectangle (fi x) (fi y) (fi w) (fi h)] >> return Nothing
processX11Event (_, _, ConfigureEvent { ev_window = win
                                      , ev_width = w, ev_height = h }) = do
  back $ do onsurface <- (==win) <$> gets surfaceWindow
            when onsurface $ do
              sur <- (pure resizeSurface
                             <*> asks sindreDisplay
                             <*> asks sindreXftMgr <*> get
                             <*> pure (Rectangle 0 0 (fi w) (fi h)))
              put =<< io sur
  redrawRoot >> return Nothing
processX11Event (_, _, AnyEvent { ev_event_type = t })
  | t == visibilityNotify = do back $ do
                                 dpy <- asks sindreDisplay
                                 win <- gets surfaceWindow
                                 io $ raiseWindow dpy win
                               redrawRoot
                               return Nothing
processX11Event _ = return Nothing

eventReader :: Display -> Window -> XIC -> MVar EventThunk ->
               Xlock -> IO ()
eventReader dpy win ic evvar xlock = forever $ do
    lockXlock xlock
    waitUntilEvent
    xev <- getX11Event dpy win ic
    unlockXlock xlock
    putMVar evvar $ processX11Event xev
      where waitUntilEvent = do
              cnt <- eventsQueued dpy queuedAfterFlush
              when (cnt == 0) $ do
                -- The following two lines have a race condition.
                unlockXlock xlock
                threadWaitRead $ Fd $ connectionNumber dpy
                lockXlock xlock
                waitUntilEvent

-- | Get the value for a named color if it exists
maybeAllocColor :: Xft.XftMgr -> String -> IO (Maybe Xft.Color)
maybeAllocColor mgr c = Xft.openColorName mgr vis colormap c
  where colormap = defaultColormap dpy $ defaultScreen dpy
        dpy      = Xft.mgrDisplay mgr
        vis      = defaultVisualOfScreen $ defaultScreenOfDisplay dpy

allocColor :: MonadIO m => Xft.XftMgr -> String -> m Xft.Color
allocColor dpy c = io (maybeAllocColor dpy c) >>=
                     maybe (fail $ "Unknown color '"++c++"'") return

sindreEventMask :: EventMask
sindreEventMask = exposureMask .|. structureNotifyMask

sindreX11Cfg :: String -> Bool -> IO (SindreX11Conf, Surface)
sindreX11Cfg dstr o = do
  sl <- supportsLocale
  unless sl $ putStrLn "Current locale is not supported" >> exitFailure
  _ <- setLocaleModifiers ""
  dpy <- setupDisplay dstr
  let scr = defaultScreenOfDisplay dpy
  xlock <- newMVar ()
  mgr <- Xft.newXftMgr dpy scr (lockXlock xlock) (unlockXlock xlock)
  rmInitialize
  s <- resourceManagerString dpy
  db <- case s of Nothing -> return Nothing
                  Just s' -> rmGetStringDatabase s'
  rect <- findRectangle dpy (rootWindowOfScreen scr)
  win <- mkWindow dpy scr (rootWindowOfScreen scr) o
         (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)
  surface <- newSurface dpy mgr scr win (fromXRect rect)
  setShape dpy surface []
  im <- openIM dpy Nothing Nothing Nothing
  ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
  evvar <- newEmptyMVar
  _ <- forkIO $ eventReader dpy win ic evvar xlock
  visopts <- defVisualOpts mgr
  return (SindreX11Conf
          { sindreDisplay = dpy
          , sindreVisualOpts = visopts
          , sindreRMDB = db
          , sindreEvtVar = evvar
          , sindreXlock = xlock
          , sindreReshape = reshape
          , sindreXftMgr = mgr }, surface)
    where reshape rs = do sur <- get
                          dpy <- asks sindreDisplay
                          io $ setShape dpy sur rs

-- | Options regarding visual appearance of widgets (colors and
-- fonts).
data VisualOpts = VisualOpts {
      foreground      :: Xft.Color
    , background      :: Xft.Color
    , focusForeground :: Xft.Color
    , focusBackground :: Xft.Color
    , font            :: Xft.Font
    }

defVisualOpts :: Xft.XftMgr -> IO VisualOpts
defVisualOpts mgr = do
  font   <- Xft.openFontName mgr "Monospace"
  case font of Just font' ->
                 pure VisualOpts <*> f fg <*> f bg <*> f ffg <*> f fbg
                        <*> pure font'
               Nothing    -> fail "Cannot open Monospace font"
  where (fg, bg, ffg, fbg) = ("black", "grey", "white", "blue")
        f = allocColor mgr

-- | Execute Sindre in the X11 backend, grabbing control of the entire
-- display and staying on top.
sindreX11override :: String -- ^ The display string (usually the value of the
                            -- environment variable @$DISPLAY@ or @:0@)
                  -> SindreX11M ExitCode
                  -- ^ The function returned by
                  -- 'Sindre.Compiler.compileSindre' after command line
                  -- options have been given
                  -> IO ExitCode
sindreX11override dstr start = do
  (cfg, sur) <- sindreX11Cfg dstr True
  _ <- io $ mapRaised (sindreDisplay cfg) (surfaceWindow sur)
  status <- grabInput (sindreDisplay cfg) (surfaceWindow sur)
  io $ selectInput (sindreDisplay cfg) (surfaceWindow sur) $
     sindreEventMask .|. visibilityChangeMask
  unless (status == grabSuccess) $
    error "Could not establish keyboard grab"
  runSindreX11 (lockX >> start) cfg sur <* Xft.freeXftMgr (sindreXftMgr cfg)

-- | Execute Sindre in the X11 backend as an ordinary client visible
-- to the window manager.
sindreX11 :: String -- ^ The display string (usually the value of the
                    -- environment variable @$DISPLAY@ or @:0@)
          -> SindreX11M ExitCode
          -- ^ The function returned by
          -- 'Sindre.Compiler.compileSindre' after command line
          -- options have been given
          -> IO ExitCode
sindreX11 dstr start = do
  (cfg, sur) <- sindreX11Cfg dstr False
  _ <- io $ mapRaised (sindreDisplay cfg) (surfaceWindow sur)
  selectInput (sindreDisplay cfg) (surfaceWindow sur) $
    keyPressMask .|. keyReleaseMask .|. sindreEventMask
  runSindreX11 (lockX >> start) cfg sur

-- | Execute Sindre in the X11 backend as a dock/statusbar.
sindreX11dock :: String -- ^ The display string (usually the value of the
                        -- environment variable @$DISPLAY@ or @:0@)
              -> SindreX11M ExitCode
              -- ^ The function returned by
              -- 'Sindre.Compiler.compileSindre' after command line
              -- options have been given
              -> IO ExitCode
sindreX11dock dstr start = do
  (cfg, sur) <- sindreX11Cfg dstr False
  let d = sindreDisplay cfg
      w = surfaceWindow sur
  a1 <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
  c1 <- internAtom d "CARDINAL"                 False
  a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
  c2 <- internAtom d "ATOM"                     False
  v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
  let reshape rs = do
        io $ changeProperty32 d w a1 c1 propModeReplace $ map fi $
          getStrutValues (mconcat rs) (surfaceBounds sur)
        sindreReshape cfg rs
  changeProperty32 d w a2 c2 propModeReplace [fi v]
  selectInput (sindreDisplay cfg) (surfaceWindow sur) sindreEventMask
  lowerWindow (sindreDisplay cfg) (surfaceWindow sur)
  _ <- mapWindow (sindreDisplay cfg) (surfaceWindow sur)
  runSindreX11 (lockX >> start) cfg { sindreReshape = reshape } sur
    where strutArea [left, right, top, bot,
                     left_y1, left_y2, right_y1, right_y2,
                     top_x1, top_x2, bot_x1, bot_x2] =
                           left*(left_y2-left_y1)+right*(right_y2-right_y1)+
                           top*(top_x2-top_x1)+bot*(bot_x2-bot_x1)
          strutArea _ = 0
          getStrutValues r1 r2 = minimumBy (comparing strutArea)
            [[0,0,rectY r1+rectHeight r1,0,
              0,0,0,0,rectX r1,rectX r1 + rectWidth r1,0,0],
             [0,0,0,rectHeight r2 - rectY r1,
              0,0,0,0,0,0,rectX r1,rectX r1 + rectWidth r1],
             [rectX r1+rectWidth r1,0,0,0,
              rectY r1,rectY r1 + rectHeight r1,0,0,0,0,0,0],
             [0,rectWidth r2 - rectX r1,0,0,
              0,0,rectY r1,rectY r1 + rectHeight r1,0,0,0,0]
             ]

-- | An input stream object wrapping the given 'Handle'.  Input is
-- purely event-driven and line-oriented: the event @lines@ is sent
-- (roughly) for each sequence of lines that can be read without
-- blocking, with the payload being a single string value containing
-- the lines read since the last time the event was sent.  When end of
-- file is reached, the @eof@ event (no payload) is sent.
mkInStream :: Handle -> ObjectRef -> SindreX11M (NewObject SindreX11M)
mkInStream h r = do
  evvar <- asks sindreEvtVar
  linevar <- io newEmptyMVar
  let putEv ev = putMVar evvar $ return $ Just $ ev $ ObjectSrc r
      getLines = do
        line <- takeMVar linevar
        case line of Just line' -> getLines' [line'] >> getLines
                     Nothing    -> putEv $ NamedEvent "eof" []
      getLines' lns = do
        line <- yield >> tryTakeMVar linevar
        case line of Just Nothing -> do
                       putEv $ NamedEvent "lines" [asStr lns]
                       putEv $ NamedEvent "eof" []
                     Just (Just line') -> getLines' $ line' : lns
                     Nothing -> putEv $ NamedEvent "lines" [asStr lns]
      readLines = forever (putMVar linevar =<<
                           Just <$> E.decodeUtf8With E.lenientDecode <$> B.hGetLine h)
                  `catch` (\(_::IOException) -> putMVar linevar Nothing)
  _ <- io $ forkIO getLines
  _ <- io $ forkIO readLines
  return $ newObject h M.empty [] (const $ return ())
    where asStr = StringV . T.unlines . reverse

-- | Performs a lookup in the X resources database for a given
-- property.  The class used is @/Sindre/./class/./property/@ and the
-- name is @/progname/./name/./property/@, where /progname/ is the
-- value of 'getProgName'.
xopt :: Param SindreX11M a => Maybe String
        -- ^ Name of widget, using @_@ if 'Nothing' is passed
     -> String -- ^ Widget class
     -> String -- ^ Property name
     -> ConstructorM SindreX11M a
xopt name clss attr = do
  progname <- io getProgName
  let clss' = "Sindre" ++ "." ++ clss ++ "." ++ attr
      name' = progname ++ "." ++ fromMaybe "_" name ++ "." ++ attr
  mdb <- back $ asks sindreRMDB
  case mdb of
    Nothing -> noParam name'
    Just db -> do
      res <- io $ rmGetResource db name' clss'
      case res of
        Nothing -> noParam name'
        Just ("String", v) -> do
          v' <- io $ rmValue v
          maybe (badValue name' $ string v') return =<< back (moldM $ string v')
        Just _ -> badValue name' $ string "<Not a string property>"

instance Param SindreX11M Xft.Color where
  moldM (mold -> Just c) =
    io . flip maybeAllocColor c =<< asks sindreXftMgr
  moldM _ = return Nothing

instance Param SindreX11M Xft.Font where
  moldM (true -> False) = return Nothing
  moldM (mold -> Just s) = do
    mgr <- asks sindreXftMgr
    io $ Xft.openFontName mgr s
  moldM _ = return Nothing

-- | Read visual options from either widget parameters or the X
-- resources database using 'xopt', or a combination.  The following
-- graphical components are read:
--
--  [@Foreground color@] From @fg@ parameter or @foreground@ X
--  property.
--
--  [@Background color@] From @bg@ parameter or @background@ X
--  property.
--
--  [@Focus foreground color@] From @ffg@ parameter or
--  @focusForeground@ X property.
--
--  [@Focus background color@] From @fbg@ parameter or
--  @focusBackground@ X property.
visualOpts :: WidgetRef -> ConstructorM SindreX11M VisualOpts
visualOpts (_, clss, name) = do
  VisualOpts {..} <- back $ asks sindreVisualOpts
  flipcol <- param "highlight" <|> return False
  let pert = if flipcol then flip (,) else (,)
      (fgs, ffgs) = pert ("foreground", foreground)
                         ("focusForeground", focusForeground)
      (bgs, fbgs) = pert ("background", background)
                         ("focusBackground", focusBackground)
  font' <- paramM "font" <|> xopt name clss "font" <|> return font
  fg <- paramM "fg" <|> xopt name clss (fst fgs) <|> pure (snd fgs)
  bg <- paramM "bg" <|> xopt name clss (fst bgs) <|> pure (snd bgs)
  ffg <- paramM "ffg" <|> xopt name clss (fst ffgs) <|> pure (snd ffgs)
  fbg <- paramM "fbg" <|> xopt name clss (fst fbgs) <|> pure (snd fbgs)
  return VisualOpts { foreground = fg, background = bg,
                      focusForeground = ffg, focusBackground = fbg,
                      font = font' }

-- | Helper function that makes it easier it write consistent widgets
-- in the X11 backend.  The widget is automatically filled with its
-- (nonfocus) background color.  You are supposed to use this in the
-- 'drawI' method of a 'Widget' instance definition.  An example:
--
-- @
-- drawI = drawing myWidgetWin myWidgetVisual $ \r fg bg ffg fbg -> do
--   fg drawString 0 5 \"foreground\"
--   bg drawString 0 15 \"background\"
--   ffg drawString 0 25 \"focus foreground\"
--   fbg drawString 0 35 \"focus background\"
-- @
drawing :: VisualOpts
        -> (Rectangle -> Drawer -> Drawer -> ObjectM a SindreX11M [Rectangle])
        -- ^ The body of the @drawing@ call - this function is called
        -- with a rectangle representing the area of the widget, and
        -- 'Drawer's for "foreground," "background", "focus
        -- foreground", and "focus background" respectively.
        -> Rectangle -> ObjectM a SindreX11M SpaceUse
drawing VisualOpts{..} m r@Rectangle{..} = do
  dpy <- back $ asks sindreDisplay
  canvas <- back $ gets surfaceCanvas
  let mkgc fg bg = io $ do gc <- createGC dpy canvas
                           setForeground dpy gc $ Xft.pixel fg
                           setBackground dpy gc $ Xft.pixel bg
                           return gc
  let pass fgc bgc = do fggc <- mkgc fgc bgc
                        bggc <- mkgc bgc fgc
                        return $ Drawer (\f -> f dpy canvas fggc)
                                        (\f -> f dpy canvas bggc)
                                        font fgc bgc
      gcsOf d = [fg d $ \_ _ gc -> gc, bg d $ \_ _ gc -> gc]
  normal <- pass foreground background
  focus  <- pass focusForeground focusBackground
  io $ bg normal fillRectangle (fi rectX) (fi rectY)
                               (fi rectWidth) (fi rectHeight)
  m r normal focus
    <* io (mapM_ (freeGC dpy) (gcsOf normal++gcsOf focus) >> sync dpy False)

-- | Variant of @drawing@ that assumes the entire rectangle is used.
drawing' :: VisualOpts
         -> (Rectangle -> Drawer -> Drawer -> ObjectM a SindreX11M ())
         -> Rectangle -> ObjectM a SindreX11M SpaceUse
drawing' vo m = drawing vo $ \r normal focus -> do
  m r normal focus
  return [r]

-- | A small function that automatically passes appropriate 'Display',
-- 'Window' and 'GC' values to an Xlib drawing function (that,
-- conveniently, always accepts these arguments in the same order).
type CoreDrawer f = (Display -> Drawable -> GC -> f) -> f

data Drawer = Drawer { fg :: forall f. CoreDrawer f
                     , bg :: forall f. CoreDrawer f
                     , drawerFont :: Xft.Font
                     , drawerFgColor :: Xft.Color
                     , drawerBgColor :: Xft.Color
                     }

setFgColor :: Drawer -> Xft.Color -> IO Drawer
setFgColor d c = do
  fg d $ \dpy _ gc -> setForeground dpy gc $ Xft.pixel c
  bg d $ \dpy _ gc -> setBackground dpy gc $ Xft.pixel c
  return d { drawerFgColor = c }

setBgColor :: Drawer -> Xft.Color -> IO Drawer
setBgColor d c = do
  fg d $ \dpy _ gc -> setBackground dpy gc $ Xft.pixel c
  bg d $ \dpy _ gc -> setForeground dpy gc $ Xft.pixel c
  return d { drawerBgColor = c }

padding :: Integral a => a
padding = 2

type X11Field s = FieldDesc s SindreX11M

data Dial = Dial { dialMax    :: Integer
                 , dialVal    :: Integer
                 }

-- | A simple dial using an arc segment to indicate the value compared
-- to the max value.  Accepts @max@ and @value@ parameters (both
-- integers, default values 12 and 0), and a single field: @value@.
-- @<n>@ and @<p>@ are used to increase and decrease the value.
mkDial :: Constructor SindreX11M
mkDial r [] = do
  maxv <- param "max" <|> return 12
  val <- param "value" <|> return 0
  visual <- visualOpts r
  sindre $ return $ newWidget (Dial maxv val)
         M.empty [field value]
         recvEventI composeI (drawI visual)
    where composeI = return (Exact 50, Exact 50)
          value = ReadWriteField "value" (gets dialVal) $ \v ->
            modify (\s -> s { dialVal = clamp 0 v (dialMax s) }) >> redraw
          recvEventI (KeyPress (_, CharKey 'n')) = do
            dmax <- gets dialMax
            changeField_ value $ \v -> return $ clamp 0 (v+1) dmax
          recvEventI (KeyPress (_, CharKey 'p')) = do
            dmax <- gets dialMax
            changeField_ value $ \v -> return $ clamp 0 (v-1) dmax
          recvEventI _ = return ()
          drawI visual = drawing' visual $ \Rectangle{..} d _ -> do
            val    <- gets dialVal
            maxval <- gets dialMax
            io $ do
              let unitAng = 2*pi / fi maxval
                  angle   = (-unitAng) * fi val :: Double
                  dim     = min rectWidth rectHeight - 1
                  cornerX = fi rectX + (rectWidth - dim) `div` 2
                  cornerY = fi rectY + (rectHeight - dim) `div` 2
              fg d drawArc (fi cornerX) (fi cornerY) (fi dim) (fi dim) 0 (360*64)
              fg d fillArc (fi cornerX) (fi cornerY)
                   (fi dim) (fi dim) (90*64) (round $ angle * (180/pi) * 64)
              fg d drawRectangle (fi cornerX) (fi cornerY) (fi dim) (fi dim)
mkDial _ _ = error "Dials do not have children"

-- | Label displaying the text contained in the field @label@, which
-- is also accepted as a widget parameter (defaults to the empty
-- string).
mkLabel :: Constructor SindreX11M
mkLabel wr [] = do
  lbl <- param "label" <|> return []
  visual <- visualOpts wr
  return $ newWidget lbl M.empty
         [field label]
         (const $ return ())
         (composeI visual) (drawI visual)
    where label = ReadWriteField "label" getLabel setLabel
          setLabel v = put v >> fullRedraw
          getLabel = get
          composeI visual = do
            text <- get
            case text of
              [] -> return (Exact 0, Exact $ 2 * padding + Xft.height (font visual))
              _  -> do r <- back $ fmtSize (font visual) text
                       return (Exact $ rectWidth r,
                               Exact $ rectHeight r)
          drawI visual = drawing' visual $ \r fg _ ->
            back . drawFmt fg r =<< get
mkLabel _ _ = error "Labels do not have children"

-- | A blank widget, showing only background color, that can use as
-- much or as little room as necessary.  Useful for constraining the
-- layout of other widgets.
mkBlank :: Constructor SindreX11M
mkBlank r [] = do
  visual <- visualOpts r
  return $ newWidget () M.empty [] (const $ return ())
           (return (Unlimited, Unlimited))
           (drawing' visual $ \_ _ _ -> return ())
mkBlank _ _ = error "Blanks do not have children"

data TextField = TextField { fieldText :: (String, String) }

fieldValue :: TextField -> String
fieldValue = uncurry (++) . first reverse . fieldText

-- | Single-line text field, whose single field @value@ (also a
-- parameter, defaults to the empty string) is the contents of the
-- editing buffer.
mkTextField :: Constructor SindreX11M
mkTextField r [] = do
  v <- param "value" <|> return ""
  visual <- visualOpts r
  return $ newWidget (TextField ("",v)) methods [field value]
                     recvEventI (composeI visual) (drawI visual)
    where methods = M.fromList []
          value   = ReadWriteField "value" getValue setValue
          getValue = T.pack <$> gets fieldValue
          setValue v =
            modify $ \s -> s { fieldText = (reverse $ T.unpack v, "") }
          recvEventI (KeyPress (S.toList -> [], CharKey c)) =
            changingField value $ do
              modify $ \(TextField (bef, aft)) -> TextField (c:bef, aft)
              fullRedraw
          recvEventI (KeyPress k) =
            maybe (return ()) (redraw >>) $ M.lookup k (editorCommands value)
          recvEventI _ = return ()
          composeI visual = do
            text  <- gets fieldValue
            (w,h) <- back $ textExtents (font visual) text
            return (Max $ fi w + padding * 2, Exact $ fi h + padding * 2)
          drawI visual = drawing' visual $ \Rectangle{..} d _ -> do
            (bef,_)   <- gets fieldText
            text      <- gets fieldValue
            (befw, _) <- back $ textExtents (font visual) bef
            (w, h)    <- back $ textExtents (font visual) text
            let width = liftM snd . textExtents (font visual)
            text' <- if w <= fi rectWidth then return text
                     else do fits <- back $ filterM (liftM (<= fi rectWidth) . width)
                                          $ tails $ reverse text
                             case fits of
                               []    -> return ""
                               (t:_) -> return $ reverse t
            back $ drawText (drawerFgColor d) (drawerFont d)
                   (rectX+padding) (rectY+padding)
                   (rectHeight - padding*2) text'
            when (padding+befw <= fi rectWidth) $
              io $ fg d drawLine (fi rectX+padding+fi befw) (fi rectY+padding)
                        (fi rectX+padding+fi befw) (fi rectY+padding+fi h)
mkTextField _ _ = error "TextFields do not have children"

editorCommands :: X11Field TextField T.Text
               -> M.Map Chord (ObjectM TextField SindreX11M ())
editorCommands value = M.fromList
  [ (chord [] "Right", moveForward $ splitAt 1)
  , (chord [Control] 'f', moveForward $ splitAt 1)
  , (chord [] "Left", moveBackward $ splitAt 1)
  , (chord [Control] 'b', moveBackward $ splitAt 1)
  , (chord [Meta] 'f', do moveForward (break isAlphaNum)
                          moveForward (span isAlphaNum))
  , (chord [Meta] 'b', do moveBackward (break isAlphaNum)
                          moveBackward (span isAlphaNum))
  , (chord [Control] 'a', moveBackward (,""))
  , (chord [Control] 'e', moveForward (,""))
  , (chord [] "Home", moveBackward ("",))
  , (chord [] "End", moveForward ("",))
  , (chord [Control] 'w', delBackward word)
  , (chord [Control] "BackSpace", delBackward word)
  , (chord [Meta] 'd', delForward word)
  , (chord [Control] 'k', delForward $ const "")
  , (chord [Control] 'u', delBackward $ const "")
  , (chord [] "BackSpace", delBackward $ drop 1)
  , (chord [Control] 'd', delForward $ drop 1)]
    where word = dropWhile isAlphaNum . dropWhile (not . isAlphaNum)
          moveForward f = modify $ \s ->
            let (bef, (pre, post)) = second f $ fieldText s
            in s { fieldText = (reverse pre ++ bef, post) }
          moveBackward f = modify $ \s ->
            let ((pre, post), aft) = first f $ fieldText s
            in s { fieldText = (post, reverse pre ++ aft) }
          delBackward delf = changingField value $ do
            fullRedraw
            modify $ \s -> s { fieldText = first delf $ fieldText s }
          delForward delf = changingField value $ do
            fullRedraw
            modify $ \s -> s { fieldText = second delf $ fieldText s }

data ListElem = ListElem { showAs   :: FormatString
                         , valueOf  :: T.Text
                         , filterBy :: T.Text }
                deriving (Show, Eq, Ord)

parseListElem :: T.Text -> ListElem
parseListElem s = case KV.parseKV p s of
                    Left  _       -> el
                    Right (v,val) ->
                      case parseFormatString v of
                        Left  _  -> el
                        Right s' -> ListElem (pad s') val $ T.toCaseFold $ textContents s'
  where p  = elf <$?> (Nothing, Just <$> KV.value (T.pack "show"))
                 <||> KV.value (T.pack "value")
        elf s' v' = (fromMaybe v' s', v')
        pad s' = maybeToList (Bg <$> startBg s')
                 ++ [Text $ T.pack " "] ++ s' ++ [Text $ T.pack " "]
        el = ListElem [Text $ T.concat [T.pack " ", s, T.pack " "]] s $ T.toCaseFold s

data NavList = NavList { linePrev :: [ListElem]
                       , lineContents :: Maybe ([(ListElem, Rectangle)],
                                                (ListElem, Rectangle),
                                                [(ListElem, Rectangle)])
                       , lineNext :: [ListElem] }

type Movement m = ([ListElem] -> m ([(ListElem, Rectangle)], [ListElem]))
                -> NavList -> m (Maybe NavList)

contents :: NavList -> [ListElem]
contents NavList { lineContents = Just (pre, cur, aft) } =
  reverse (map fst pre)++[fst cur]++map fst aft
contents _ = []

selected :: NavList -> Maybe ListElem
selected NavList { lineContents = Just (_, (cur, _), _) } = Just cur
selected _ = Nothing

listPrev :: Monad m => Movement m
listPrev _ l@NavList { lineContents = Just (pre:pre', cur, aft) } =
  return $ Just l { lineContents = Just (pre', pre, cur:aft) }
listPrev more l = do
  (conts', rest) <- more (linePrev l)
  case conts' of
    []   -> return Nothing
    x:xs -> return $ Just $ NavList
              rest (Just (xs, x, [])) (contents l++lineNext l)

listNext :: Monad m => Movement m
listNext _ l@NavList { lineContents = Just (pre, cur, aft:aft') } =
  return $ Just l { lineContents = Just (cur:pre, aft, aft') }
listNext more l = do
  (conts', rest) <- more $ lineNext l
  case conts' of
    [] -> return Nothing
    x:xs -> return $ Just $ NavList
              (reverse (contents l)++linePrev l) (Just ([], x, xs)) rest

listLast :: Monad m => Movement m
listLast more l = do
  (line, rest) <- more $ reverse (contents l ++ lineNext l) ++ linePrev l
  case line of [] -> return Nothing
               x:xs -> return $ Just $ NavList rest (Just (xs, x, [])) []

listFirst :: Monad m => Movement m
listFirst more l = do
  (line, rest) <- more $ reverse (linePrev l) ++ contents l ++ lineNext l
  case line of [] -> return Nothing
               x:xs -> return $ Just $ NavList [] (Just ([], x, xs)) rest

moveUntil :: (MonadIO m, Monad m) => Movement m -> (NavList -> Bool) -> Movement m
moveUntil mov p more l | p l = return $ Just l
                       | otherwise = do
  l' <- mov more l
  case l' of Nothing  -> return Nothing
             Just l'' -> moveUntil mov p more l''

lineElems :: (Rectangle -> Integer) -> Rectangle -> [ListElem]
          -> ObjectM List SindreX11M ([(ListElem, Rectangle)], [ListElem])
lineElems rdf r l = elemLine l $ rdf r
  where elemLine [] _ = return ([], [])
        elemLine es@(e:es') room = do
          r' <- join $ gets listElemSize <*> pure e
          if room >= rdf r' then do (es'', rest) <- elemLine es' $ room-rdf r'
                                    return ((e,r'):es'', rest)
                       else return ([], es)

fromElems :: ([(ListElem, Rectangle)], [ListElem]) -> NavList
fromElems ([], rest) = NavList [] Nothing rest
fromElems (x:xs, rest) = NavList [] (Just ([], x, xs)) rest

data List = List { listElems :: [ListElem]
                 , listFilter :: T.Text
                 , listLine :: NavList
                 , listElemSize :: ListElem -> ObjectM List SindreX11M Rectangle
                 , listFilterF :: T.Text -> [ListElem] -> [ListElem]
                 , listSize :: Rectangle
                 , listDim :: Rectangle -> Integer
                 }

listFiltered :: List -> [ListElem]
listFiltered List { listLine = l } =
  reverse (linePrev l) ++ contents l ++ lineNext l

selection :: List -> Maybe T.Text
selection l = liftM f $ lineContents $ listLine l
  where f (_,(c,_),_) = valueOf c

refilter :: T.Text -> [ListElem] -> [ListElem]
refilter f = sortMatches filterBy (T.toCaseFold f)

methInsert :: X11Field List (Maybe T.Text)
           -> T.Text -> ObjectM List SindreX11M ()
methInsert sel vs = changingField sel $ do
  s <- get
  let v    = listFilterF s (listFilter s) $ listFiltered s ++ elems
      p l  = selected l == selected (listLine s)
      more = lineElems (listDim s) (listSize s)
  line  <- fromElems <$> more v
  line' <- moveUntil listNext p more line
  fullRedraw >> put s { listElems = listElems s ++ elems
                      , listLine = fromMaybe line line' }
   where elems = map parseListElem $ T.lines vs

methClear :: X11Field List (Maybe T.Text)
          -> ObjectM List SindreX11M ()
methClear sel = changingField sel $ do
  modify $ \s -> s { listElems = [] , listLine = NavList [] Nothing [] }
  fullRedraw

methFilter :: X11Field List (Maybe T.Text) -> String
           -> ObjectM List SindreX11M ()
methFilter sel f =
  changingField sel $ do
    s <- get
    let v = listFilterF s f' $ if listFilter s `T.isPrefixOf` f'
                               then listFiltered s
                               else listElems s
    line <- fromElems <$> lineElems (listDim s) (listSize s) v
    redraw >> put s { listFilter = f', listLine = line }
  where f' = T.pack f

methMove :: X11Field List (Maybe T.Text)
         -> (([ListElem] -> ObjectM List SindreX11M
                            ([(ListElem, Rectangle)], [ListElem]))
             -> NavList -> ObjectM List SindreX11M (Maybe NavList))
         -> ObjectM List SindreX11M Bool
methMove sel f = do
  dimf <- gets listDim
  rect <- gets listSize
  l <- f (lineElems dimf rect) =<< gets listLine
  case l of Nothing -> return False
            Just l' -> do
              changingField sel $ do
                redraw
                modify $ \s -> s { listLine = l' }
              return True

mkList :: (VisualOpts -> ObjectM List SindreX11M SpaceNeed)
       -> (VisualOpts -> Rectangle -> ObjectM List SindreX11M SpaceUse)
       -> (Rectangle -> Integer)
       -> (VisualOpts -> Rectangle -> ObjectM List SindreX11M Rectangle)
       -> Constructor SindreX11M
mkList cf df dim uf wr [] = do
  visual <- visualOpts wr
  return $ newWidget (List [] T.empty (NavList [] Nothing [])
                     (elemSize visual) refilter mempty dim)
                     methods [field sel, field elements]
                     (const $ return ()) (composeI visual) (drawI visual)
    where methods = M.fromList [ ("insert", function $ methInsert sel)
                               , ("clear", function $ methClear sel)
                               , ("filter", function $ methFilter sel)
                               , ("next", function $ methMove sel listNext)
                               , ("prev", function $ methMove sel listPrev)
                               , ("first", function $ methMove sel listFirst)
                               , ("last", function $ methMove sel listLast)]
          sel = ReadOnlyField "selected" $ gets selection
          elements = ReadOnlyField "elements" $ Dict <$> M.fromList <$>
                     zip (map Number [1..]) <$> map (unmold . showAs) <$>
                     gets listFiltered
          composeI = cf
          drawI visual r = do
            l <- get
            r' <- uf visual r
            when (r' /= listSize l) $ do
              line <- lineElems (listDim l) r' $ listFiltered l
              modify $ \s -> s { listSize = r', listLine = fromElems line }
            df visual r
          elemSize visual = back . fmtSize (font visual) . showAs

mkList _ _ _ _ _ _ = error "Lists do not have children"

-- | Horizontal dmenu-style list containing a list of elements, one of
-- which is the \"selected\" element.  If the parameter @i@ is given a
-- true value, element matching will be case-insensitive.  The
-- following methods are supported:
--
-- [@insert(string)@] Split @string@ into lines and add each line as
-- an element.
--
-- [@clear()@] Delete all elements.
--
-- [@filter(string)@] Only display those elements that contain @string@.
--
-- [@next()@] Move selection right.
--
-- [@prev()@] Move selection left.
--
-- [@first()@] Move to leftmost element.
--
-- [@last()@] Move to rightmost element.
--
-- The field @selected@ is the selected element.
mkHList :: Constructor SindreX11M
mkHList = mkList composeHoriz drawHoriz rectWidth usable
  where composeHoriz = return . (Unlimited,) . Exact . Xft.height . font

        prestr = "< "
        aftstr = "> "

        usable visual r = do
          (w1, _) <- back $ textExtents (font visual) prestr
          (w2, _) <- back $ textExtents (font visual) aftstr
          return r { rectWidth = rectWidth r - fi w1 - fi w2 }

        drawHoriz visual = drawing' visual $ \r d fd -> do
          (prestrw,_) <- back $ textExtents (font visual) prestr
          let (x,y,w,h) = ( fi $ rectX r, rectY r
                          , fi $ rectWidth r, rectHeight r)
              drawElem d' x' (e,r') = back $ do
                drawFmt d' (r' { rectX = x', rectY = rectY r }) $ showAs e
                return $ x'+rectWidth r'
          line <- gets listLine
          case lineContents line of
            Just (pre, cur, aft) -> do
              unless (null $ linePrev line) $
                back $ drawText (drawerFgColor d) (drawerFont d) x y h prestr
              x' <- foldM (drawElem d)
                          (fi $ prestrw + fi x) $ reverse pre
              x'' <- drawElem fd x' cur
              foldM_ (drawElem d)  x'' aft
              unless (null $ lineNext line) $ back $ do
                (aftw,_) <- textExtents (font visual) aftstr
                drawText (drawerFgColor d) (drawerFont d)
                  (x + w - aftw) y h aftstr
            Nothing -> return ()

-- | As 'mkHList', except the list is vertical.  The parameter @lines@
-- (default value 10) is the number of lines shown.
mkVList :: Constructor SindreX11M
mkVList k cs = do
  n <- param "lines" <|> return 10
  mkList (composeVert n) drawVert rectHeight (const return) k cs
  where composeVert n visual =
          return (Unlimited, Exact $ (Xft.height (font visual) + 2*padding) * n)

        drawVert visual = drawing' visual $ \r d fd -> do
          let fr y r' = r { rectY = y, rectHeight = rectHeight r' }
              drawElem d' y (e, r') = do
                drawFmt d' (fr y r') $ showAs e
                return $ y + rectHeight r'
          line <- gets (lineContents . listLine)
          case line of
            Just (pre, cur, aft) -> back $ do
              y' <- foldM (drawElem d) (rectY r) $ reverse pre
              y'' <- drawElem fd y' cur
              foldM_ (drawElem d) y'' aft
            Nothing -> return ()
