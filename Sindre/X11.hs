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
                 , SindreX11Conf( sindreDisplay, sindreScreen
                                , sindreVisualOpts, sindreRoot)
                 , sindreX11override
                 , sindreX11dock
                 , sindreX11
                 , xopt
                 , VisualOpts(..)
                 , visualOpts
                 , allocColour
                 , drawing
                 , drawing'
                 , Drawer
                 , drawText
                 , textHeight
                 , textWidth
                 , windowSize
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
import Sindre.Compiler
import Sindre.Lib
import Sindre.Runtime
import Sindre.Util
import Sindre.Widgets

import Graphics.X11.Xlib hiding ( refreshKeyboardMapping
                                , Rectangle 
                                , badValue )
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras hiding (Event, getEvent)
import qualified Graphics.X11.Xlib.Extras as X
import Graphics.X11.Xinerama
import Graphics.X11.Xshape
import Graphics.X11.Xim
import Graphics.X11.XRM

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

import Prelude hiding (catch)

fromXRect :: X.Rectangle -> Rectangle
fromXRect r =
    Rectangle { rectX = fi $ rect_x r
              , rectY = fi $ rect_y r
              , rectWidth = fi $ rect_width r
              , rectHeight = fi $ rect_height r }

type EventThunk = Sindre SindreX11M (Maybe Event)

-- | The read-only configuration of the X11 backend, created during
-- backend initialisation.
data SindreX11Conf = SindreX11Conf {
      sindreDisplay    :: Display
    -- ^ The display that we are -- connected to.
    , sindreScreen     :: Screen
    -- ^ Our current screen.
    , sindreRoot       :: Window
    -- ^ The window that will be the ultimate ancestor of all windows
    -- created by the backend (this is not the same as the X11 root
    -- window).
    , sindreSurface    :: Drawable
    , sindreScreenSize :: Rectangle
    -- ^ The full size of the screen.
    , sindreVisualOpts :: VisualOpts
    -- ^ The default visual options (colour, font, etc) used if no
    -- others are specified for a widget.
    , sindreRMDB       :: RMDatabase
    -- ^ The X11 resource database (Xdefaults/Xresources).
    , sindreXlock      :: Xlock
    -- ^ Synchronisation lock for Xlib access.
    , sindreEvtVar     :: MVar EventThunk
    -- ^ Channel through which events are sent by other threads to the
    -- Sindre command loop.
    , sindreReshape    :: [Rectangle] -> SindreX11M ()
    -- ^ A function that reshapes the 'sindreRoot' to be the union of
    -- the given rectangles (as per the XShape extension).
    }

-- | Sindre backend using Xlib.
newtype SindreX11M a = SindreX11M (ReaderT SindreX11Conf IO a)
  deriving (Functor, Monad, MonadIO, MonadReader SindreX11Conf, Applicative)

runSindreX11 :: SindreX11M a -> SindreX11Conf -> IO a
runSindreX11 (SindreX11M m) = runReaderT m

instance MonadBackend SindreX11M where
  type BackEvent SindreX11M = (KeySym, String, X.Event)
  
  initDrawing (orient, rootwr) = do
    SindreX11Conf{ sindreScreenSize=screen
                 , sindreDisplay=dpy
                 , sindreRoot=win
                 , sindreSurface=sur } <- back ask
    orient' <- case orient of
      Just orient' -> maybe (fail $ "position '"
                             ++ show orient'
                             ++ "' for root window not known")
                      return (mold orient')
      Nothing -> return (AlignCenter, AlignCenter)
    gc <- io $ createGC dpy win
    let rootRedraw = do
          reqs <- compose rootwr
          reshape <- back $ asks sindreReshape
          winsize <- back $ windowSize win
          let rect = adjustRect orient' winsize $ fitRect winsize reqs
          usage <- draw rootwr $ Just rect
          back $ reshape usage
          redrawed usage
        redrawed rects = do
          let Rectangle{..} = mconcat rects
          io $ copyArea dpy sur win gc
               (fi rectX) (fi rectY)
               (fi rectWidth) (fi rectHeight)
               (fi rectX) (fi rectY)
          io $ sync dpy False
    return (rootRedraw, redrawed)
  
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

drawableSize :: Display -> Drawable -> IO Rectangle
drawableSize dpy drw = do
  (_,x,y,w, h,_,_) <- io $ getGeometry dpy drw
  return $ Rectangle (fi x) (fi y) (fi w) (fi h)

windowSize :: Window -> SindreX11M Rectangle
windowSize win = do dpy <- asks sindreDisplay
                    io $ drawableSize dpy win

textAscent :: FontStruct -> String -> Position
textAscent fstruct text = a
    where (_, a, _, _) = textExtents fstruct text

textHeight :: FontStruct -> String -> Position
textHeight fstruct text = a+d
    where (_, a, d, _) = textExtents fstruct text

drawText :: Display -> Window -> GC 
         -> Position -> Position -> Dimension 
         -> FontStruct -> String -> IO ()
drawText dpy win gc x y h fs str =
  drawString dpy win gc x y' str
    where y' = textAscent fs str +
               align AlignCenter y (textHeight fs str) (y+fi h)

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
  (_, _, _, x, y, _, _, _) <- queryPointer dpy rootw
  let hasPointer rect = fi x >= rect_x rect &&
                        fi (rect_width rect) + rect_x rect > fi x &&
                        fi y >= rect_y rect &&
                        fi (rect_height rect) + rect_y rect > fi y
  fromJust <$> find hasPointer <$> getScreenInfo dpy

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
processX11Event (_, _, ExposeEvent { ev_count = 0, ev_window = win
                                   , ev_x = x, ev_y = y
                                   , ev_width = w, ev_height = h }) =
  do sur <- back $ asks sindreSurface
     dpy <- back $ asks sindreDisplay
     io $ do gc <- createGC dpy win
             copyArea dpy sur win gc (fi x) (fi y) (fi w) (fi h) (fi x) (fi y)
             freeGC dpy gc
             return Nothing
processX11Event (_, _, ConfigureEvent {}) = do fullRedraw
                                               return Nothing
processX11Event  _ = return Nothing

eventReader :: Display -> Window -> XIC -> MVar EventThunk ->
               Xlock -> IO ()
eventReader dpy win ic evvar xlock = forever $ do
    lockXlock xlock
    cnt <- eventsQueued dpy queuedAfterFlush
    when (cnt == 0) $ do
      -- The following two lines have a race condition.
      unlockXlock xlock
      threadWaitRead $ Fd $ connectionNumber dpy
      lockXlock xlock
    xev <- getX11Event dpy win ic
    unlockXlock xlock
    putMVar evvar $ processX11Event xev

-- | Get the 'Pixel' value for a named colour if it exists
maybeAllocColour :: Display -> String -> IO (Maybe Pixel)
maybeAllocColour dpy c = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  catch (Just . color_pixel . fst <$> allocNamedColor dpy colormap c)
    (\(_ :: IOException) -> return Nothing)

allocColour :: MonadIO m => Display -> String -> m Pixel
allocColour dpy c = io (maybeAllocColour dpy c) >>=
                    maybe (fail $ "Unknown color '"++c++"'") return

shapeWindow :: Display -> Window -> Pixmap -> Rectangle
            -> [Rectangle] -> IO ()
shapeWindow dpy win pm full rects = do
  maskgc <- createGC dpy pm
  setForeground dpy maskgc 0
  unmaskgc <- createGC dpy pm
  setForeground dpy unmaskgc 1
  fillRectangle dpy pm maskgc 0 0 (fi $ rectWidth full) (fi $ rectHeight full)
  forM_ rects $ \rect ->
    fillRectangle dpy pm unmaskgc
      (fi $ rectX rect)
      (fi $ rectY rect)
      (fi $ rectWidth rect)
      (fi $ rectHeight rect)
  xshapeCombineMask dpy win shapeBounding 0 0 pm shapeSet
  freeGC dpy maskgc
  freeGC dpy unmaskgc

sindreX11Cfg :: String -> Bool -> IO SindreX11Conf
sindreX11Cfg dstr o = do
  sl <- supportsLocale
  unless sl $ putStrLn "Current locale is not supported" >> exitFailure
  _ <- setLocaleModifiers ""
  dpy <- setupDisplay dstr
  rmInitialize
  db <- rmGetStringDatabase $ resourceManagerString dpy
  let scr = defaultScreenOfDisplay dpy
  rect <- findRectangle dpy (rootWindowOfScreen scr)
  win <- mkWindow dpy scr (rootWindowOfScreen scr) o
         (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)
  pm <- createPixmap dpy win (rect_width rect) (rect_height rect) 1
  sf <- createPixmap dpy win (rect_width rect) (rect_height rect) $
        defaultDepthOfScreen scr
  shapeWindow dpy win pm (fromXRect rect) [Rectangle 0 0 0 0]
  im <- openIM dpy Nothing Nothing Nothing
  ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
  visopts <- defVisualOpts dpy
  evvar <- newEmptyMVar
  xlock <- newMVar ()
  _ <- forkIO $ eventReader dpy win ic evvar xlock
  return SindreX11Conf
             { sindreDisplay = dpy
             , sindreScreen = scr
             , sindreRoot = win
             , sindreSurface = sf
             , sindreScreenSize = fromXRect rect
             , sindreVisualOpts = visopts
             , sindreRMDB = db
             , sindreEvtVar = evvar
             , sindreXlock = xlock
             , sindreReshape = io . shapeWindow dpy win pm (fromXRect rect) }

-- | Options regarding visual appearance of widgets (colours and
-- fonts).
data VisualOpts = VisualOpts {
      foreground      :: Pixel
    , background      :: Pixel
    , focusForeground :: Pixel
    , focusBackground :: Pixel
    , font            :: FontStruct
    }

defVisualOpts :: Display -> IO VisualOpts
defVisualOpts dpy =
  pure VisualOpts
           <*> f fg
           <*> f bg
           <*> f ffg
           <*> f fbg
           <*> loadQueryFont dpy "fixed"
      where (fg, bg, ffg, fbg) = ("black", "grey", "white", "blue")
            f = allocColour dpy

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
  cfg <- sindreX11Cfg dstr True
  _ <- io $ mapRaised (sindreDisplay cfg) (sindreRoot cfg)
  status <- grabInput (sindreDisplay cfg) (sindreRoot cfg)
  unless (status == grabSuccess) $
    error "Could not establish keyboard grab"
  runSindreX11 (lockX >> start) cfg

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
  cfg <- sindreX11Cfg dstr False
  _ <- io $ mapRaised (sindreDisplay cfg) (sindreRoot cfg)
  selectInput (sindreDisplay cfg) (sindreRoot cfg)
    (keyPressMask .|. keyReleaseMask .|. exposureMask .|. structureNotifyMask)
  runSindreX11 (lockX >> start) cfg

-- | Execute Sindre in the X11 backend as a dock/statusbar.
sindreX11dock :: String -- ^ The display string (usually the value of the
                        -- environment variable @$DISPLAY@ or @:0@)
              -> SindreX11M ExitCode
              -- ^ The function returned by
              -- 'Sindre.Compiler.compileSindre' after command line
              -- options have been given
              -> IO ExitCode
sindreX11dock dstr start = do
  cfg <- sindreX11Cfg dstr False
  let d = sindreDisplay cfg
      w = sindreRoot cfg
  a1 <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
  c1 <- internAtom d "CARDINAL"                 False
  a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
  c2 <- internAtom d "ATOM"                     False
  v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
  let reshape rs = do
        io $ changeProperty32 d w a1 c1 propModeReplace $ map fi $
          getStrutValues (mconcat rs) (sindreScreenSize cfg)
        sindreReshape cfg rs
  changeProperty32 d w a2 c2 propModeReplace [fi v]
  lowerWindow (sindreDisplay cfg) (sindreRoot cfg)
  _ <- mapWindow (sindreDisplay cfg) (sindreRoot cfg)
  runSindreX11 (lockX >> start) cfg { sindreReshape = reshape }
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

data InStream = InStream Handle

instance (MonadIO m, MonadBackend m) => Object m InStream where

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
                           Just <$> E.decodeUtf8 <$> B.hGetLine h)
                  `catch` (\(_::IOException) -> putMVar linevar Nothing)
  _ <- io $ forkIO getLines
  _ <- io $ forkIO readLines
  return $ NewObject $ InStream h
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
  db <- back $ asks sindreRMDB
  res <- io $ rmGetResource db name' clss'
  case res of
    Nothing -> noParam name'
    Just ("String", v) -> do
      v' <- io $ rmValue v
      maybe (badValue name' $ string v') return =<< back (moldM $ string v')
    Just _ -> badValue name' $ string "<Not a string property>"

instance Param SindreX11M Pixel where
  moldM (mold -> Just c) = io . flip maybeAllocColour c =<< asks sindreDisplay
  moldM _ = return Nothing

instance Param SindreX11M FontStruct where
  moldM (mold -> Just s) = do
    dpy <- asks sindreDisplay
    io $ (Just <$> loadQueryFont dpy s) `catch`
         \(_::IOException) -> return Nothing
  moldM _ = return Nothing

-- | Read visual options from either widget parameters or the X
-- resources database using 'xopt', or a combination.  The following
-- graphical components are read:
--
--  [@Foreground colour@] From @fg@ parameter or @foreground@ X
--  property.
--
--  [@Background colour@] From @bg@ parameter or @background@ X
--  property.
--
--  [@Focus foreground colour@] From @ffg@ parameter or
--  @focusForeground@ X property.
--
--  [@Focus background colour@] From @fbg@ parameter or
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
-- (nonfocus) background colour.  You are supposed to use this in the
-- 'drawI' method of a 'Widget' instance definition.  An example:
--
-- @
-- drawI = drawing myWidgetWin myWidgetVisual $ \r fg bg ffg fbg -> do
--   fg drawString 0 5 \"foreground\"
--   bg drawString 0 15 \"background\"
--   ffg drawString 0 25 \"focus foreground\"
--   fbg drawString 0 35 \"focus background\"
-- @
drawing :: (a -> VisualOpts)
        -> (Rectangle -> Drawer -> Drawer -> Drawer -> Drawer
            -> ObjectM a SindreX11M [Rectangle])
        -- ^ The body of the @drawing@ call - this function is called
        -- with a rectangle representing the area of the widget, and
        -- 'Drawer's for "foreground," "background", "focus
        -- foreground", and "focus background" respectively.
        -> Rectangle -> ObjectM a SindreX11M SpaceUse
drawing vf m r@Rectangle{..} = do
  dpy <- back $ asks sindreDisplay
  sur <- back $ asks sindreSurface
  VisualOpts{..} <- vf <$> get
  let mkgc fg bg = io $ do gc <- createGC dpy sur
                           setForeground dpy gc fg
                           setBackground dpy gc bg
                           setFont dpy gc $ fontFromFontStruct font
                           return gc
  fggc <- mkgc foreground background
  bggc <- mkgc background foreground
  ffggc <- mkgc focusForeground focusBackground
  fbggc <- mkgc focusBackground focusForeground
  io $ fillRectangle dpy sur bggc (fi rectX) (fi rectY)
         (fi rectWidth) (fi rectHeight)
  let pass :: GC -> Drawer
      pass gc f = f dpy sur gc
  m r (pass fggc) (pass bggc) (pass ffggc) (pass fbggc)
    <* io (mapM_ (freeGC dpy) [fggc, bggc, ffggc, fbggc] >> sync dpy False)

-- | Variant of @drawing@ that assumes the entire rectangle is used.
drawing' :: (a -> VisualOpts)
         -> (Rectangle -> Drawer -> Drawer -> Drawer -> Drawer
             -> ObjectM a SindreX11M ())
         -> Rectangle -> ObjectM a SindreX11M SpaceUse
drawing' vf m = drawing vf $ \r fg bg ffg fbg -> do
  m r fg bg ffg fbg
  return [r]
  
-- | A small function that automatically passes appropriate 'Display',
-- 'Window' and 'GC' values to an Xlib drawing function (that,
-- conveniently, always accepts these arguments in the same order).
type Drawer = forall f. ((Display -> Window -> GC -> f) -> f)

padding :: Integral a => a
padding = 2
                
data Dial = Dial { dialMax    :: Integer
                 , dialVal    :: Integer
                 , dialVisual :: VisualOpts
                 }

instance Object SindreX11M Dial where
    fieldSetI "value" (mold -> Just v) = do
      modify $ \s -> s { dialVal = clamp 0 v (dialMax s) }
      redraw >> IntegerV <$> gets dialVal
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "value" = IntegerV <$> gets dialVal
    fieldGetI _       = return $ IntegerV 0

    recvEventI (KeyPress (_, CharKey 'n')) =
      changeFields [("value", unmold . dialVal)] $ \s -> do
        redraw
        return s { dialVal = clamp 0 (dialVal s+1) (dialMax s) }
    recvEventI (KeyPress (_, CharKey 'p')) =
      changeFields [("value", unmold . dialVal)] $ \s -> do
        redraw
        return s { dialVal = clamp 0 (dialVal s-1) (dialMax s) }
    recvEventI _ = return ()

instance Widget SindreX11M Dial where
    composeI = return (Exact 50, Exact 50)
    drawI = drawing' dialVisual $ \Rectangle{..} fg _ _ _ -> do
      val    <- gets dialVal
      maxval <- gets dialMax
      io $ do
        let unitAng = 2*pi / fi maxval
            angle   = (-unitAng) * fi val :: Double
            dim     = min rectWidth rectHeight - 1
            cornerX = fi rectX + (rectWidth - dim) `div` 2
            cornerY = fi rectY + (rectHeight - dim) `div` 2
        fg drawArc (fi cornerX) (fi cornerY) (fi dim) (fi dim) 0 (360*64)
        fg fillArc (fi cornerX) (fi cornerY)
           (fi dim) (fi dim) (90*64) (round $ angle * (180/pi) * 64)
        fg drawRectangle (fi cornerX) (fi cornerY) (fi dim) (fi dim)

-- | A simple dial using an arc segment to indicate the value compared
-- to the max value.  Accepts @max@ and @value@ parameters (both
-- integers, default values 12 and 0), and a single field: @value@.
-- @<n>@ and @<p>@ are used to increase and decrease the value.
mkDial :: Constructor SindreX11M
mkDial r [] = do
  maxv <- param "max" <|> return 12
  val <- param "value" <|> return 0
  visual <- visualOpts r
  sindre $ return $ NewWidget $ Dial maxv val visual
mkDial _ _ = error "Dials do not have children"

data Label = Label { labelText :: String 
                   , labelVisual :: VisualOpts
                   }

instance Object SindreX11M Label where
    fieldSetI "label" v = do
      modify $ \s -> s { labelText = fromMaybe "" $ mold v }
      fullRedraw
      string <$> gets labelText
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "label" = string <$> gets labelText
    fieldGetI _       = return $ IntegerV 0

instance Widget SindreX11M Label where
    composeI = do
      fstruct <- gets (font . labelVisual)
      text <- gets labelText
      case text of
        "" -> return (Exact 0, Max 0)
        _  -> return (Exact $ fi (textWidth fstruct text) + padding * 2,
                      Exact $ fi (textHeight fstruct text) + padding * 2)
    drawI = drawing' labelVisual $ \Rectangle{..} fg _ _ _ -> do
      fstruct <- gets (font . labelVisual)
      label <- gets labelText
      io $ fg drawText (fi rectX+padding) (fi rectY+padding)
                       (fi rectHeight-padding*2) fstruct label

-- | Label displaying the text contained in the field @label@, which
-- is also accepted as a widget parameter (defaults to the empty
-- string).
mkLabel :: Constructor SindreX11M
mkLabel r [] = do
  label <- param "label" <|> return ""
  visual <- visualOpts r
  return $ NewWidget $ Label label visual
mkLabel _ _ = error "Labels do not have children"

data Blank = Blank { blankVisual :: VisualOpts }
                
instance Object SindreX11M Blank where
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI _   = return $ IntegerV 0

instance Widget SindreX11M Blank where
    composeI = return (Unlimited, Unlimited)
    drawI = drawing' blankVisual $ \_ _ _ _ _ -> return ()

-- | A blank widget, showing only background colour, that can use as
-- much or as little room as necessary.  Useful for constraining the
-- layout of other widgets.
mkBlank :: Constructor SindreX11M
mkBlank r [] = do
  visual <- visualOpts r
  return $ NewWidget $ Blank visual
mkBlank _ _ = error "Blanks do not have children"

data TextField = TextField { fieldText :: (String, String)
                           , fieldVisual :: VisualOpts }

fieldValue :: TextField -> String
fieldValue = uncurry (++) . first reverse . fieldText

instance Object SindreX11M TextField where
    fieldSetI "value" (mold -> Just v) = do
      modify $ \s -> s { fieldText = (reverse v, "") }
      fullRedraw
      return $ string v
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "value" = string <$> fieldValue <$> get
    fieldGetI _       = return $ IntegerV 0
    
    recvEventI (KeyPress (S.toList -> [], CharKey c)) =
      changeFields [("value", unmold . fieldValue)] $ \s -> do
        let (bef, aft) = fieldText s
        fullRedraw
        return s { fieldText = (c:bef, aft) }
    recvEventI (KeyPress k) =
      maybe (return ()) (redraw >>) $ M.lookup k editorCommands
    recvEventI _ = return ()

editorCommands :: M.Map Chord (ObjectM TextField SindreX11M ())
editorCommands = M.fromList
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
          delBackward delf = changeFields [("value", unmold . fieldValue)]
                             $ \s -> do
            fullRedraw
            return s { fieldText = first delf $ fieldText s }
          delForward delf = changeFields [("value", unmold . fieldValue)]
                            $ \s -> do
            fullRedraw
            return s { fieldText = second delf $ fieldText s }

instance Widget SindreX11M TextField where
    composeI = do
      fstruct <- gets (font . fieldVisual)
      text <- gets fieldValue
      return (Max $ fi (textWidth fstruct text) + padding * 2,
              Exact $ fi (textHeight fstruct text) + padding * 2)
    drawI = drawing' fieldVisual $ \Rectangle{..} fg _ _ _ -> do
      (bef,_) <- gets fieldText
      text <- gets fieldValue
      fstruct <- gets (font . fieldVisual)
      io $ do
        let h = ascentFromFontStruct fstruct + descentFromFontStruct fstruct
            w' = textWidth fstruct bef
            text' = if textWidth fstruct text <= fi rectWidth then text
                    else let fits = (<= fi rectWidth) . textWidth fstruct
                         in case filter fits $ map (("..."++) . drop 3)
                                 $ tails $ reverse text of
                              []    -> ""
                              (t:_) -> reverse t
        fg drawText (fi rectX+padding) (fi rectY+padding)
           (fi rectHeight - padding*2) fstruct text'
        when (padding+w' <= fi rectWidth) $
          fg drawLine (fi rectX+padding+w') (fi rectY+padding)
                      (fi rectX+padding+w') (fi rectY+padding+h)

-- | Single-line text field, whose single field @value@ (also a
-- parameter, defaults to the empty string) is the contents of the
-- editing buffer.
mkTextField :: Constructor SindreX11M
mkTextField r [] = do
  v <- param "value" <|> return ""
  visual <- visualOpts r
  return $ NewWidget $ TextField ("",v) visual
mkTextField _ _ = error "TextFields do not have children"

data NavList = NavList { linePrev :: [T.Text]
                       , lineContents :: Maybe ([(T.Text, Rectangle)],
                                                (T.Text, Rectangle),
                                                [(T.Text, Rectangle)])
                       , lineNext :: [T.Text] }

type Movement m = ([T.Text] -> m ([(T.Text, Rectangle)], [T.Text]))
                -> NavList -> m (Maybe NavList)

contents :: NavList -> [T.Text]
contents NavList { lineContents = Just (pre, cur, aft) } =
  reverse (map fst pre)++[fst cur]++map fst aft
contents _ = []

listPrev :: Monad m => Movement m
listPrev _ l@NavList { lineContents = Just (pre:pre', cur, aft) } =
  return $ Just l { lineContents = Just (pre', pre, cur:aft) }
listPrev more l = do
  (conts', rest) <- more $ linePrev l
  case conts' of [] -> return Nothing
                 x:xs -> return $ Just $ NavList
                          rest (Just (xs, x, [])) (contents l++lineNext l)

listNext :: Monad m => Movement m
listNext _ l@NavList { lineContents = Just (pre, cur, aft:aft') } =
  return $ Just l { lineContents = Just (cur:pre, aft, aft') }
listNext more l = do
  (conts', rest) <- more $ lineNext l
  case conts' of [] -> return Nothing
                 x:xs -> return $ Just $ NavList
                         (contents l++linePrev l) (Just ([], x, xs)) rest

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

lineElems :: (Rectangle -> Integer) -> Rectangle -> [T.Text]
          -> ObjectM List SindreX11M ([(T.Text, Rectangle)], [T.Text])
lineElems rdf r l = elemLine l $ rdf r
  where elemLine [] _ = return ([], [])
        elemLine es@(e:es') room = do
          r' <- elemRect e
          if room >= rdf r' then do (es'', rest) <- elemLine es' $ room-rdf r'
                                    return ((e,r'):es'', rest)
                       else return ([], es)

fromElems :: ([(T.Text, Rectangle)], [T.Text]) -> NavList
fromElems ([], rest) = NavList [] Nothing rest
fromElems (x:xs, rest) = NavList [] (Just ([], x, xs)) rest

elemRect :: T.Text -> ObjectM List SindreX11M Rectangle
elemRect e = do
  fstruct <- gets (font . listVisual)
  let spacing = textWidth fstruct " "
  return $ Rectangle 0 0
           (fi $ textWidth fstruct (T.unpack e) + spacing * 2)
           (fi $ ascentFromFontStruct fstruct + descentFromFontStruct fstruct)

data List = List { listElems :: [T.Text]
                 , listFilter :: T.Text
                 , listLine :: NavList
                 , listVisual :: VisualOpts
                 , listCompose :: ObjectM List SindreX11M SpaceNeed
                 , listDraw :: Rectangle
                            -> ObjectM List SindreX11M SpaceUse
                 , listFilterF :: T.Text -> [T.Text] -> [T.Text]
                 , listUsableRect :: Rectangle
                                  -> ObjectM List SindreX11M Rectangle
                 , listSize :: Rectangle
                 , listDim :: Rectangle -> Integer
                 }

listFiltered :: List -> [T.Text]
listFiltered List { listLine = l } =
  reverse (linePrev l) ++ contents l ++ lineNext l

selection :: List -> Value
selection l = maybe falsity f $ lineContents $ listLine l
  where f (_,(c,_),_) = StringV c

refilter :: (T.Text -> T.Text) -> T.Text -> [T.Text] -> [T.Text]
refilter tr f ts = exacts++prefixes++infixes
  where (exacts, nonexacts) = partition (==f') $ map tr ts
        (prefixes, nonprefixes) = partition (T.isPrefixOf f') nonexacts
        (infixes, _) = partition (T.isInfixOf f') nonprefixes
        f' = tr f

methInsert :: T.Text -> ObjectM List SindreX11M ()
methInsert vs = changeFields [("selected", selection)] $ \s -> do
  line <- fromElems <$> lineElems (listDim s) (listSize s)
          (listFiltered s ++ listFilterF s (listFilter s) lines')
  fullRedraw
  return s { listElems = listElems s ++ lines'
           , listLine = line }
   where lines' = T.lines vs

methClear :: ObjectM List SindreX11M ()
methClear = do
  modify $ \s -> s { listElems = [] , listLine = NavList [] Nothing [] }
  fullRedraw

methFilter :: String -> ObjectM List SindreX11M ()
methFilter f =
  changeFields [("selected", selection)] $ \s -> do
    let v = listFilterF s f' $ if listFilter s `T.isPrefixOf` f'
                               then listFiltered s
                               else listElems s
    line <- fromElems <$> lineElems (listDim s) (listSize s) v
    redraw >> return s { listFilter = f', listLine = line }
  where f' = T.pack f

methMove :: (([T.Text] -> ObjectM List SindreX11M ([(T.Text, Rectangle)], [T.Text]))
             -> NavList -> ObjectM List SindreX11M (Maybe NavList))
         -> ObjectM List SindreX11M Bool
methMove f = do
  dimf <- gets listDim
  rect <- gets listSize
  l <- f (lineElems dimf rect) =<< gets listLine
  case l of Nothing -> return False
            Just l' -> do
              changeFields [("selected", selection)] $ \s -> do
                redraw
                return s { listLine = l' }
              return True

instance Object SindreX11M List where
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "selected" = selection <$> get
    fieldGetI "elements" = Dict <$> M.fromList <$>
                           zip (map IntegerV [1..]) <$>
                           map unmold <$> listFiltered <$> get
    fieldGetI _ = return $ IntegerV 0
    callMethodI "insert" = function methInsert
    callMethodI "clear"  = function methClear
    callMethodI "filter" = function methFilter
    callMethodI "next" = function $ methMove listNext
    callMethodI "prev" = function $ methMove listPrev
    callMethodI "first" = function $ methMove listFirst
    callMethodI "last" = function $ methMove listLast
    callMethodI m = fail $ "Unknown method '" ++ m ++ "'"

instance Widget SindreX11M List where
    composeI = join $ gets listCompose
    drawI r = do l <- get
                 r' <- listUsableRect l r
                 when (r' /= listSize l) $ do
                   line <- lineElems (listDim l) r' $ listFiltered l
                   modify $ \s -> s { listSize = r', listLine = fromElems line }
                 listDraw l r

mkList :: ObjectM List SindreX11M SpaceNeed
       -> (Rectangle -> ObjectM List SindreX11M SpaceUse)
       -> (Rectangle -> Integer)
       -> (Rectangle -> ObjectM List SindreX11M Rectangle)
       -> Constructor SindreX11M
mkList cf df dim uf r [] = do
  visual <- visualOpts r
  insensitive <- param "i" <|> return False
  let trf = if insensitive then T.toCaseFold else id
  return $ NewWidget $ List [] T.empty (NavList [] Nothing [])
         visual cf df (refilter trf) uf mempty dim
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
  where composeHoriz = do fstruct <- gets (font . listVisual)
                          let h = ascentFromFontStruct fstruct +
                                  descentFromFontStruct fstruct
                          return (Unlimited, Exact $ fi h + padding * 2)

        prestr = "< "
        aftstr = "> "

        usable r = do fstruct <- gets (font . listVisual)
                      let w = textWidth fstruct prestr
                              + textWidth fstruct aftstr
                      return r { rectWidth = rectWidth r - fi w }

        drawHoriz = drawing' listVisual $ \r fg _ ffg fbg -> do
          fstruct <- gets (font . listVisual)
          let y = fi $ rectY r
              spacing = textWidth fstruct " "
              h       = fi $ rectHeight r
              prestrw = textWidth fstruct prestr
              drawElem x (e,r') = do
                io $ fg drawText (x+spacing) y h fstruct $ T.unpack e
                return $ x+fi (rectWidth r')
              drawFocus x (e,r') = do
                io $ fbg fillRectangle x y (fi $ rectWidth r') h
                io $ ffg drawText (x+spacing) y h fstruct
                   $ T.unpack e
                return $ x+fi (rectWidth r')
          line <- gets listLine
          case lineContents line of
            Just (pre, cur, aft) -> do
              unless (null $ linePrev line) $
                io $ fg drawText (fi $ rectX r) y h fstruct prestr
              x' <- foldM drawElem (prestrw + fi (rectX r)) $ reverse pre
              x'' <- drawFocus x' cur
              foldM_ drawElem x'' aft
              unless (null $ lineNext line) $ do
                let aftpos = fi (rectX r + rectWidth r)
                             - textWidth fstruct aftstr
                io $ fg drawText aftpos y h fstruct aftstr
            Nothing -> return ()

-- | As 'mkHList', except the list is vertical.  The parameter @lines@
-- (default value 10) is the number of lines shown.
mkVList :: Constructor SindreX11M
mkVList k cs = do
  n <- param "lines" <|> return 10
  mkList (composeVert n) drawVert rectHeight return k cs
  where composeVert n = do fstruct <- gets (font . listVisual)
                           let h = ascentFromFontStruct fstruct +
                                   descentFromFontStruct fstruct
                           return (Unlimited, Exact $ (fi h + padding * 2) * n)

        drawVert = drawing' listVisual $ \Rectangle{..} fg _ ffg fbg -> do
          fstruct <- gets (font . listVisual)
          let h = ascentFromFontStruct fstruct + descentFromFontStruct fstruct
              drawElem y e = do
                fg drawText (fi rectX+padding) (y+padding) (fi h) fstruct e
                return $ y + h
              drawFocus y e = do
                fbg fillRectangle (fi rectX) y (fi rectWidth) (fi h+2*padding)
                ffg drawText (fi rectX+padding) (y+padding) (fi h) fstruct e
                return $ y + h
          line <- gets (lineContents . listLine)
          case line of
            Just (pre, cur, aft) -> io $ do
              y' <- foldM drawElem (fi rectY)
                    $ map (T.unpack . fst) $ reverse pre
              y'' <- drawFocus y' $ T.unpack $ fst cur
              foldM_ drawElem y'' $ map (T.unpack . fst) aft
            Nothing -> return ()
