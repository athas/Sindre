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
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.X11
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  unportable
--
-- X11 backend for Sindre.
--
-----------------------------------------------------------------------------
module Sindre.X11( SindreX11M
                 , SindreX11Conf(sindreDisplay, sindreScreen, sindreVisualOpts)
                 , sindreX11
                 , xopt
                 , VisualOpts(..)
                 , visualOpts
                 , drawing
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

import System.Environment
import System.Exit
import System.IO
import System.Posix.Types
import System.Locale.SetLocale(setLocale, Category(..))

import Control.Concurrent
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Char hiding (Control)
import Data.Maybe
import Data.List
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

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
  type InitVal SindreX11M = Window
  
  initDrawing (orient, rootwr) = do
    SindreX11Conf{ sindreScreenSize=screen
                 , sindreDisplay=dpy
                 , sindreRoot=win } <- back ask
    orient' <- case orient of
      Just orient' -> maybe (fail $ "position '"
                             ++ show orient'
                             ++ "' for root window not known")
                      return (mold orient')
      Nothing -> return (AlignCenter, AlignCenter)
    io $ do _ <- mapRaised dpy win
            status <- grabInput dpy win
            unless (status == grabSuccess) $
              error "Could not establish keyboard grab"
    return $ do
      reqs <- compose rootwr
      reshape <- back $ asks sindreReshape
      let rect = adjustRect orient' screen $ fitRect screen reqs
      back $ reshape [rect]
      usage <- draw rootwr $ Just rect
      back $ reshape usage
      io $ sync dpy False
  
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

mkWindow :: Window -> Position
         -> Position -> Dimension -> Dimension -> SindreX11M Window
mkWindow rw x y w h = do
  dpy <- asks sindreDisplay
  s   <- asks sindreScreen
  let visual   = defaultVisualOfScreen s
      attrmask = cWBackPixel
  io $ allocaSetWindowAttributes $ \attrs -> do
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
  (str,keysym,event) <-
    allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- X.getEvent e
      (ks,s) <- if ev_event_type ev == keyPress
                then utf8LookupString ic e
                else return (Nothing, Nothing)
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
                 Just $ CharKey $ head $ keysymToString ks
               (c:_)  -> Just $ CharKey c
               _ -> Nothing
      where mods (CharKey c) = (Shift `S.delete` getModifiers m, CharKey c)
            mods (CtrlKey c) = (getModifiers m, CtrlKey c)
processX11Event (_, _, ExposeEvent { ev_count = 0, ev_window = _ }) =
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

-- | Get the 'Pixel' value for a named colour if it exists
maybeAllocColour :: Display -> String -> IO (Maybe Pixel)
maybeAllocColour dpy c = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  catch (Just . color_pixel . fst <$> allocNamedColor dpy colormap c)
    (const $ return Nothing)

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

sindreX11Cfg :: String -> IO SindreX11Conf
sindreX11Cfg dstr = do
  ret <- setLocale LC_ALL Nothing
  case ret of
    Nothing -> putStrLn "Can't set locale." >> exitFailure
    _       -> return ()
  sl <- supportsLocale
  unless sl $ putStrLn "Current locale is not supported" >> exitFailure
  _ <- setLocaleModifiers ""
  dpy <- setupDisplay dstr
  rmInitialize
  db <- rmGetStringDatabase $ resourceManagerString dpy
  let scr = defaultScreenOfDisplay dpy
  rect <- findRectangle dpy (rootWindowOfScreen scr)
  win <- mkUnmanagedWindow dpy scr (rootWindowOfScreen scr)
         (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)
  pm <- createPixmap dpy win (rect_width rect) (rect_height rect) 1
  shapeWindow dpy win pm (fromXRect rect) [Rectangle 0 0 0 0]
  im <- openIM dpy Nothing Nothing Nothing
  ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
  visopts <- defVisualOpts dpy
  evvar <- newEmptyMVar
  xlock <- newMVar ()
  _ <- forkIO $ eventReader dpy ic evvar xlock
  return SindreX11Conf
             { sindreDisplay = dpy
             , sindreScreen = scr
             , sindreRoot = win
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

-- | Execute Sindre in the X11 backend.
sindreX11 :: String -- ^ The display string (usually the value of the
                    -- environment variable @$DISPLAY@ or @:0@)
          -> (Window -> SindreX11M ExitCode) 
          -- ^ The function returned by
          -- 'Sindre.Compiler.compileSindre' after command line
          -- options have been given
          -> IO ExitCode
sindreX11 dstr start = do
  cfg <- sindreX11Cfg dstr
  runSindreX11 (lockX >> start (sindreRoot cfg)) cfg

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
                  `catch` (\_ -> putMVar linevar Nothing)
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
      maybe (badValue name') return =<< back (moldM $ string v')
    Just _ -> badValue name'

instance Param SindreX11M Pixel where
  moldM (mold -> Just c) = io . flip maybeAllocColour c =<< asks sindreDisplay
  moldM _ = return Nothing

instance Param SindreX11M FontStruct where
  moldM (mold -> Just s) = do
    dpy <- asks sindreDisplay
    io $ (Just <$> loadQueryFont dpy s) `catch` const (return Nothing)
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
drawing :: (a -> Window) -- ^ Window from widget state.
        -> (a -> VisualOpts) -- ^ Visual options from widget state.
        -> (Rectangle -> Drawer -> Drawer -> Drawer -> Drawer
            -> WidgetM a SindreX11M ())
        -- ^ The body of the @drawing@ call - this function is called
        -- with a rectangle representing the area of the widget, and
        -- 'Drawer's for "foreground," "background", "focus
        -- foreground", and "focus background" respectively.
        -> Maybe Rectangle -> WidgetM a SindreX11M SpaceUse
drawing wf optsf m r = do
  dpy <- back $ asks sindreDisplay
  win <- gets wf
  VisualOpts{..} <- gets optsf
  r' <- case r of
          Just r' -> do
            io $ moveResizeWindow dpy win
                   (fi $ rectX r') (fi $ rectY r')
                   (fi $ max 1 $ rectWidth r') (fi $ max 1 $ rectHeight r')
            return r'
          Nothing -> back $ windowSize win
  let mkgc fg bg = io $ do gc <- createGC dpy win
                           setForeground dpy gc fg
                           setBackground dpy gc bg
                           setFont dpy gc $ fontFromFontStruct font
                           return gc
  fggc <- mkgc foreground background
  bggc <- mkgc background foreground
  ffggc <- mkgc focusForeground focusBackground
  fbggc <- mkgc focusBackground focusForeground
  io $ fillRectangle dpy win bggc 0 0
         (fi $ rectWidth r') (fi $ rectHeight r')
  let pass :: GC -> Drawer
      pass gc f = f dpy win gc
  m r' (pass fggc) (pass bggc) (pass ffggc) (pass fbggc)
  io (mapM_ (freeGC dpy) [fggc, bggc, ffggc, fbggc] >> sync dpy False)
  return [r']

-- | A small function that automatically passes appropriate 'Display',
-- 'Window' and 'GC' values to an Xlib drawing function (that,
-- conveniently, always accepts these arguments in the same order).
type Drawer = forall f. ((Display -> Window -> GC -> f) -> f)

padding :: Integral a => a
padding = 2
                
data Dial = Dial { dialMax    :: Integer
                 , dialVal    :: Integer
                 , dialWin    :: Window
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
    composeI = return (Max 50, Max 50)
    drawI = drawing dialWin dialVisual $ \r fg _ _ _ -> do
      val    <- gets dialVal
      maxval <- gets dialMax
      io $ do
        let unitAng = 2*pi / fi maxval
            angle   = (-unitAng) * fi val :: Double
            dim     = min (rectWidth r) (rectHeight r) - 1
            cornerX = (rectWidth r - dim) `div` 2
            cornerY = (rectHeight r - dim) `div` 2
        fg drawArc (fi cornerX) (fi cornerY) (fi dim) (fi dim) 0 (360*64)
        fg fillArc (fi cornerX) (fi cornerY)
           (fi dim) (fi dim) (90*64) (round $ angle * (180/pi) * 64)
        fg drawRectangle (fi cornerX) (fi cornerY) (fi dim) (fi dim)

-- | A simple dial using an arc segment to indicate the value compared
-- to the max value.  Accepts @max@ and @value@ parameters (both
-- integers, default values 12 and 0), and a single field: @value@.
-- @<n>@ and @<p>@ are used to increase and decrease the value.
mkDial :: Constructor SindreX11M
mkDial w r [] = do
  maxv <- param "max" <|> return 12
  val <- param "value" <|> return 0
  visual <- visualOpts r
  sindre $ do
    win <- back $ mkWindow w 1 1 1 1
    back $ do dpy <- asks sindreDisplay
              io $ mapWindow dpy win
              io $ selectInput dpy win
                (keyPressMask .|. buttonReleaseMask)
    construct (Dial maxv val win visual, win)
mkDial _ _ _ = error "Dials do not have children"

data Label = Label { labelText :: String 
                   , labelWin :: Window
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
    drawI = drawing labelWin labelVisual $ \r fg _ _ _ -> do
      fstruct <- gets (font . labelVisual)
      label <- gets labelText
      io $ fg drawText 0 0 (fi $ rectHeight r) fstruct label

-- | Label displaying the text contained in the field @label@, which
-- is also accepted as a widget parameter (defaults to the empty
-- string).
mkLabel :: Constructor SindreX11M
mkLabel w r [] = do
  label <- param "label" <|> return ""
  win <- back $ mkWindow w 1 1 1 1
  visual <- visualOpts r
  back $ do dpy <- asks sindreDisplay
            io $ mapWindow dpy win
            io $ selectInput dpy win
              (keyPressMask .|. buttonReleaseMask)
  sindre $ construct (Label label win visual, win)
mkLabel _ _ _ = error "Labels do not have children"

data Blank = Blank { blankWin :: Window
                   , blankVisual :: VisualOpts }
                
instance Object SindreX11M Blank where
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI _   = return $ IntegerV 0

instance Widget SindreX11M Blank where
    composeI = return (Unlimited, Unlimited)
    drawI = drawing blankWin blankVisual $ \_ _ _ _ _ -> return ()

-- | A blank widget, showing only background colour, that can use as
-- much or as little room as necessary.  Useful for constraining the
-- layout of other widgets.
mkBlank :: Constructor SindreX11M
mkBlank w r [] = do
  win <- back $ mkWindow w 1 1 1 1
  visual <- visualOpts r
  back $ do dpy <- asks sindreDisplay
            io $ mapWindow dpy win
            io $ selectInput dpy win
              (keyPressMask .|. buttonReleaseMask)
  sindre $ construct (Blank win visual, win)
mkBlank _ _ _ = error "Blanks do not have children"

data TextField = TextField { fieldText :: String
                           , fieldPoint :: Int
                           , fieldWin :: Window
                           , fieldAlign :: Align
                           , fieldVisual :: VisualOpts }

instance Object SindreX11M TextField where
    fieldSetI "value" (mold -> Just v) = do
      modify $ \s -> s { fieldText = v, fieldPoint = length v }
      fullRedraw
      string <$> gets fieldText
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "value" = string <$> gets fieldText
    fieldGetI _       = return $ IntegerV 0
    
    recvEventI (KeyPress (S.toList -> [], CharKey c)) =
      changeFields [("value", unmold . fieldText)] $ \s -> do
        let (v, p) = (fieldText s, fieldPoint s)
        fullRedraw
        return s { fieldText = take p v ++ [c] ++ drop p v, fieldPoint = p+1 }
    recvEventI (KeyPress k) =
      maybe (return ()) (redraw >>) $ M.lookup k editorCommands
    recvEventI _ = return ()

editorCommands :: M.Map Chord (ObjectM TextField SindreX11M ())
editorCommands = M.fromList
  [ (chord [] "Right", movePoint 1)
  , (chord [Control] "f", movePoint 1)
  , (chord [] "Left", movePoint (-1))
  , (chord [Control] "b", movePoint (-1))
  , (chord [Control] 'a', gotoStart)
  , (chord [Control] 'e', gotoEnd)
  , (chord [] "Home", gotoStart)
  , (chord [] "End", gotoEnd)
  , (chord [Control] 'w', delWordBack)
  , (chord [Control] "BackSpace", delWordBack)
  , (chord [Meta] 'd', delWordFwd)    
  , (chord [Control] 'k', delForward $ const "")
  , (chord [Control] 'u', delBackward $ const "")
  , (chord [] "BackSpace", delBackward $ drop 1) 
  , (chord [Control] 'd', delForward $ drop 1)]
    where delWordBack = delBackward $ dropWhile isAlphaNum .
                        dropWhile (not . isAlphaNum)
          delWordFwd = delForward $ dropWhile isAlphaNum .
                        dropWhile (not . isAlphaNum)
          movePoint d = modify $ \s -> s
                        { fieldPoint = (fieldPoint s+d) `boundBy`
                                       fieldText s }
          delBackward delf = changeFields [("value", unmold . fieldText)] 
                             $ \s@TextField{..} -> do
            fullRedraw
            let text' = reverse $ delf $ reverse $ take fieldPoint fieldText
                point' = fieldPoint - length fieldText + length text'
            return s { fieldText = text' ++ drop fieldPoint fieldText
                     , fieldPoint = point' }
          delForward delf = changeFields [("value", unmold . fieldText)]
                            $ \s -> do
            fullRedraw
            let (v, p) = (fieldText s, fieldPoint s)
            return s { fieldText = take p v ++ delf (drop p v) }
          gotoStart = modify $ \s -> s { fieldPoint = 0 }
          gotoEnd   = modify $ \s -> s { fieldPoint = length $ fieldText s }

instance Widget SindreX11M TextField where
    composeI = do
      fstruct <- gets (font . fieldVisual)
      text <- gets fieldText
      return (Max $ fi (textWidth fstruct text) + padding * 2,
              Exact $ fi (textHeight fstruct text) + padding * 2)
    drawI = drawing fieldWin fieldVisual $ \Rectangle{..} fg _ _ _-> do
      text <- gets fieldText
      just <- gets fieldAlign
      p <- gets fieldPoint
      fstruct <- gets (font . fieldVisual)
      io $ do
        let (_, a, d, _) = textExtents fstruct text
            w = textWidth fstruct text
            w' = textWidth fstruct $ take p text
            x = align just 0 w (fi rectWidth - padding*2) + padding
            y = align AlignCenter 0 (a+d) (fi rectHeight - padding*2) + padding
            text' = if w <= fi rectWidth then text
                    else let fits = (<= fi rectWidth) . textWidth fstruct
                         in case filter fits $ tails $ reverse text of
                              []    -> ""
                              (t:_) -> reverse $ "..." ++ drop 3 t
        fg drawString x (a+y) text'
        fg drawLine (x+w') (y-padding) (x+w') (y+padding+a+d)

-- | Single-line text field, whose single field @value@ (also a
-- parameter, defaults to the empty string) is the contents of the
-- editing buffer.
mkTextField :: Constructor SindreX11M
mkTextField w r [] = do
  v <- param "value" <|> return ""
  win <- back $ mkWindow w 1 1 1 1
  visual <- visualOpts r
  back $ do dpy <- asks sindreDisplay
            io $ mapWindow dpy win
            io $ selectInput dpy win
              (keyPressMask .|. buttonReleaseMask)
  sindre $ construct (TextField v 0 win AlignNeg visual, win)
mkTextField _ _ _ = error "TextFields do not have children"

data List = List { listElems :: [T.Text]
                 , listWin :: Window
                 , listFilter :: T.Text
                 , listFiltered :: [T.Text]
                 , listSel :: Int
                 , listVisual :: VisualOpts
                 , listCompose :: WidgetM List SindreX11M SpaceNeed
                 , listDraw :: Maybe Rectangle
                            -> WidgetM List SindreX11M SpaceUse
                 , listFilterF :: T.Text -> [T.Text] -> [T.Text]
                 }

selection :: List -> Value
selection l = StringV $ case drop (listSel l) (listFiltered l) of
  []    -> T.empty
  (e:_) -> e

refilter :: (T.Text -> T.Text) -> T.Text -> [T.Text] -> [T.Text]
refilter tr f ts = exacts++prefixes++infixes
  where (exacts, nonexacts) = partition (==f') $ map tr ts
        (prefixes, nonprefixes) = partition (T.isPrefixOf f') nonexacts
        (infixes, _) = partition (T.isInfixOf f') nonprefixes
        f' = tr f

methInsert :: T.Text -> ObjectM List SindreX11M ()
methInsert vs = do
  modify $ \s -> s { listElems = listElems s ++ lines'
                   , listFiltered =
                     listFilterF s (listFilter s) (listFiltered s ++ lines') }
  fullRedraw
   where lines' = T.lines vs

methClear :: ObjectM List SindreX11M ()
methClear = do
  modify $ \s -> s { listElems = [] , listFiltered = [] , listSel = 0 }
  fullRedraw

boundBy :: Int -> [a] -> Int
boundBy x _ | x <= 0 = 0
boundBy _ []  = 0
boundBy _ [_] = 0
boundBy x (_:es) = 1 + (x-1) `boundBy` es

methFilter :: String -> ObjectM List SindreX11M ()
methFilter f =
  changeFields [("selected", selection)] $ \s -> do
    let v = listFilterF s f' $ if listFilter s `T.isPrefixOf` f'
                               then listFiltered s
                               else listElems s
    redraw >> return s { listFilter = f'
                       , listFiltered = v
                       , listSel = listSel s `boundBy` v }
    where f' = T.pack f

methNext :: ObjectM List SindreX11M ()
methPrev :: ObjectM List SindreX11M ()
(methNext, methPrev) = (move 1, move (-1))
    where move d = changeFields [("selected", selection)] $ \s -> do
                     redraw
                     return s { listSel = (listSel s + d)
                                          `boundBy` listFiltered s }

instance Object SindreX11M List where
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "selected" = selection <$> get
    fieldGetI _ = return $ IntegerV 0
    callMethodI "insert" = function methInsert
    callMethodI "clear"  = function methClear
    callMethodI "filter" = function methFilter
    callMethodI "next" = function methNext
    callMethodI "prev" = function methPrev
    callMethodI m = fail $ "Unknown method '" ++ m ++ "'"

composeHoriz :: WidgetM List SindreX11M SpaceNeed
composeHoriz = do fstruct <- gets (font . listVisual)
                  let h = ascentFromFontStruct fstruct +
                          descentFromFontStruct fstruct
                  return (Unlimited, Exact $ fi h + padding * 2)

drawHoriz :: Maybe Rectangle -> WidgetM List SindreX11M SpaceUse
drawHoriz = drawing listWin listVisual $ \r fg _ ffg fbg -> do
  fstruct <- gets (font . listVisual)
  elems <- map T.unpack <$> gets listFiltered
  sel <- gets listSel
  let prestr  = "< "
      nextstr = " > "
      spacing = textWidth fstruct " "
      prew   = textWidth fstruct prestr
      nextw  = textWidth fstruct nextstr
      h      = fi $ rectHeight r
      elines = zip [0..] $ elemLines $ zip [0..] elems
      elemLine [] _ = ([], [])
      elemLine es@((i, e):es') left =
        case textWidth fstruct e of
          w | left >= w+2*spacing ->
            let (es'', rest) = elemLine es' (left-w-2*spacing)
            in ((i, (drawer, w)):es'', rest)
            where drawer x = if i == sel then
                               do fbg fillRectangle x 0 (fi w+2*fi spacing) h
                                  ffg drawText (x+spacing) 0 h fstruct e
                             else fg drawText (x+spacing) 0 h fstruct e
          _ | otherwise -> ([], es)
      elemLines es = case elemLine es $
                          fi (rectWidth r)-nextw-prew of
                            (es', [])   -> [es']
                            ([], _)     -> []
                            (es', rest) -> es':elemLines rest
  case find (elem sel . map fst . snd) elines of
    Just (i, line) -> io $ do
      foldM_ draw' prew $ map snd line
      when (i /= 0) $
        fg drawText 0 0 h fstruct prestr
      unless (null $ drop i elines) $
        fg drawText (fi (rectWidth r) - nextw) 0 h fstruct nextstr
      where draw' x (drawer, w) = drawer x >> return (x+w+2*spacing)
    Nothing   -> return ()

composeVert :: Integer -> WidgetM List SindreX11M SpaceNeed
composeVert n = do fstruct <- gets (font . listVisual)
                   let h = ascentFromFontStruct fstruct +
                           descentFromFontStruct fstruct
                   return (Unlimited, Exact $ (fi h + padding * 2) * n)

drawVert :: Integer -> Maybe Rectangle -> WidgetM List SindreX11M SpaceUse
drawVert n = drawing listWin listVisual $ \r fg _ ffg fbg -> do
  fstruct <- gets (font . listVisual)
  elems <- map T.unpack <$> gets listFiltered
  sel <- gets listSel
  let h = ascentFromFontStruct fstruct +
          descentFromFontStruct fstruct
      elems' = take (fi n) $
               drop ((sel `div` fi n) * fi n) $
               zip [0..] elems
  io $ do
    let printElems [] _ = return ()
        printElems ((i, e):es) y = do
          if i == sel then
            do fbg fillRectangle 0 y (fi $ rectWidth r) (fi h+2*padding)
               ffg drawText padding (y+padding) (fi h) fstruct e
            else fg drawText padding (y+padding) (fi h) fstruct e
          printElems es (y+h+2*padding)
    printElems elems' 0

instance Widget SindreX11M List where
    composeI = join $ gets listCompose
    drawI r = join $ gets listDraw <*> pure r

mkList :: WidgetM List SindreX11M SpaceNeed
       -> (Maybe Rectangle -> WidgetM List SindreX11M SpaceUse)
       -> Constructor SindreX11M
mkList cf df w r [] = do
  win <- back $ mkWindow w 1 1 1 1
  visual <- visualOpts r
  insensitive <- param "i" <|> return False
  let trf = if insensitive then T.toCaseFold else id
  back $ do dpy <- asks sindreDisplay
            io $ mapWindow dpy win
            io $ selectInput dpy win
              (keyPressMask .|. buttonReleaseMask)
  sindre $ construct (List [] win T.empty [] 0 visual cf df (refilter trf),
                      win)
mkList _ _ _ _ _ = error "Lists do not have children"

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
-- The field @selected@ is the selected element.
mkHList :: Constructor SindreX11M
mkHList = mkList composeHoriz drawHoriz

-- | As 'mkHList', except the list is vertical.  The parameter @lines@
-- (default value 10) is the number of lines shown.
mkVList :: Constructor SindreX11M
mkVList w k cs = do
  n <- param "lines" <|> return 10
  mkList (composeVert n) (drawVert n) w k cs
