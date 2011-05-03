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
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- X11 backend for Sindre.
--
-----------------------------------------------------------------------------

module Sindre.X11( SindreX11M
                 , SindreX11Conf(..)
                 , runSindreX11
                 , sindreX11
                 , mkDial
                 , mkLabel
                 , mkTextField
                 , mkInStream
                 , mkList
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
import Data.Char(isPrint)
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

fromXRect :: X.Rectangle -> Rectangle
fromXRect r =
    Rectangle { rectX = fi $ rect_x r
              , rectY = fi $ rect_y r
              , rectWidth = fi $ rect_width r
              , rectHeight = fi $ rect_height r }

type EventThunk = Sindre SindreX11M (Maybe (EventSource, Event))

data SindreX11Conf = SindreX11Conf {
      sindreDisplay    :: Display
    , sindreScreen     :: Screen
    , sindreRoot       :: Window
    , sindreScreenSize :: Rectangle
    , sindreVisualOpts :: VisualOpts
    , sindreRMDB       :: RMDatabase
    , sindreXlock      :: Xlock
    , sindreEvtVar     :: MVar EventThunk
    , sindreReshape    :: [Rectangle] -> SindreX11M ()
    , sindreRootWidget :: ((Align, Align), WidgetRef)
    }

newtype SindreX11M a = SindreX11M (ReaderT SindreX11Conf IO a)
  deriving (Functor, Monad, MonadIO, MonadReader SindreX11Conf, Applicative)

runSindreX11 :: SindreX11M a -> SindreX11Conf -> IO a
runSindreX11 (SindreX11M m) = runReaderT m

instance MonadBackend SindreX11M where
  type BackEvent SindreX11M = (KeySym, String, X.Event)
  type InitVal SindreX11M = Window
  
  redrawRoot = do
    (orient, rootwr) <- back $ asks sindreRootWidget
    screen <- back $ asks sindreScreenSize
    dpy  <- back $ asks sindreDisplay
    reqs <- compose rootwr
    reshape <- back $ asks sindreReshape
    let rect = adjustRect orient screen $ fitRect screen reqs
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
  
  getBackEvent =
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
    | t == keyPress =
      return $ ((BackendSrc,) . KeyPress . mods) <$>
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

sindreX11Cfg :: String -> (Maybe Value, WidgetRef) -> IO SindreX11Conf
sindreX11Cfg dstr (orient, root) = do
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
  _ <- mapRaised dpy win
  status <- grabInput dpy win
  unless (status == grabSuccess) $
    error "Could not establish keyboard grab"
  im <- openIM dpy Nothing Nothing Nothing
  ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
  visopts <- defVisualOpts dpy
  evvar <- newEmptyMVar
  xlock <- newMVar ()
  _ <- forkIO $ eventReader dpy ic evvar xlock
  orient' <- case orient of
               Just orient' -> maybe (fail $ "position '"
                                      ++ show orient'
                                      ++ "' for root window not known")
                                   return (mold orient')
               Nothing -> return (AlignCenter, AlignCenter)
  return SindreX11Conf
             { sindreDisplay = dpy
             , sindreScreen = scr
             , sindreRoot = win
             , sindreScreenSize = fromXRect rect
             , sindreVisualOpts = visopts
             , sindreRMDB = db
             , sindreEvtVar = evvar
             , sindreXlock = xlock
             , sindreReshape = io . shapeWindow dpy win pm (fromXRect rect)
             , sindreRootWidget = (orient', root) }

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

functions :: FuncMap SindreX11M
functions = stdFunctions `M.union` ioFunctions

sindreX11 :: Program -> ClassMap SindreX11M -> ObjectMap SindreX11M 
          -> String -> ( [SindreOption]
                       , Arguments -> IO ExitCode)
sindreX11 prog cm om dstr =
  case compileSindre prog cm om functions of
    Left s -> error s
    Right (opts, prog', rootw) ->
      let m args = do
            cfg <- sindreX11Cfg dstr rootw
            runSindreX11 (lockX >> prog' args (sindreRoot cfg)) cfg
      in (opts, m)

data InStream = InStream Handle

instance (MonadIO m, MonadBackend m) => Object m InStream where

mkInStream :: Handle -> ObjectRef -> SindreX11M (NewObject SindreX11M)
mkInStream h r = do evvar <- asks sindreEvtVar
                    _ <- io $ forkIO $ getHandleEvent evvar
                    return $ NewObject $ InStream h
    where getHandleEvent evvar = do
            d <- io $ hGetContents h
            mapM_ (putEv . NamedEvent "line" . (:[]) . StringV) $ lines d
            putEv $ NamedEvent "contents" [StringV d]
            putEv $ NamedEvent "eof" []
              where putEv ev = putMVar evvar $
                                 return $ Just (ObjectSrc r, ev)

xopt :: Param SindreX11M a => Maybe String -> String -> String 
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
      maybe (badValue name') return =<< back (moldM $ StringV v')
    Just _ -> badValue name'

instance Param SindreX11M Pixel where
  moldM (mold -> Just c) = io . flip maybeAllocColour c =<< asks sindreDisplay
  moldM _ = return Nothing

visualOpts :: Maybe String -> String -> ConstructorM SindreX11M VisualOpts
visualOpts name clss = do
  VisualOpts {..} <- back $ asks sindreVisualOpts
  flipcol <- param "highlight" <|> return False
  let pert = if flipcol then flip (,) else (,)
      (fgs, ffgs) = pert ("foreground", foreground)
                         ("focusForeground", focusForeground)
      (bgs, fbgs) = pert ("background", background)
                         ("focusBackground", focusBackground)
  fg <- paramM "fg" <|> xopt name clss (fst fgs) <|> pure (snd fgs)
  bg <- paramM "bg" <|> xopt name clss (fst bgs) <|> pure (snd bgs)
  ffg <- paramM "ffg" <|> xopt name clss (fst ffgs) <|> pure (snd ffgs)
  fbg <- paramM "fbg" <|> xopt name clss (fst fbgs) <|> pure (snd fbgs)
  return VisualOpts { foreground = fg, background = bg,
                      focusForeground = ffg, focusBackground = fbg,
                      font = font }

drawing :: (a -> Window) -> (a -> VisualOpts)
        -> (Rectangle -> Drawer -> Drawer -> Drawer -> Drawer
            -> WidgetM a SindreX11M ())
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
      changeFields ["value"] [unmold . dialVal] $ \s -> do
        redraw
        return s { dialVal = clamp 0 (dialVal s+1) (dialMax s) }
    recvEventI (KeyPress (_, CharKey 'p')) =
      changeFields ["value"] [unmold . dialVal] $ \s -> do
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

mkDial :: Constructor SindreX11M
mkDial w k [] = constructing $ do
  maxv <- param "max" <|> return 12
  visual <- visualOpts k "Dial"
  sindre $ do
    win <- back $ mkWindow w 1 1 1 1
    back $ do dpy <- asks sindreDisplay
              io $ mapWindow dpy win
              io $ selectInput dpy win
                (keyPressMask .|. buttonReleaseMask)
    construct (Dial maxv 0 win visual, win)
mkDial _ _ _ = error "Dials do not have children"

data Label = Label { labelText :: String 
                   , labelWin :: Window
                   , labelAlign :: Align
                   , labelVisual :: VisualOpts
                   }

instance Object SindreX11M Label where
    fieldSetI "label" v = do
      modify $ \s -> s { labelText = fromMaybe "" $ mold v }
      StringV <$> gets labelText
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "label" = StringV <$> gets labelText
    fieldGetI _       = return $ IntegerV 0

instance Widget SindreX11M Label where
    composeI = do
      fstruct <- gets (font . labelVisual)
      text <- gets labelText
      let (_, a, d, _) = textExtents fstruct text
      case text of
        "" -> return (Exact 0, Max 0)
        _  -> return (Exact $ fi (textWidth fstruct text) + padding * 2,
                      Exact $ fi (a+d) + padding * 2)
    drawI = drawing labelWin labelVisual $ \r fg _ _ _ -> do
      fstruct <- gets (font . labelVisual)
      label <- gets labelText
      just <- gets labelAlign
      io $ do
        let (_, a, d, _) = textExtents fstruct label
            w = textWidth fstruct label
            h = a+d
        fg drawString
               (align just 0 w (fi $ rectWidth r))
               (a + align AlignCenter 0 h (fi $ rectHeight r))
               label

mkLabel :: Constructor SindreX11M
mkLabel w k [] = constructing $ do
  label <- param "label" <|> return ""
  win <- back $ mkWindow w 1 1 1 1
  visual <- visualOpts k "Label"
  back $ do dpy <- asks sindreDisplay
            io $ mapWindow dpy win
            io $ selectInput dpy win
              (keyPressMask .|. buttonReleaseMask)
  sindre $ construct (Label label win AlignCenter visual, win)
mkLabel _ _ _ = error "Labels do not have children"
                
data TextField = TextField { fieldText :: String
                           , fieldPoint :: Int
                           , fieldWin :: Window
                           , fieldAlign :: Align
                           , fieldVisual :: VisualOpts }

instance Object SindreX11M TextField where
    fieldSetI "value" (mold -> Just v) = do
      modify $ \s -> s { fieldText = v, fieldPoint = length v }
      fullRedraw
      StringV <$> gets fieldText
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "value" = StringV <$> gets fieldText
    fieldGetI _       = return $ IntegerV 0
    
    recvEventI (KeyPress (S.toList -> [], CharKey c)) =
      changeFields ["value"] [unmold . fieldText] $ \s -> do
        let (v, p) = (fieldText s, fieldPoint s)
        fullRedraw
        return s { fieldText = take p v ++ [c] ++ drop p v, fieldPoint = p+1 }
    recvEventI (KeyPress (S.toList -> [], CtrlKey "BackSpace")) =
      changeFields ["value"] [unmold . fieldText] $ \s -> do
        let (v, p) = (fieldText s, fieldPoint s)
        fullRedraw
        case p of 0 -> return s
                  _ -> return s { fieldText = take (p-1) v ++ drop p v
                                , fieldPoint = p-1 }
    recvEventI (KeyPress (S.toList -> [], CtrlKey "Right")) =
      fullRedraw >> movePoint 1
    recvEventI (KeyPress (S.toList -> [], CtrlKey "Left")) =
      fullRedraw >> movePoint (-1)
    recvEventI (KeyPress (S.toList -> [Control], CharKey 'w')) =
      changeFields ["value"] [unmold . fieldText] $ \s -> do
        fullRedraw
        return s { fieldText = "", fieldPoint = 0 }
    recvEventI _ = return ()

instance Widget SindreX11M TextField where
    composeI = do
      fstruct <- gets (font . fieldVisual)
      text <- gets fieldText
      let (_, a, d, _) = textExtents fstruct text
      return (Max $ fi (textWidth fstruct text) + padding * 2,
              Max $ fi (a+d) + padding * 2)
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
        err $ show (w, rectWidth)
        fg drawString x (a+y) text'
        fg drawLine (x+w') (y-padding) (x+w') (y+padding+a+d)

movePoint :: Int -> ObjectM TextField m ()
movePoint d = do ep <- gets fieldPoint
                 n <- length <$> gets fieldText
                 modify $ \s -> s { fieldPoint = clamp 0 (ep+d) n }

mkTextField :: Constructor SindreX11M
mkTextField w k [] = constructing $ do
  v <- param "value" <|> return ""
  win <- back $ mkWindow w 1 1 1 1
  visual <- visualOpts k "TextField"
  back $ do dpy <- asks sindreDisplay
            io $ mapWindow dpy win
            io $ selectInput dpy win
              (keyPressMask .|. buttonReleaseMask)
  sindre $ construct (TextField v 0 win AlignNeg visual, win)
mkTextField _ _ _ = error "TextFields do not have children"

data List = List { listElems :: [String] 
                 , listWin :: Window
                 , listFilter :: String
                 , listFiltered :: [String]
                 , listSel :: Int 
                 , listVisual :: VisualOpts }

selection :: List -> Value
selection l =
  StringV $ case drop (listSel l) (listFiltered l) of
              []    -> ""
              (e:_) -> e

methInsert :: String -> ObjectM List SindreX11M ()
methInsert vs = do
  forM_ (lines vs) $ \v ->
    modify $ \s -> s { listElems = v `insert` listElems s 
                     , listFiltered = if listFilter s `isInfixOf` v
                                      then v `insert` listFiltered s
                                      else listFiltered s }
  fullRedraw

methFilter :: String -> ObjectM List SindreX11M ()
methFilter f =
  changeFields ["selected"] [selection] $ \s -> do
    let v = filter (isInfixOf f) (listElems s)
    redraw >> return s { listFilter = f
                       , listFiltered = v
                       , listSel = clamp 0 (listSel s) $ length v - 1 }

methNext :: ObjectM List SindreX11M ()
methPrev :: ObjectM List SindreX11M ()
(methNext, methPrev) = (move 1, move (-1))
    where move d = changeFields ["selected"] [selection] $ \s -> do
                     redraw
                     return s { listSel = clamp 0 (listSel s + d)
                                          $ length (listFiltered s) - 1 }

instance Object SindreX11M List where
    fieldSetI _ _ = return $ IntegerV 0
    fieldGetI "selected" = selection <$> get
    fieldGetI _ = return $ IntegerV 0
    callMethodI "insert" = function methInsert
    callMethodI "filter" = function methFilter
    callMethodI "next" = function methNext
    callMethodI "prev" = function methPrev
    callMethodI m = fail $ "Unknown method '" ++ m ++ "'"

spacing :: Integral a => a
spacing = 5

instance Widget SindreX11M List where
    composeI = do
      fstruct <- gets (font . listVisual)
      let h = ascentFromFontStruct fstruct +
              descentFromFontStruct fstruct
      return (Unlimited, Min $ fi h + padding * 2)
    drawI = drawing listWin listVisual $ \r fg _ ffg fbg -> do
      fstruct <- gets (font . listVisual)
      elems <- gets listFiltered
      sel <- gets listSel
      io $ do
        let printElems [] _ _ = return ()
            printElems ((i, e):es) x left = do
              let (_, a, d, _) = textExtents fstruct e
                  w = textWidth fstruct e
                  y = align AlignCenter 0 (a+d)
                      (fi (rectHeight r) - padding*2) + padding
              when (left >= w) $ do
                if i == sel then
                  do fbg fillRectangle x 0 (fi w+2*spacing) (fi $ rectHeight r)
                     ffg drawString (x+spacing) (y+a) e
                  else fg drawString (x+spacing) (y+a) e
                printElems es (x+w+2*spacing) $ left - w - spacing
        printElems (zip [0..] elems) 0 $ fi $ rectWidth r

mkList :: Constructor SindreX11M
mkList w k [] = constructing $ do
  win <- back $ mkWindow w 1 1 1 1
  visual <- visualOpts k "List"
  back $ do dpy <- asks sindreDisplay
            io $ mapWindow dpy win
            io $ selectInput dpy win
              (keyPressMask .|. buttonReleaseMask)
  sindre $ construct (List [] win "" [] 0 visual, win)
mkList _ _ _ = error "Lists do not have children"
