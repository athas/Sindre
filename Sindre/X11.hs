{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
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
               , InitSindreX11M
               , runInitSindreX11
               , sindreX11
               , mkDial )
    where

import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime
import Sindre.Util

import Codec.Binary.UTF8.String (decodeString)
import Graphics.X11.Xlib hiding ( refreshKeyboardMapping
                                , Rectangle )
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras hiding (Event, getEvent)
import qualified Graphics.X11.Xlib.Extras as X
import Graphics.X11.Xinerama
import Graphics.X11.Xshape

import System.Exit

import Control.Concurrent
import Control.Applicative
import "monads-fd" Control.Monad.Reader
import "monads-fd" Control.Monad.State
import Data.Bits
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

toXRect :: Rectangle -> X.Rectangle
toXRect (Rectangle (x1, y1) w h) =
  X.Rectangle { rect_x = fi x1
              , rect_y = fi y1
              , rect_width = fi w
              , rect_height = fi h }
  
fromXRect :: X.Rectangle -> Rectangle
fromXRect r =
    Rectangle { rectCorner = (fi $ rect_x r, fi $ rect_y r)
              , rectWidth = fi $ rect_width r
              , rectHeight = fi $ rect_height r }

data SindreX11Conf = SindreX11Conf {
      sindreDisplay    :: Display
    , sindreScreen     :: Screen
    , sindreRoot       :: Window
    , sindreScreenSize :: Rectangle
    }

newtype InitSindreX11M a = InitSindreX11M (ReaderT SindreX11Conf IO a)
  deriving (Functor, Monad, MonadIO, MonadReader SindreX11Conf)

runInitSindreX11 :: InitSindreX11M a -> SindreX11Conf -> IO a
runInitSindreX11 (InitSindreX11M m) = runReaderT m

newtype SindreX11M a = SindreX11M (ReaderT SindreX11Conf (StateT (SindreState SindreX11M) IO) a)
  deriving (Functor, Monad, MonadIO, MonadReader SindreX11Conf,
            MonadState (SindreState SindreX11M), Applicative)

runSindreX11 :: SindreX11M a -> SindreX11Conf -> SindreState SindreX11M -> IO a
runSindreX11 (SindreX11M m) cfg state = evalStateT (runReaderT m cfg) state

instance MonadSindre SindreX11M where
  type SubCfg SindreX11M = SindreX11Conf
  type SubEvent SindreX11M = (KeySym, String, X.Event)
  type InitM SindreX11M = InitSindreX11M
  type InitVal SindreX11M = Window
  
  fullRedraw = do
    screen <- asks sindreScreenSize
    root <- asks sindreRoot
    dpy  <- asks sindreDisplay
    (usage, _) <- draw rootWidget screen
    io $ do
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
    return ()
  
  getEvent = do
    xev <- getX11Event
    ev  <- processX11Event xev
    maybe getEvent return ev
  
  printVal = io . putStr
  
  quit = io . exitWith

getX11Event :: SindreX11M (KeySym, String, X.Event)
getX11Event = do
  dpy <- asks sindreDisplay
  (keysym,string,event) <- do
    io $ allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- X.getEvent e
      (ks,s) <- if ev_event_type ev == keyPress
                then lookupString $ asKeyEvent e
                else return (Nothing, "")
      return (ks,decodeString s,ev)
  return (fromMaybe xK_VoidSymbol keysym, string, event)

processX11Event :: (KeySym, String, X.Event) -> SindreX11M (Maybe Event)
processX11Event (ks, s, KeyEvent {ev_event_type = t, ev_state = m })
    | t == keyPress = do
  return $ Just $ KeyPress (S.empty, CharacterKey s)
processX11Event (_, _, ExposeEvent { ev_count = 0 }) = do
  fullRedraw
  return Nothing
processX11Event  _ = return Nothing

mkWindow :: Window -> Position
         -> Position -> Dimension -> Dimension -> InitSindreX11M Window
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
                 
windowSize :: Window -> SindreX11M Rectangle
windowSize win = do
  dpy <- asks sindreDisplay
  (_, x, y, w, h, _, _) <- io $ getGeometry dpy win
  return $ Rectangle (fi x, fi y) (fi w) (fi h)

setupDisplay :: String -> IO Display
setupDisplay dstr = do
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

sindreX11Cfg :: String -> IO SindreX11Conf
sindreX11Cfg dstr = do
  dpy <- setupDisplay dstr
  let scr = defaultScreenOfDisplay dpy
  rect <- findRectangle dpy (rootWindowOfScreen scr)
  win <- mkUnmanagedWindow dpy scr (rootWindowOfScreen scr)
         (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)
  _ <- mapRaised dpy win
  status <- io $ grabInput dpy win
  unless (status == grabSuccess) $
    error "Could not establish keyboard grab"
  return $ SindreX11Conf { sindreDisplay = dpy
                         , sindreScreen = scr
                         , sindreRoot = win 
                         , sindreScreenSize = fromXRect rect }

sindreX11 :: Program -> ClassMap SindreX11M -> String -> IO ()
sindreX11 prog cm dstr = do
  cfg <- sindreX11Cfg dstr
  case compileSindre prog cm (sindreRoot cfg) of
    Left s -> error s
    Right (statem, m) -> do
      state <- runInitSindreX11 statem cfg
      runSindreX11 m cfg state
                
data Dial = Dial { dialMax :: Integer
                 , dialVal :: Integer
                 , dialWin :: Window
                 }

instance Object SindreX11M Dial where
    fieldSet dial "value" (IntegerV v) = do
      return dial'
        where dial' = dial { dialVal = clamp 0 v (dialMax dial) }
    fieldSet dial _ _                  = return dial
    fieldGet dial "value" = return $ IntegerV $ dialVal dial
    fieldGet _    _       = return $ IntegerV 0

instance Widget SindreX11M Dial where
    compose _ = return
    draw dial r = do
      dpy <- asks sindreDisplay
      scr <- asks sindreScreen
      io $ do
        moveResizeWindow dpy (dialWin dial)
          (fi $ fst $ rectCorner r) (fi $ snd $ rectCorner r)
          (fi $ rectWidth r) (fi $ rectHeight r)
        gc <- createGC dpy $ dialWin dial
        setForeground dpy gc $ whitePixelOfScreen scr
        setBackground dpy gc $ blackPixelOfScreen scr
        fillRectangle dpy (dialWin dial) gc
                  0 0 (fi $ rectWidth r) (fi $ rectHeight r)
        setForeground dpy gc $ blackPixelOfScreen scr
        setBackground dpy gc $ whitePixelOfScreen scr
        drawArc dpy (dialWin dial) gc (fi cornerX) (fi cornerY) 
          (fi $ dim) (fi dim) 0 (360*64)
        fillArc dpy (dialWin dial) gc (fi cornerX) (fi cornerY) 
          (fi $ dim) (fi dim) (90*64) (round $ angle * (180/pi) * 64)
        drawRectangle dpy (dialWin dial) gc
                  (fi cornerX) (fi cornerY)
                  (fi dim) (fi dim)
        drawString dpy (dialWin dial) gc
                       (fi $ cornerX + dim `div` 2)
                       (fi $ cornerY + dim `div` 4)
                       (show (dialVal dial) ++ "/" ++ show (dialMax dial))
        freeGC dpy gc
      return ([r], dial)
      where dim = min (rectWidth r) (rectHeight r)
            cornerX = (rectWidth r - dim) `div` 2
            cornerY = (rectHeight r - dim) `div` 2
            unitAng = 2*pi / fi (dialMax dial)
            angle :: Double
            angle   = (-unitAng) * fi (dialVal dial)

mkDial' :: Window -> Integer -> Construction SindreX11M
mkDial' w maxv = do
  dpy <- asks sindreDisplay
  win <- mkWindow w 1 1 1 1
  io $ mapWindow dpy win
  io $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
  construct (Dial maxv 0 win, win)

mkDial :: Constructor SindreX11M
mkDial w m [] | [("max", IntegerV maxv)] <- M.toList m =
  mkDial' w maxv
mkDial w m [] | m == M.empty = mkDial' w 12
mkDial _ _ [] = error "Dials take at most one integer argument"
mkDial _ m _ | m /= M.empty = error "Dials do not have children"
mkDial _ _ _ = error "Invalid initial argument"
