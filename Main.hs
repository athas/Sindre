{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- visp, a programming language for writing simple GUIs
--
-----------------------------------------------------------------------------

module Main(main)
    where

import Visp.Compiler
import Visp.Parser
import Visp.Visp
import Visp.Util

import Codec.Binary.UTF8.String (decodeString)
import Graphics.X11.Xlib hiding ( refreshKeyboardMapping
                                , Rectangle)
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras hiding (Event)
import qualified Graphics.X11.Xlib.Extras as X
import Graphics.X11.Xinerama

import System.Environment

import Control.Applicative
import "monads-fd" Control.Monad.Reader
import "monads-fd" Control.Monad.State
import Data.Bits
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  dpy <- setupDisplay dstr
  let scr = defaultScreenOfDisplay dpy
  rect <- findRectangle dpy (rootWindowOfScreen scr)
  win <- mkUnmanagedWindow dpy scr (rootWindowOfScreen scr)
         (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)
  _ <- mapRaised dpy win
  let cfg = VispConf { vispDisplay = dpy
                     , vispScreen = scr
                     , vispRoot = win }
  runVisp cfg dialProgram
  
setupDisplay :: String -> IO Display
setupDisplay dstr =
  openDisplay dstr `Prelude.catch` \_ ->
    error $ "Cannot open display \"" ++ dstr ++ "\"."

data VispConf = VispConf {
      vispDisplay  :: Display
    , vispScreen   :: Screen
    , vispRoot     :: Window
    }

findRectangle :: Display -> Window -> IO X.Rectangle
findRectangle dpy rootw = do
  (_, _, _, x, y, _, _, _) <- queryPointer dpy rootw
  let hasPointer rect = fi x >= rect_x rect &&
                        fi (rect_width rect) + rect_x rect > fi x &&
                        fi y >= rect_y rect &&
                        fi (rect_height rect) + rect_y rect > fi y
  fromJust <$> find hasPointer <$> getScreenInfo dpy

runVisp :: VispConf -> Program -> IO ()
runVisp cfg prog = case compileVisp prog classMap (vispRoot cfg) of
                     Left s -> error s
                     Right (statem, m) -> do state <- runInitVispM statem cfg
                                             runVispM m cfg state

newtype InitVispM a = InitVispM (ReaderT VispConf IO a)
  deriving (Functor, Monad, MonadIO, MonadReader VispConf)

runInitVispM :: InitVispM a -> VispConf -> IO a
runInitVispM (InitVispM m) = runReaderT m

newtype VispM a = VispM (ReaderT VispConf (StateT (VispState VispM) IO) a)
  deriving (Functor, Monad, MonadIO, MonadReader VispConf,
            MonadState (VispState VispM), Applicative)

instance MonadVisp VispM where
  type SubCfg VispM = VispConf
  type SubEvent VispM = (KeySym, String, X.Event)
  type InitM VispM = InitVispM
  type InitVal VispM = Window
  
  rootRectangle = do
    dpy <- asks vispDisplay
    root <- asks vispRoot
    (_, x, y, w, h, _, _) <- io $ getGeometry dpy root 
    return $ Rectangle (fi x, fi y) (fi w) (fi h)

  getSubEvent = do
    dpy <- asks vispDisplay
    (keysym,string,event) <- do
      io $ allocaXEvent $ \e -> do
        nextEvent dpy e
        ev <- getEvent e
        (ks,s) <- if ev_event_type ev == keyPress
                  then lookupString $ asKeyEvent e
                  else return (Nothing, "")
        return (ks,decodeString s,ev)
    return (fromMaybe xK_VoidSymbol keysym, string, event)

runVispM :: VispM a -> VispConf -> VispState VispM -> IO a
runVispM (VispM m) cfg state = evalStateT (runReaderT m cfg) state

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

mkWindow :: Window -> Position
         -> Position -> Dimension -> Dimension -> InitVispM Window
mkWindow rw x y w h = do
  dpy <- asks vispDisplay
  s   <- asks vispScreen
  let visual   = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
      black    = blackPixelOfScreen s
      white    = whitePixelOfScreen s
  io $ allocaSetWindowAttributes $ \attrs -> do
    set_background_pixel attrs white
    set_border_pixel attrs black
    createWindow dpy rw x y w h 0 copyFromParent
                 inputOutput visual attrmask attrs

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

classMap :: ClassMap VispM
classMap = M.fromList [ ("Dial", mkDial)
                      , ("Horizontally", horizontally) 
                      , ("Vertically", vertically)
                      ]

data Dial = Dial { dialMax :: Integer
                 , dialVal :: Integer
                 , dialWin :: Window
                 }

instance Object VispM Dial where
    fieldSet dial "value" (IntegerV v) = return $ dial { dialVal = v }
    fieldSet dial _ _                  = return dial
    fieldGet dial "value" = return $ IntegerV $ dialVal dial
    fieldGet _    _       = return $ IntegerV 0

instance Widget VispM Dial where
    compose _ = return (Rectangle (0,0) 4 5)
    draw dial r = do
      io $ putStrLn $ "Dial at " ++ show r
      dpy <- asks vispDisplay
      scr <- asks vispScreen
      io $ do
        moveResizeWindow dpy (dialWin dial)
                  (fi $ fst $ rectCorner r) (fi $ snd $ rectCorner r)
                  (fi $ rectWidth r) (fi $ rectHeight r)
        raiseWindow dpy (dialWin dial)
        gc <- createGC dpy $ dialWin dial
        setForeground dpy gc $ whitePixelOfScreen scr
        setBackground dpy gc $ blackPixelOfScreen scr
        fillRectangle dpy (dialWin dial) gc
                  0 0
                  (fi $ rectWidth r) (fi $ rectHeight r)
        setForeground dpy gc $ blackPixelOfScreen scr
        setBackground dpy gc $ whitePixelOfScreen scr
        drawLine dpy (dialWin dial) gc 0 0 (fi $ rectWidth r) (fi $ rectHeight r)
        drawLine dpy (dialWin dial) gc (fi $ rectWidth r) 0 0 (fi $ rectHeight r)
        drawRectangle dpy (dialWin dial) gc
                  0 0
                  (fi $ rectWidth r) (fi $ rectHeight r)
        freeGC dpy gc
      return dial
    recvSubEvent dial _ = return ([], dial)
    recvEvent dial _ = return ([], dial)

mkDial' :: Window -> Integer -> Construction VispM
mkDial' w maxv = do
  dpy <- asks vispDisplay
  win <- mkWindow w 1 1 20 20
  io $ mapWindow dpy win
  io $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
  construct (Dial maxv 0 win, win)

mkDial :: Constructor VispM
mkDial w [IntegerV maxv] [] = mkDial' w maxv
mkDial w [] [] = mkDial' w 12
mkDial _ [] _ = error "Dials do not have children"
mkDial _ _ [] = error "Dials take at most one integer argument"
mkDial _ _ _ = error "Invalid initial argument"

data Oriented m = Oriented {
      divideSpace :: Rectangle -> Integer -> [Rectangle]
    , children :: [SubWidget m]
  }

instance Object VispM (Oriented VispM) where

instance Widget VispM (Oriented VispM) where
    compose _ = return (Rectangle (0,0) 4 5)
    draw o r = do
      zipWithM_ draw (children o) $ divideSpace o r n
      return o
        where n = genericLength (children o)
    recvSubEvent o _ = return ([], o)
    recvEvent o _ = return ([], o)

horizontally :: Constructor VispM
horizontally w [] cs = construct (Oriented splitVert cs, w)
horizontally _ _ _ = error "horizontally: bad args"

vertically :: Constructor VispM
vertically w [] cs = construct (Oriented splitHoriz cs, w)
vertically _ _ _ = error "vertically: bad args"

dialProgram :: Program
dialProgram = do
  Program { programGUI = gui
          , programActions = M.fromList [ readStdin, onChange ] }
    where readStdin = ( SourcedPattern { patternSource = NamedSource "stdin"
                                       , patternEvent  = "data" 
                                       , patternVars   = ["data"] }
                      , ExprAction $ "value" `FieldOf` Var "dial" `Assign` Var "data" )
          onChange  = ( SourcedPattern { patternSource = NamedSource "dial"
                                       , patternEvent  = "changed" 
                                       , patternVars   = ["from", "to"] }
                      , ExprAction $ Print [Var "data"] )
          gui = GUI Nothing "Horizontally" [] [
                  GUI (Just "dial1") "Dial" [Literal $ IntegerV 12] []
                , GUI (Just "dial2") "Dial" [Literal $ IntegerV 12] []
                , GUI Nothing "Vertically" [] [
                            GUI (Just "dial4") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          , GUI (Just "dial5") "Dial" [Literal $ IntegerV 12] []
                          
                     ]
                , GUI (Just "dial3") "Dial" [Literal $ IntegerV 12] []
                ]
