{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  dpy <- setupDisplay dstr
  let screen = defaultScreenOfDisplay dpy
  rect <- findRectangle dpy (rootWindowOfScreen screen)
  let cfg = VispConf { vispDisplay = dpy
                     , vispScreen = screen 
                     , vispRoot = rootWindowOfScreen screen }
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

mkUnmanagedWindow :: Window -> Position
                  -> Position -> Dimension -> Dimension -> InitVispM Window
mkUnmanagedWindow rw x y w h = do
  dpy <- asks vispDisplay
  s   <- asks vispScreen
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
toXRect r@(Rectangle (x1, y1) (x2, y2)) =
  X.Rectangle { rect_x = fi x1, rect_y = fi y2,
                rect_width = fi $ width r,
                rect_height = fi $ height r }

classMap :: ClassMap VispM
classMap = M.fromList [("Dial", mkDial)]

data Dial = Dial { dialMax :: Integer
                 , dialVal :: Integer
                 , dialWin :: Window
                 }

mkDial :: Constructor VispM
mkDial w [IntegerV max] = do
  win <- mkWindow w 1 1 20 20
  return (buildCont $ Dial max 0 win, win)
mkDial _ _ = error "Invalid initial argument"

instance Object VispM Dial where
    fieldSet dial "value" (IntegerV v) = return $ dial { dialVal = v }
    fieldSet dial _ _                  = return dial
    fieldGet dial "value" = return $ IntegerV $ dialVal dial
    fieldGet _    _       = return $ IntegerV 0

instance Widget VispM Dial where
    compose _ = return (Rectangle (0,0) (4,5))
    draw dial r = do
      dpy <- asks vispDisplay
      scr <- asks vispScreen
      io $ do
        gc <- createGC dpy $ dialWin dial
        setForeground dpy gc $ blackPixelOfScreen scr
        setBackground dpy gc $ whitePixelOfScreen scr        
        drawRectangles dpy (dialWin dial) gc [toXRect r]
        freeGC dpy gc
      return dial
    recvSubEvent dial _ = return ([], dial)
    recvEvent dial _ = return ([], dial)

dialProgram :: Program
dialProgram = do
  Program { gui = GUI (Just "dial") "Dial" [Literal $ IntegerV 12] []
          , actions = M.fromList [ readStdin, onChange ] }
    where readStdin = ( SourcedPattern { patternSource = NamedSource "stdin"
                                       , patternEvent  = "data" 
                                       , patternVars   = ["data"] }
                      , ExprAction $ "value" `FieldOf` Var "dial" `Assign` Var "data" )
          onChange  = ( SourcedPattern { patternSource = NamedSource "dial"
                                       , patternEvent  = "changed" 
                                       , patternVars   = ["from", "to"] }
                      , ExprAction $ Print [Var "data"] )
