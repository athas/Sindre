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

import Visp.Parser
import Visp.Visp
import Visp.Util

import Graphics.X11.Xlib hiding ( refreshKeyboardMapping
                                , Rectangle)
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras hiding (Event)
import qualified Graphics.X11.Xlib.Extras as X
import Graphics.X11.Xinerama

import System.Environment

import Control.Applicative
import "monads-fd" Control.Monad.Reader
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
  let cfg = VispConf { vispDisplay = dpy, vispScreen = screen }
  runVisp cfg dialProgram
  
setupDisplay :: String -> IO Display
setupDisplay dstr =
  openDisplay dstr `Prelude.catch` \_ ->
    error $ "Cannot open display \"" ++ dstr ++ "\"."

data VispConf = VispConf {
      vispDisplay  :: Display
    , vispScreen   :: Screen
    }

findRectangle :: Display -> Window -> IO X.Rectangle
findRectangle dpy rootw = do
  (_, _, _, x, y, _, _, _) <- queryPointer dpy rootw
  let hasPointer rect = fi x >= rect_x rect &&
                        fi (rect_width rect) + rect_x rect > fi x &&
                        fi y >= rect_y rect &&
                        fi (rect_height rect) + rect_y rect > fi y
  fromJust <$> find hasPointer <$> getScreenInfo dpy

runVisp :: VispConf -> Program VispM -> IO ()
runVisp cfg prog = return ()


newtype VispM a = VispM (ReaderT VispConf IO a)
  deriving (Functor, Monad, MonadIO, MonadReader VispConf)

instance MonadVisp VispM where
  type SubCfg VispM = VispConf
  type SubEvent VispM = X.Event
  
data GUIDesc m a = forall s . (Widget m s, MonadVisp m) =>
                   GUIDesc (Maybe String) (a -> m (s, a)) [GUIDesc m a]

initGUI :: (MonadVisp m, Functor m) => a -> GUIDesc m a ->
           m (M.Map String WidgetRef, WidgetBox m)
initGUI x (GUIDesc v f cs) = do
  (s, x')  <- f x
  (refs, boxes) <- unzip <$> mapM (initGUI x') cs
  let ms = map (snd . M.mapAccum (\a r -> (a+1, a:r)) 0) refs
  let m  = maybe M.empty (flip M.singleton []) v
  return $ (m `M.union` M.unions ms, WidgetBox boxes s)

  
mkUnmanagedWindow :: Window -> Position
                  -> Position -> Dimension -> Dimension -> VispM Window
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
         -> Position -> Dimension -> Dimension -> VispM Window
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

data Dial = Dial { dialMax :: Integer
                 , dialVal :: Integer
                 , dialWin :: Window
                 }

instance Widget VispM Dial where
    fieldSet dial "value" (IntegerV v) = return $ dial { dialVal = v }
    fieldSet dial _ _                  = return dial
    fieldGet dial "value" = IntegerV $ dialVal dial
    fieldGet _    _       = IntegerV 0
    compose _ = return (Rectangle (0,0) (4,5))
    draw dial r = do
      dpy <- asks vispDisplay
      --drawRectangle
      return dial
    recvRawEvent dial _ = return ([], dial)
    recvEvent dial _ = return ([], dial)

mkDial :: Integer -> Window -> VispM (Dial, Window)
mkDial max w = do
  win <- mkWindow w 0 0 20 20
  return (Dial max 0 win, win)

dialGUI :: GUIDesc VispM Window
dialGUI = GUIDesc (Just "dial") (mkDial 12) []

dialProgram :: Program VispM
dialProgram = Program { widgets = WidgetBox [] (Dial 12 0 undefined)
                      , actions = M.fromList [ ( SourcedPattern { patternSource = NamedSource (BuiltinRef "stdin")
                                                                , patternEvent  = "data" 
                                                                , patternVars   = ["data"] }
                                               , ExprAction $ "value" `FieldOf` Var "dial" `Assign` Var "data" )
                                             , ( SourcedPattern { patternSource = NamedSource (WidgetRef [])
                                                                , patternEvent  = "changed" 
                                                                , patternVars   = ["from", "to"] }
                                               , ExprAction $ Print [Var "data"] )] }
