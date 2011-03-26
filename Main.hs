-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- Sindre, a programming language for writing simple GUIs
--
-----------------------------------------------------------------------------

module Main(main)
    where

import Sindre.Compiler
import Sindre.Widgets
import Sindre.Parser
import Sindre.X11

import System.Environment
import System.Exit
import System.IO

import Control.Applicative
import qualified Data.Map as M

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  args <- getArgs
  result <- parseSindre (args!!0) <$> readFile (args!!0)
  case result of
    Left e -> error $ show e
    Right program -> do sindreX11 program classMap objectMap dstr
                        exitSuccess
  
classMap :: ClassMap SindreX11M
classMap = M.fromList [ ("Dial", sizeable mkDial)
                      , ("Horizontally", mkHorizontally)
                      , ("Vertically", mkVertically)
                      ]

objectMap :: ObjectMap SindreX11M
objectMap = M.fromList [ ("stdout", mkOutStream stdout) 
                       , ("stderr", mkOutStream stderr) ]
