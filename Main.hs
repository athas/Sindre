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
import Sindre.Runtime
import Sindre.Widgets
import Sindre.Parser
import Sindre.Sindre
import Sindre.Util
import Sindre.X11

import Control.Applicative

import System.Environment
import System.Exit
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  args <- getArgs
  result <- parseSindre (args!!0) <$> readFile (args!!0)
  case result of
    Left e -> error $ show e
    Right program -> do sindreX11 program classMap dstr
                        exitSuccess
  
classMap :: ClassMap SindreX11M
classMap = M.fromList [ ("Dial", sizeable mkDial)
                      , ("Horizontally", mkHorizontally)
                      , ("Vertically", mkVertically)
                      ]
