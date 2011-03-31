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
import Sindre.Sindre
import Sindre.Widgets
import Sindre.Parser
import Sindre.X11
import Sindre.Util

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Control.Applicative
import qualified Data.Map as M

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  opts <- getOpt Permute options <$> getArgs
  let cfg = AppConfig { cfgDisplay = dstr 
                      , cfgProgram = emptyProgram }
  case opts of
    (opts', [], []) -> runWithCfg =<< foldl (>>=) (return cfg) opts'
    (_, nonopts, errs) -> do 
              mapM_ (err . ("Junk argument: " ++)) nonopts
              usage <- usageStr
              err $ concat errs ++ usage
              exitFailure

runWithCfg :: AppConfig -> IO ()
runWithCfg cfg = do
  sindreX11 (cfgProgram cfg) classMap objectMap (cfgDisplay cfg)
  exitSuccess

data AppConfig = AppConfig {
      cfgProgram :: Program
    , cfgDisplay :: String
  }

usageStr :: IO String
usageStr = do
  prog <- getProgName
  let header = "Help for " ++ prog ++ " "
  return $ usageInfo header options

type SindreOption = OptDescr (AppConfig -> IO AppConfig)

options :: [SindreOption]
options = [ Option ['f'] ["file"]
            (ReqArg (\arg cfg -> do
                       result <- parseSindre (cfgProgram cfg) arg <$> readFile arg 
                       case result of
                         Left e -> error $ show e
                         Right prog -> return $ cfg { cfgProgram = prog })
             "FILE")
            "Read program code from the given file."
          , Option ['e'] ["expression"]
            (ReqArg (\arg cfg -> do
                       case parseSindre (cfgProgram cfg) "expression" arg of
                         Left e -> error $ show e
                         Right prog -> return $ cfg { cfgProgram = prog })
             "code")
            "Add the given code to the program."
          ]

mkUndef :: MonadSubstrate m => Constructor m
mkUndef _ _ _ = fail "No GUI defined (empty program?)"

emptyProgram :: Program
emptyProgram = Program {
                 programGUI = GUI Nothing "" M.empty []
               , programActions = []
               , programConstants = []
               , programFunctions = M.empty
               }
  
classMap :: ClassMap SindreX11M
classMap = M.fromList [ ("Dial", sizeable mkDial)
                      , ("Horizontally", mkHorizontally)
                      , ("Vertically", mkVertically)
                      , ("", mkUndef)
                      ]

objectMap :: ObjectMap SindreX11M
objectMap = M.fromList [ ("stdout", mkOutStream stdout)
                       , ("stderr", mkOutStream stderr)
                       , ("stdin", mkInStream stdin) ]
