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
import Sindre.Parser
import Sindre.Runtime
import Sindre.Sindre
import Sindre.Util
import Sindre.Widgets
import Sindre.X11

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Control.Applicative
import qualified Data.Map as M

main :: IO ()
main = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  args <- getArgs
  let cfg = AppConfig { cfgDisplay = dstr 
                      , cfgProgram = emptyProgram }
  case getOpt' Permute options args of
    (opts, _, _, []) -> runWithCfg args =<< foldl (>>=) (return cfg) opts
    (_, nonopts, unrecs, errs) -> do
      usage <- usageStr options
      badOptions usage nonopts errs unrecs

runWithCfg :: [String] -> AppConfig -> IO ()
runWithCfg args cfg =
  case getOpt' Permute progopts' args of
    (opts, [], [], []) -> exitWith =<< start (foldl (flip id) M.empty opts)
    (_, nonopts, unrecs, errs) -> do
      usage <- usageStr progopts'
      badOptions usage nonopts errs unrecs
  where (progopts, start) = sindreX11 (cfgProgram cfg) 
                          classMap objectMap (cfgDisplay cfg)
        progopts' = mergeOpts progopts

badOptions :: String -> [String] -> [String] -> [String] -> IO ()
badOptions usage nonopts errs unrecs = do 
  mapM_ (err . ("Junk argument: " ++)) nonopts
  mapM_ (err . ("Unrecognised argument: " ++)) unrecs
  err $ concat errs ++ usage
  exitFailure

mergeOpts :: [SindreOption] -> [SindreOption]
mergeOpts = (++map defang options)
    where defang (Option s l arg doc) = Option s l (idarg arg) doc
          idarg (ReqArg _ desc) = ReqArg (const id) desc
          idarg (OptArg _ desc) = OptArg (const id) desc
          idarg (NoArg _)       = NoArg id

data AppConfig = AppConfig {
      cfgProgram :: Program
    , cfgDisplay :: String
  }

usageStr :: [OptDescr a] -> IO String
usageStr opts = do
  prog <- getProgName
  let header = "Help for " ++ prog ++ " "
  return $ usageInfo header opts

type AppOption = OptDescr (AppConfig -> IO AppConfig)

options :: [AppOption]
options = [ Option "f" ["file"]
            (ReqArg (\arg cfg -> do
                       result <- parseSindre (cfgProgram cfg) arg <$> readFile arg 
                       case result of
                         Left e -> error $ show e
                         Right prog -> return $ cfg { cfgProgram = prog })
             "FILE")
            "Read program code from the given file."
          , Option "e" ["expression"]
            (ReqArg (\arg cfg ->
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
                 programGUI = (Nothing, GUI Nothing (P nowhere "") M.empty [])
               , programActions = []
               , programGlobals = []
               , programOptions = []
               , programFunctions = []
               , programBegin = []
               }
  
classMap :: ClassMap SindreX11M
classMap = M.fromList [ ("Dial", sizeable mkDial)
                      , ("Label", sizeable mkLabel)
                      , ("Horizontally", mkHorizontally)
                      , ("Vertically", mkVertically)
                      , ("Input", sizeable mkTextField)
                      , ("List", sizeable mkList)
                      , ("", mkUndef)
                      ]

objectMap :: ObjectMap SindreX11M
objectMap = M.fromList [ ("stdout", mkOutStream stdout)
                       , ("stderr", mkOutStream stderr)
                       , ("stdin", mkInStream stdin) ]
