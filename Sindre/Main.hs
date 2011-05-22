-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Main
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Sindre, a programming language for writing simple GUIs
--
-----------------------------------------------------------------------------

module Sindre.Main( sindreMain,
                    emptyProgram,
                    classMap,
                    objectMap,
                    funcMap,
                    globMap,
                    module Sindre.Sindre,
                    module Sindre.Runtime,
                    module Sindre.Compiler)
    where

import Sindre.Compiler
import Sindre.Lib
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

-- | The main Sindre entry point.
sindreMain :: Program -> ClassMap SindreX11M -> ObjectMap SindreX11M
           -> FuncMap SindreX11M -> GlobMap SindreX11M
           -> [String] -> IO ()
sindreMain prog cm om fm gm args = do
  dstr <- getEnv "DISPLAY" `catch` const (return "")
  let cfg = AppConfig { cfgDisplay = dstr 
                      , cfgProgram = prog }
  case getOpt' Permute options args of
    (opts, _, _, []) -> do
      cfg' <- foldl (>>=) (return cfg) opts
      let (srcopts, start) = compileSindre (cfgProgram cfg') cm om fm gm
          progopts = mergeOpts srcopts
      case getOpt' Permute progopts args of
        (opts', [], [], []) ->
          let start' = start $ foldl (flip id) M.empty opts'
          in exitWith =<< sindreX11 (cfgDisplay cfg') start'
        (_, nonopts, unrecs, errs) -> do
          usage <- usageStr progopts
          badOptions usage nonopts errs unrecs
    (_, nonopts, unrecs, errs) -> do
      usage <- usageStr options
      badOptions usage nonopts errs unrecs
    

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

mkUndef :: MonadBackend m => Constructor m
mkUndef _ _ = sindre $ fail "No GUI defined (empty program?)"

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
classMap = M.fromList [ ("Dial", mkDial)
                      , ("Label", mkLabel)
                      , ("Blank", mkBlank)
                      , ("Horizontally", mkHorizontally)
                      , ("Vertically", mkVertically)
                      , ("Input", mkTextField)
                      , ("HList", mkHList)
                      , ("VList", mkVList)
                      , ("", mkUndef)
                      ]

objectMap :: ObjectMap SindreX11M
objectMap = M.fromList [ ("stdin", mkInStream stdin) ]

funcMap :: FuncMap SindreX11M
funcMap = stdFunctions `M.union` ioFunctions

globMap :: GlobMap SindreX11M
globMap = ioGlobals
