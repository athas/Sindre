{-# LANGUAGE ScopedTypeVariables #-}
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
                    module Export )
    where

import Sindre.Compiler as Export
import Sindre.Lib
import Sindre.Parser
import Sindre.Runtime as Export
import Sindre.Sindre as Export
import Sindre.Util
import Sindre.Widgets
import Sindre.X11

import Paths_sindre (version)

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Types
import System.Locale.SetLocale(setLocale, Category(..))

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import qualified Data.Map as M
import qualified Data.Traversable as T
import Data.Version (showVersion)

setupLocale :: IO ()
setupLocale = do
  ret <- setLocale LC_ALL Nothing
  case ret of
    Nothing -> putStrLn "Can't set locale." >> exitFailure
    _       -> return ()

-- | The main Sindre entry point.
sindreMain :: Program -> ClassMap SindreX11M -> ObjectMap SindreX11M
           -> FuncMap SindreX11M -> GlobMap SindreX11M
           -> [String] -> IO ()
sindreMain prog cm om fm gm args = do
  setupLocale
  dstr <- getEnv "DISPLAY" `catch` \(_ :: IOException) -> (return "")
  let cfg = AppConfig { cfgDisplay = dstr
                      , cfgProgram = prog
                      , cfgBackend = sindreX11override
                      , cfgFiles   = M.empty }
  case getOpt' Permute options args of
    (opts, _, _, []) -> do
      cfg' <- foldl (>>=) (return cfg) opts
      hom <- T.mapM (liftM mkInStream . fdToHandle) $ cfgFiles cfg'
      let (srcopts, start) =
            compileSindre (cfgProgram cfg') cm (om `M.union` hom) fm gm
          progopts = mergeOpts srcopts
      case getOpt' Permute progopts args of
        (opts', [], [], []) ->
          let start' = start $ foldl (flip id) M.empty opts'
          in exitWith =<< cfgBackend cfg' (cfgDisplay cfg') start'
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
  hPutStr stderr $ concat errs ++ usage
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
  , cfgBackend :: String
               -> SindreX11M ExitCode
               -> IO ExitCode
  , cfgFiles :: M.Map String Fd
  }

usageStr :: [OptDescr a] -> IO String
usageStr opts = do
  prog <- getProgName
  let header = "Help for " ++ prog ++ " (Sindre " ++ showVersion version ++ ")"
  return $ usageInfo header opts

type AppOption = OptDescr (AppConfig -> IO AppConfig)

options :: [AppOption]
options =
  [ Option "f" ["file"]
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
  , Option "" ["wmmode"]
    (let wmmode "normal" cfg = return cfg { cfgBackend = sindreX11 }
         wmmode "override" cfg = return cfg { cfgBackend = sindreX11override }
         wmmode "dock" cfg = return cfg { cfgBackend = sindreX11dock }
         wmmode _ _ = error "Argument to --wmmode must be normal, override or dock."
     in ReqArg wmmode "normal|override|dock")
    "How Sindre interacts with the window manager (defaults to 'override')."
  , Option "v" ["version"]
    (NoArg (\_ -> do hPutStrLn stderr $ "Sindre " ++ showVersion version ++ " (C) " ++ mail
                     exitSuccess))
    "Show version information."
  , Option "h" ["help"]
    (NoArg (\_ -> do hPutStr stderr =<< usageStr options
                     exitSuccess))
    "Show usage information."
  , Option "" ["fd"]
    (ReqArg (\arg cfg ->
             case span isAlpha arg of
               (name@(_:_), '=':fdnum@(_:_)) | all isDigit fdnum ->
                 return cfg { cfgFiles = M.insert name (read fdnum)
                                         $ cfgFiles cfg }
               _ -> error "Malformed --fd option")
    "STREAMNAME=FD")
     "Create input stream from file descriptor"
  ]

mail :: String
mail = "Troels Henriksen <athas@sigkill.dk>"

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
