#!/usr/bin/env runhaskell

import Distribution.Simple.Setup (CopyDest(..),ConfigFlags(..),BuildFlags(..),
                                  CopyFlags(..),RegisterFlags(..),InstallFlags(..),
                                  defaultRegisterFlags,fromFlagOrDefault,Flag(..),
                                  defaultCopyFlags)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
                            (LocalBuildInfo(..),absoluteInstallDirs)
import Distribution.Simple.Configure (configCompilerAux)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.InstallDirs
                            (InstallDirs(..))
import Distribution.Simple.Program 
                            (Program(..),ConfiguredProgram(..),ProgramConfiguration(..),
                             ProgramLocation(..),simpleProgram,lookupProgram,
                             rawSystemProgramConf)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Data.Char (isSpace, showLitChar)
import Data.List (isSuffixOf,isPrefixOf)
import Data.Maybe (listToMaybe,isJust)
import Data.Version
import Control.Monad (when,unless)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Exit
import System.IO (hGetContents,hClose,hPutStr,stderr)
import System.IO.Error (try)
import System.Process (runInteractiveProcess,waitForProcess)
import System.Posix
import System.Directory
import System.Info (os)
import System.FilePath

main = defaultMainWithHooks sindreHooks
sindreHooks = simpleUserHooks { postInst = sindrePostInst
                              , postCopy = sindrePostCopy }

sindre = "sindre"

isWindows :: Bool
isWindows = os == "mingw" -- XXX

sindrePostInst a (InstallFlags { installPackageDB = db, installVerbosity = v }) =
  sindrePostCopy a (defaultCopyFlags { copyDest = Flag NoCopyDest, copyVerbosity = v })

sindrePostCopy a (CopyFlags { copyDest = cdf, copyVerbosity = vf }) pd lbi =
  do let v         = fromFlagOrDefault normal vf
         cd        = fromFlagOrDefault NoCopyDest cdf
         dirs      = absoluteInstallDirs pd lbi cd
         bin       = combine (bindir dirs)
     unless isWindows $ do
       copyFileVerbose v "sinmenu" (bin "sinmenu")
       fs <- getFileStatus (bin "sindre")
       setFileMode (bin "sinmenu") $ fileMode fs
       putStrLn $ "Installing manpage in " ++ mandir dirs
       createDirectoryIfMissing True $ mandir dirs `combine` "man1"
       copyFileVerbose v "sindre.1" (mandir dirs `combine` "man1" `combine` "sindre.1")
       createDirectoryIfMissing True $ mandir dirs `combine` "man1"
       copyFileVerbose v "sinmenu.1" (mandir dirs `combine` "man1" `combine` "sinmenu.1")