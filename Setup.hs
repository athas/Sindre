#!/usr/bin/env runhaskell

import Distribution.Simple.Setup (CopyDest(..),ConfigFlags(..),BuildFlags(..),
                                  CopyFlags(..),RegisterFlags(..),InstallFlags(..),
                                  defaultRegisterFlags,fromFlagOrDefault,Flag(..),
                                  defaultCopyFlags)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.InstallDirs
                            (InstallDirs(..))
import Distribution.Simple.Utils
import Distribution.Verbosity
import Data.List (isPrefixOf)
import Data.Version
import Control.Monad (unless)
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

subst :: String -> String -> String -> String
subst t r [] = []
subst t r s@(c:s')
  | t `isPrefixOf` s = r ++ subst t r (drop (length t) s)
  | otherwise = c : subst t r s'

sindrePostInst a (InstallFlags { installPackageDB = db, installVerbosity = v }) =
  sindrePostCopy a (defaultCopyFlags { copyDest = Flag NoCopyDest, copyVerbosity = v })

sindrePostCopy a (CopyFlags { copyDest = cdf, copyVerbosity = vf }) pd lbi =
  do let v         = fromFlagOrDefault normal vf
         cd        = fromFlagOrDefault NoCopyDest cdf
         dirs      = absoluteInstallDirs pd lbi cd
         bin       = combine (bindir dirs)
         substVersion = subst "VERSION" $ showVersion $ packageVersion $ package pd
     unless isWindows $ do
       copyFileVerbose v "sinmenu" (bin "sinmenu")
       fs <- getFileStatus (bin "sindre")
       setFileMode (bin "sinmenu") $ fileMode fs
       putStrLn $ "Installing manpage in " ++ mandir dirs
       createDirectoryIfMissing True $ mandir dirs `combine` "man1"
       withFileContents "sindre.1" $
         writeFileAtomic (mandir dirs `combine` "man1" `combine` "sindre.1") . substVersion
       withFileContents "sinmenu.1" $
         writeFileAtomic (mandir dirs `combine` "man1" `combine` "sinmenu.1") . substVersion
