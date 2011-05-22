-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Sindre, a programming language for writing simple GUIs
--
-----------------------------------------------------------------------------

module Main(main)
    where

import Sindre.Main

import System.Environment

-- | The main Sindre entry point.
main :: IO ()
main = sindreMain emptyProgram classMap objectMap funcMap globMap =<< getArgs
