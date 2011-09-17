{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.ANSI
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  unportable
--
-- ANSI backend for Sindre.
--
-----------------------------------------------------------------------------
module Sindre.ANSI( SindreANSIM
                  , sindreANSI
                  )
    where

import Sindre.Sindre
import Sindre.Compiler
import Sindre.Lib
import Sindre.Runtime
import Sindre.Util
import Sindre.Widgets

import System.Console.ANSI

import System.Environment
import System.Exit
import System.IO
import System.Posix.Types

import Control.Arrow(first,second)
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Char hiding (Control)
import Data.Maybe
import Data.List
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Monoid
import Data.Ord
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Prelude hiding (catch)


-- | The read-only configuration of the ANSI backend, created during
-- backend initialisation.
data SindreANSIConf = SindreANSIConf {
    sindreTerminal   :: Handle -- ^ Where we're being displayed.
  , sindreVisualOpts :: VisualOpts
  -- ^ The default visual options used if no others are specified for
  -- a widget.
  , sindreEvtVar     :: MVar Event
  -- ^ Channel through which events are sent by other threads to the
  -- Sindre command loop.
  }

-- | Sindre backend using ANSI.
newtype SindreANSIM a = SindreANSIM (ReaderT SindreANSIConf (StateT Rectangle IO) a)
  deriving ( Functor, Monad, MonadIO, MonadReader SindreANSIConf
           , MonadState Rectangle, Applicative)

runSindreANSI :: SindreANSIM a -> SindreANSIConf -> Rectangle -> IO a
runSindreANSI (SindreANSIM m) = evalStateT . runReaderT m

instance MonadBackend SindreANSIM where
  type BackEvent SindreANSIM = Char
  type RootPosition SindreANSIM = ()

  redrawRoot = do
    (orient, rootwr) <- gets rootWidget
    reqs <- compose rootwr
    winsize <- back get
    let orient' = fromMaybe () orient
        rect = fitRect winsize reqs
    draw rootwr $ Just rect
    return ()

  redrawRegion _ = return ()
  
  waitForBackEvent = do
    evvar <- back $ asks sindreEvtVar
    io $ takeMVar evvar
  
  getBackEvent = do
    io yield
    back (io . tryTakeMVar =<< asks sindreEvtVar)

  printVal s = io $ putStr s *> hFlush stdout

setupTerminal :: IO Handle
setupTerminal = do h <- openFile "/dev/tty" ReadWriteMode
                   hSetBuffering h NoBuffering
                   hSetEcho h False
                   return h

getKeypress :: Handle -> IO Chord
getKeypress h = (S.empty,) <$> CharKey <$> hGetChar h

eventReader :: Handle -> MVar Event -> IO ()
eventReader h evvar = forever $ (putMVar evvar . KeyPress) =<< getKeypress h

sindreANSICfg :: IO SindreANSIConf
sindreANSICfg = do
  h <- setupTerminal
  visopts <- defVisualOpts
  evvar <- newEmptyMVar
  xlock <- newMVar ()
  _ <- forkIO $ eventReader h evvar
  return SindreANSIConf { sindreTerminal = h
                        , sindreVisualOpts = visopts
                        , sindreEvtVar = evvar }

-- | Options regarding visual appearance of widgets (colours and
-- fonts).
data VisualOpts = VisualOpts {
      foreground      :: Color
    , background      :: Color
    , focusForeground :: Color
    , focusBackground :: Color
    }

defVisualOpts :: IO VisualOpts
defVisualOpts = pure $ VisualOpts Black White White Blue

-- | Execute Sindre in the ANSI backend.
sindreANSI :: SindreANSIM ExitCode
           -- ^ The function returned by
           -- 'Sindre.Compiler.compileSindre' after command line
           -- options have been given
           -> IO ExitCode
sindreANSI start = do
  cfg <- sindreANSICfg
  rows <- read <$> getEnv "LINES"
  cols <- read <$> getEnv "COLUMNS"
  runSindreANSI start cfg $ Rectangle 0 0 rows cols
