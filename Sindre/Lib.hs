{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Lib
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Library routines and helper functions for the Sindre programming
-- language.
--
-----------------------------------------------------------------------------
module Sindre.Lib ( stdFunctions
                  , ioFunctions
                  , ioGlobals
                  , LiftFunction(..)
                  , KeyLike(..)
                  )
    where

import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime
import Sindre.Util

import System.Environment
import System.Exit
import System.IO
import System.Process hiding (env)
import Text.Regex.PCRE

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

lengthFun :: Value -> Integer
lengthFun (Dict m) = fi $ M.size m
lengthFun v = maybe 0 genericLength (mold v :: Maybe String)

builtin :: LiftFunction im m a => a -> Compiler im ([Value] -> m im Value)
builtin f = return $ function f

-- | A set of pure functions that can work with any Sindre backend.
-- Includes the functions @length@, @abs@, @substr@, @index@, @match@,
-- @sub@, @gsub@, @tolower@, and @toupper@.
stdFunctions :: forall im. MonadBackend im => FuncMap im
stdFunctions = M.fromList
               [ ("length", builtin $ return' . lengthFun)
               , ("abs"   , builtin $ return' . (abs :: Int -> Int))
               , ("substr", builtin $ \(s::String) m n ->
                   return' $ take n $ drop (m-1) s)
               , ("index",  builtin $ \(s::String) t ->
                   return' $ maybe 0 (1+) $ findIndex (isPrefixOf t) $ tails s)
               , ("match", do
                     rstart  <- setValue "RSTART"
                     rlength <- setValue "RLENGTH"
                     return $ function $ \(s::String) (r::String) -> do
                       let (stt, len) = s =~ r :: (Int, Int)
                       execute_ $ do rstart $ unmold (stt+1)
                                     rlength $ unmold len
                       return' $ unmold (stt+1))
               , ("sub", builtin sub)
               , ("gsub", builtin gsub)
               , ("tolower", builtin $ return' . map toLower)
               , ("toupper", builtin $ return' . map toUpper)
               ]
    where return' :: Mold a => a -> Sindre im a
          return' = return
          sub (r::String) t (s::String) =
            case s =~ r of
              (-1,_) -> return' s
              (i,n)  -> return' $ take i s ++ t ++ drop (i+n) s
          gsub (r::String) t (s::String) =
            case s =~ r of
              (-1,_) -> return' s
              (i,n)  -> do s' <- gsub r t $ drop (i+n) s
                           return' $ take i s ++ t ++ s'
-- | A set of impure functions that only work in IO backends.
-- Includes the @system@ function.
ioFunctions :: (MonadIO m, MonadBackend m) => FuncMap m
ioFunctions = M.fromList
              [ ("system", do
                   exitval <- setValue "EXITVAL"
                   builtin $ \s -> do
                     c <- io $ system s
                     let v = case c of ExitSuccess   -> 0
                                       ExitFailure e -> e
                     execute_ $ exitval $ unmold v
                     return' v)
              , ("osystem", do
                    exitval <- setValue "EXITVAL"
                    return $ function $ \s -> do
                      (Just inh, Just outh, _, pid) <-
                        io $ createProcess (shell s) { std_in  = CreatePipe,
                                                       std_out = CreatePipe,
                                                       std_err = Inherit }
                      io $ hClose inh
                      output <- io $ hGetContents outh
                      ex <- io $ waitForProcess pid
                      execute_ $ exitval $ unmold $ case ex of
                        ExitSuccess   -> 0
                        ExitFailure r -> r
                      return' output)
              ]
    where return' :: Mold a => a -> Sindre im a
          return' = return

-- | Global variables that require an IO backend.  Includes the
-- @ENVIRON@ global.
ioGlobals :: MonadIO im => M.Map Identifier (im Value)
ioGlobals = M.fromList [("ENVIRON", do
                           env <- io getEnvironment
                           let f (k, s) = (unmold k, unmold s)
                           return $ Dict $ M.fromList $ map f env)
                       ]

-- | A class making it easy to adapt Haskell functions as Sindre
-- functions that take and return 'Value's.
class (MonadBackend im, MonadSindre im m) => LiftFunction im m a where
  function :: a -> [Value] -> m im Value
  -- ^ @function f@ is a monadic function that accepts a list of
  -- 'Value's and returns a 'Value'.  If the list does not contain the
  -- number, or type, of arguments expected by @f@, 'fail' will be
  -- called with an appropriate error message.

instance (Mold a, MonadSindre im m) => LiftFunction im m (m im a) where
  function x [] = liftM unmold x
  function _ _ = fail "Too many arguments"

instance (Mold a, LiftFunction im m b, MonadSindre im m)
    => LiftFunction im m (a -> b) where
  function f (x:xs) = case mold x of
                        Nothing -> fail "Cannot mold argument"
                        Just x' -> f x' `function` xs
  function _ [] = fail "Not enough arguments"

-- | Convenience class for writing 'Chord' values.
class KeyLike a where
  chord :: [KeyModifier] -> a -> Chord
  -- ^ Given a list of modifiers and either a 'char' or a 'String',
  -- yield a 'Chord'.  If given a character, the Chord will contain a
  -- 'CharKey', if given a string, it will contain a 'CtrlKey'.

instance KeyLike Char where
  chord ms c = (S.fromList ms, CharKey c)

instance KeyLike String where
  chord ms s = (S.fromList ms, CtrlKey s)
