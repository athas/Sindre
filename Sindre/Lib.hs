{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Lib
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- Built-in definitions for the Sindre language.
--
-----------------------------------------------------------------------------

module Sindre.Lib ( stdFunctions
                  , ioFunctions
                  , LiftFunction(..)
                  )
    where

import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime
import Sindre.Util

import System.Exit
import System.Process
import Text.Regex.PCRE

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import qualified Data.Map as M

lengthFun :: Value -> Integer
lengthFun (Dict m) = fi $ M.size m
lengthFun v = maybe 0 genericLength $ (mold v :: Maybe String)

stdFunctions :: forall im. MonadBackend im => FuncMap im
stdFunctions = M.fromList
               [ ("length", function $ return' . lengthFun)
               , ("abs"   , function $ return' . (abs :: Int -> Int))
               , ("substr", function $ \(s::String) m n ->
                   return' $ take n $ drop (m-1) s)
               , ("index",  function $ \(s::String) t ->
                   return' $ maybe 0 (1+) $ findIndex (isPrefixOf t) $ tails s)
               , ("match", function $ \(s::String) (r::String) ->
                   return' $ fst (s =~ r :: (Int, Int)))
               , ("sub", function sub)
               , ("gsub", function gsub)
               , ("tolower", function $ return' . map toLower)
               , ("toupper", function $ return' . map toUpper)
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

ioFunctions :: forall im.(MonadIO im, MonadBackend im) => FuncMap im
ioFunctions = M.fromList
              [ ("system", function $ \s -> do
                   c <- io $ system s
                   case c of ExitSuccess   -> return' 0
                             ExitFailure e -> return' e)
              ]
    where return' :: Mold a => a -> Sindre im a
          return' = return

class (MonadBackend im, MonadSindre im m) => LiftFunction im m a where
  function :: a -> [Value] -> m im Value

instance (Mold a, MonadSindre im m) => LiftFunction im m (m im a) where
  function x [] = liftM unmold x
  function _ _ = fail "Too many arguments"

instance (Mold a, LiftFunction im m b, MonadSindre im m)
    => LiftFunction im m (a -> b) where
  function f (x:xs) = case mold x of
                        Nothing -> fail "Cannot mold argument"
                        Just x' -> f x' `function` xs
  function _ [] = fail "Not enough arguments"
