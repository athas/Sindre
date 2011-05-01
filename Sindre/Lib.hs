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
                  , LiftFunction(..)
                  )
    where

import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M

stdFunctions :: forall im. MonadBackend im => FuncMap im
stdFunctions = M.fromList
               [ ("length", function $ return' . (length :: String -> Int))
               , ("abs"   , function $ return' . (abs :: Int -> Int))
               , ("substr", function $ \(s::String) m n ->
                   return' $ take n $ drop (m-1) s)
               , ("index",  function $ \(s::String) t ->
                   return' $ maybe 0 (1+) $ findIndex (isPrefixOf t) $ tails s)
               , ("tolower", function $ return' . map toLower)
               , ("toupper", function $ return' . map toUpper)
               ]
    where return' :: Mold a => a -> Sindre im a
          return' = return

class (MonadBackend im, MonadSindre im m) => LiftFunction im m a where
  function :: a -> [Value] -> m im Value

instance (Mold a, MonadSindre im m) => LiftFunction im m (m im a) where
  function x [] = liftM unmold x
  function _ _ = error "Too many arguments"

instance (Mold a, LiftFunction im m b, MonadSindre im m)
    => LiftFunction im m (a -> b) where
  function f (x:xs) = case mold x of
                        Nothing -> error "Cannot mold argument"
                        Just x' -> f x' `function` xs
  function _ [] = error "Not enough arguments"
