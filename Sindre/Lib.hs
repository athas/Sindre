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

liftF :: forall im a b. (Mold a,Mold b, MonadBackend im) =>
         (a -> b) -> [Value] -> Sindre im Value
liftF f = function body
    where body :: a -> Sindre im b
          body = return . f

liftF2 :: forall im a b c. (Mold a,Mold b,Mold c, MonadBackend im) =>
          (a -> b -> c) -> [Value] -> Sindre im Value
liftF2 f = function body
    where body :: a -> b -> Sindre im c
          body x = return . f x

liftF3 :: forall im a b c d. (Mold a,Mold b,Mold c,Mold d, MonadBackend im) =>
          (a -> b -> c -> d) -> [Value] -> Sindre im Value
liftF3 f = function body
    where body :: a -> b -> c -> Sindre im d
          body x y = return . f x y

stdFunctions :: MonadBackend im => FuncMap im
stdFunctions = M.fromList
               [ ("length", liftF (length :: String -> Int))
               , ("abs"   , liftF (abs :: Integer -> Integer))
               , ("substr", liftF3 $ \(s::String) m n ->
                   take n $ drop (m-1) s)
               , ("index",  liftF2 $ \(s::String) t ->
                   maybe 0 (1+) $ findIndex (isPrefixOf t) $ tails s)
               , ("tolower", liftF (map toLower))
               , ("toupper", liftF (map toUpper))
               ]

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
