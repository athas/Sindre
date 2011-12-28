{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Widgets
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Portable Sindre gadgets and helper functions that can be used by
-- any backend.
--
-----------------------------------------------------------------------------
module Sindre.Widgets ( mkHorizontally
                      , mkVertically
                      , changeFields
                      , Match(..)
                      , match
                      , filterMatches
                      , sortMatches
                      )
    where

import Sindre.Sindre
import Sindre.Compiler
import Sindre.Runtime

import Control.Monad.Error
import Control.Monad.State
import Control.Applicative

import Data.List
import Data.Maybe
import qualified Data.Text as T

data Oriented = Oriented {
      mergeSpace :: [SpaceNeed] -> SpaceNeed
    , splitSpace :: Rectangle -> [SpaceNeed] -> [Rectangle]
    , children   :: [WidgetRef]
  }

instance MonadBackend m => Object m Oriented where

instance MonadBackend m => Widget m Oriented where
    composeI = do
      chlds <- gets children
      gets mergeSpace <*> mapM compose chlds
    drawI r = do
      chlds <- gets children
      rects <- gets splitSpace <*> pure r <*> mapM compose chlds
      concat <$> zipWithM draw (reverse chlds) (Just <$> reverse rects)

sumPrim :: [DimNeed] -> DimNeed
sumPrim []     = Min 0
sumPrim (d:ds) = foldl f d ds
    where f (Min x) (Min y) = Min (x+y)
          f (Min x) (Max y) = Max (x+y)
          f (Min x) (Exact y) = Min (x+y)
          f (Max x) (Max y) = Max (x+y)
          f (Max x) (Exact y) = Max (x+y)
          f (Exact x) (Exact y) = Exact (x+y)
          f _ Unlimited = Unlimited
          f x y = f y x

sumSec :: [DimNeed] -> DimNeed
sumSec []     = Min 0
sumSec (d:ds) = foldl f d ds
    where f (Min x) (Min y) = Min $ max x y
          f (Min x) (Max y) | x < y = Max y
          f (Min x) (Max _)         = Max x
          f (Min _) (Exact y)         = Exact y
          f (Max x) (Max y) = Max $ max x y
          f (Max _) (Exact y) = Exact y
          f (Max x) Unlimited = Max x
          f (Exact x) (Exact y) = Exact $ max x y
          f (Exact x) Unlimited = Exact x
          f _ Unlimited = Unlimited
          f x y = f y x

layouting :: MonadBackend m => (forall a. ((a, a) -> a)) -> Constructor m
layouting f _ cs = return $ NewWidget $ Oriented merge split (map snd cs)
    where merge rects = ( f (sumPrim, sumSec) $ map fst rects
                        , f (sumSec, sumPrim) $ map snd rects )
          split r     = f (splitVert, splitHoriz) r . map f

-- | A widget that arranges its children in a horizontal row.
mkHorizontally :: MonadBackend m => Constructor m
mkHorizontally = layouting fst

-- | A widget that arranges its children in a vertical column.
mkVertically :: MonadBackend m => Constructor m
mkVertically = layouting snd

-- | @changeFields fs m@ applies @m@ to the state of the object,
-- replacing the state with the return value of @m@.  Value-changed
-- events are sent for each pair of field-name and accessor function
-- passed in @fs@.
changeFields :: MonadBackend im => [(Identifier, a -> Value)]
            -> (a -> ObjectM a im a) -> ObjectM a im ()
changeFields fs m = do
  s <- get
  s' <- m s
  put s' >> mapM_ (\(k, f) -> changed k (f s) (f s')) fs

-- | The result of using 'match' to apply a user-provided pattern to a
-- string.
data Match = ExactMatch
           | PrefixMatch
           | InfixMatch
             deriving (Eq, Ord, Show)

-- | @match pat s@ applies the pattern @pat@ to @s@ and returns a
-- 'Match' describing the kind of match if any, or 'Nothing'
-- otherwise.  The pattern is interpreted as tokens delimited by
-- whitespace, and each token must be present somewhere in @s@.
match :: T.Text -> T.Text -> Maybe Match
match pat s
  | pat == s = Just ExactMatch
  | otherwise =
    case T.words pat of
      []         -> Just PrefixMatch
      pat'@(x:_) | all look pat' -> if x `T.isPrefixOf` s
                                    then Just PrefixMatch
                                    else Just InfixMatch
                 | otherwise     -> Nothing
        where look tok = tok `T.isInfixOf` s

-- | @filterMatches f pat l@ returns only those elements of @l@ that
-- match @pat@, using @f@ to convert each element to a 'T.Text'.  The
-- result will be ordered equivalently to @l@
filterMatches :: (a -> T.Text) -> T.Text -> [a] -> [a]
filterMatches f pat = filter (isJust . match pat . f)

-- | @sortMatches f pat l@ returns only those elements of @l@ that
-- match @pat@, using @f@ to convert each element to a 'T.Text'.  The
-- result will be reordered such that exact matches come first, then
-- prefixes, then infixes, although original order will be maintained
-- within these three groups.
sortMatches :: (a -> T.Text) -> T.Text -> [a] -> [a]
sortMatches f t ts = map snd $ exacts++prefixes++infixes
  where attach y = do m <- match t $ f y
                      return (m, y)
        matches = mapMaybe attach ts
        (exacts, nonexacts) = partition ((==ExactMatch) . fst) matches
        (prefixes, infixes) =
          partition ((==PrefixMatch) . fst) nonexacts
