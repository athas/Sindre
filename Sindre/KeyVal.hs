{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.KeyVal
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple language for mapping keys to either a single string-value
-- or a list of strings.  The syntax is line-oriented and extremely
-- simple.  A line consists of key-value pairs, which are written as
-- the key, followed by an equals sign, followed by a double-quoted
-- string.  Several double-quoted strings can follow the equal sign,
-- in which case they will be treated as a list.  Space characters
-- separate elements, as so:
--
-- @foo="string" bar="another string" baz="s1" "s2" "this is a list" "s4"@
--
-- Literal double-quotes can be included in a string by doubling them.
--
-- @foo="this string contains ""quotes"""
--
-----------------------------------------------------------------------------
module Sindre.KeyVal( parseKV
                    , value
                    , values
                    , (<$?>)
                    , (<||>)
                    , (<$$>)
                    , (<|?>) )
    where

import Control.Applicative hiding (many, empty)
import Control.Monad.Identity

import Data.Attoparsec.Text

import qualified Data.Text as T

import Text.ParserCombinators.Perm

import Prelude hiding (takeWhile)

-- | Parse a key-value string wrapper constructed via the permutation
-- parser combinators from 'Text.Parsec.Perm' and the parsers @value@
-- and @values@.
parseKV :: PermParser Parser a -> T.Text -> Either String a
parseKV p s =
  eitherResult $ parse (permute p <* endOfInput) s `feed` T.empty

-- | @value k@ is a parser for the single-valued key @k@.
value :: T.Text -> Parser T.Text
value k = try (string k) *> realSpaces *> char '=' *> realSpaces
          *> quotedString <* realSpaces

-- | @values k@ is a parser for the list-valued key @k@.  At least a
-- single value is required.
values :: T.Text -> Parser [T.Text]
values k = try (string k) *> realSpaces *> char '=' *> realSpaces
           *> many1 quotedString <* realSpaces

quotedString :: Parser T.Text
quotedString = char '"' *> inner <* realSpaces
  where inner = do
          s <- takeWhile (/='"')
          char '\"' *> (char '\"' *> (T.append (T.snoc s '"') <$> inner)
                        <|> return s)

realSpaces :: Parser T.Text
realSpaces = takeWhile (==' ')
