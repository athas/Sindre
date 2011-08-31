{-# LANGUAGE DeriveFunctor #-}
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
                    , values )
    where

import Control.Applicative
import Control.Monad.State

import qualified Data.Map as M

import Text.Parsec hiding (many, (<|>), State)
import Text.Parsec.Perm
import Text.Parsec.String

-- | Parse a key-value string wrapper constructed via the permutation
-- parser combinators from 'Text.Parsec.Perm' and the parsers @value@
-- and @values@.
parseKV :: StreamPermParser String () a -> SourceName -> String -> Either ParseError a
parseKV p = parse (permute p <* eof)

-- | @value k@ is a parser for the single-valued key @k@.
value :: String -> Parser String
value k = try (string k) *> realSpaces *> char '=' *> realSpaces
          *> quotedString <* realSpaces

-- | @values k@ is a parser for the list-valued key @k@.  At least a
-- single value is required.
values :: String -> Parser [String]
values k = try (string k) *> realSpaces *> char '=' *> realSpaces
           *> many1 quotedString <* realSpaces

quotedString :: Parser String
quotedString = char '"' *> inner <* realSpaces
  where inner = do
          s <- many $ noneOf "\"\n"
          char '\"' *> (char '\"' *> (((s++"\"")++) <$> inner)
                        <|> return s)

realSpaces :: Parser String
realSpaces = many $ char ' '
