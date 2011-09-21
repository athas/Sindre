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

import Prelude hiding (takeWhile)

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>

{---------------------------------------------------------------
  Building a permutation parser

  Taken from Text.Parsec.Perm.
---------------------------------------------------------------}

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input -
-- use the optional combinator ('<|?>') instead. Returns a
-- new permutation parser that includes @p@. 

(<||>) :: StreamPermParser (a -> b) -> Parser a -> StreamPermParser b
(<||>) = add

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is not allowed to accept empty input - use the optional
-- combinator ('<$?>') instead.
--
-- If the function @f@ takes more than one parameter, the type variable
-- @b@ is instantiated to a functional type which combines nicely with
-- the adds parser @p@ to the ('<||>') combinator. This
-- results in stylized code where a permutation parser starts with a
-- combining function @f@ followed by the parsers. The function @f@
-- gets its parameters in the order in which the parsers are specified,
-- but actual input can be in any order.

(<$$>) :: (a -> b) -> Parser a -> StreamPermParser b
(<$$>) f p = newperm f <||> p

-- | The expression @perm \<||> (x,p)@ adds parser @p@ to the
-- permutation parser @perm@. The parser @p@ is optional - if it can
-- not be applied, the default value @x@ will be used instead. Returns
-- a new permutation parser that includes the optional parser @p@. 

(<|?>) :: StreamPermParser (a -> b) -> (a, Parser a) -> StreamPermParser b
(<|?>) perm (x,p) = addopt perm x p

-- | The expression @f \<$?> (x,p)@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is optional - if it can not be applied, the default value
-- @x@ will be used instead. 

(<$?>) :: (a -> b) -> (a, Parser a) -> StreamPermParser b
(<$?>) f (x,p) = newperm f <|?> (x,p)

{---------------------------------------------------------------
  The permutation tree
---------------------------------------------------------------}

-- | The type @StreamPermParser s st a@ denotes a permutation parser
-- that, when run with the 'parseKV' function, parses @s@ streams with
-- user state @st@ and returns a value of type @a@ on success.

data StreamPermParser a = Perm (Maybe a) [StreamBranch a]

data StreamBranch a = forall b. Branch (StreamPermParser (b -> a)) (Parser b)

-- Transform a permutation tree into a normal parser.
permute :: StreamPermParser a -> Parser a
permute (Perm def xs) = choice (map branch xs ++ empty)
  where empty = case def of Nothing -> []
                            Just x  -> [return x]

        branch (Branch perm p) = do x <- p
                                    f <- permute perm
                                    return (f x)

-- Build permutation trees.
newperm :: (a -> b) -> StreamPermParser (a -> b)
newperm f = Perm (Just f) []

add :: StreamPermParser (a -> b) -> Parser a -> StreamPermParser b
add perm@(Perm _mf fs) p = Perm Nothing (first:map insert fs)
  where first = Branch perm p
        insert (Branch perm' p') = Branch (add (mapPerms flip perm') p) p'

addopt :: StreamPermParser (a -> b) -> a -> Parser a -> StreamPermParser b
addopt perm@(Perm mf fs) x p =
  Perm (fmap ($ x) mf) (first:map insert fs)
  where first = Branch perm p
        insert (Branch perm' p') =
          Branch (addopt (mapPerms flip perm') x p) p'

mapPerms :: (a -> b) -> StreamPermParser a -> StreamPermParser b
mapPerms f (Perm x xs) = Perm (fmap f x) (map mapBranch xs)
  where mapBranch (Branch perm p) = Branch (mapPerms (f.) perm) p

-- | Parse a key-value string wrapper constructed via the permutation
-- parser combinators from 'Text.Parsec.Perm' and the parsers @value@
-- and @values@.
parseKV :: StreamPermParser a -> T.Text -> Either String a
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
