{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Formatting
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Parser and definition of the dzen2-inspired formatting language
-- used by Sindre.  A format string is a sequence of commands changing
-- drawing option parameters, and things to draw.
--
-----------------------------------------------------------------------------
module Sindre.Formatting( Format(..)
                        , FormatString
                        , textContents
                        , startBg
                        , parseFormatString
                        , unparseFormatString
                        )
    where

import qualified Sindre.Sindre as Sindre
import Sindre.Runtime (Mold(..))

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import Control.Applicative
import Data.Maybe

-- | A formatting command is either a change to the drawing state, or
-- a string to be printed at the current location.
data Format = Fg String -- ^ Draw text in the given colour.
            | DefFg -- ^ Draw text in the default colour.
            | Bg String -- ^ Draw the background in the given colour.
            | DefBg -- ^ Draw the background in the default colour.
            | Text String -- ^ Draw the given string.

-- | A list of formatting commands, interpreted left-to-right.
type FormatString = [Format]

instance Mold FormatString where
  mold v = either (const Nothing) Just . parseFormatString "input" =<< mold v
  unmold = Sindre.string . unparseFormatString

-- | The human-readable part of a format string, with formatting
-- directives stripped.
textContents :: FormatString -> String
textContents = concatMap txt
  where txt (Text s) = s
        txt _        = ""

-- | The first background colour specified in the format string, if
-- any.
startBg :: FormatString -> Maybe String
startBg = listToMaybe . concatMap ofbg
  where ofbg (Bg bg) = [bg]
        ofbg _       = []

-- | Prettyprint a 'FormatString' to a string that, when parsed by
-- 'parseFormatString', results in the original 'FormatString'
unparseFormatString :: FormatString -> String
unparseFormatString = concatMap f
  where f (Fg s)   = "fg(" ++ s ++ ")"
        f DefFg    = "fg()"
        f (Bg s)   = "bg(" ++ s ++ ")"
        f DefBg    = "bg()"
        f (Text s) = concatMap g s
          where g '^' = "^^"
                g c   = [c]

-- | Parse a format string, returning either an error message or the
-- result of the parse.
parseFormatString :: SourceName -> String -> Either ParseError FormatString
parseFormatString = parse $ many format <* eof

format :: Parser Format
format = char '^' *> command <|> text

text :: Parser Format
text = Text <$> many1 (noneOf "^")

command :: Parser Format
command =     char '^' *> pure (Text "^")
          <|> string "fg(" *> (Fg <$> many1 (noneOf ")") <|> pure DefFg)
                           <* string ")"
          <|> string "bg(" *> (Bg <$> many1 (noneOf ")") <|> pure DefBg)
                           <* string ")"
