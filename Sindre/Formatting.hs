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
                        )
    where

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String

import Control.Applicative
import Control.Monad.Identity
import Data.Char hiding (Control)
import Data.Function
import Data.List hiding (insert)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- | A formatting command is either a change to the drawing state, or
-- a string to be printed at the current location.
data Format = Fg String -- ^ Draw text in the given colour.
            | DefFg -- ^ Draw text in the default colour.
            | Bg String -- ^ Draw the background in the given colour.
            | DefBg -- ^ Draw the background in the default colour.
            | Text String -- ^ Draw the given string.
              deriving (Show)

-- | A list of formatting commands, interpreted left-to-right.
type FormatString = [Format]

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
