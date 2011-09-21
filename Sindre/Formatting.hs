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

import Sindre.Sindre hiding (string)
import Sindre.Runtime (Mold(..))

import Data.Attoparsec.Text

import Control.Applicative hiding (many)
import Control.Monad
import Data.Maybe
import qualified Data.Text as T

import Prelude hiding (takeWhile)

-- | A formatting command is either a change to the drawing state, or
-- a string to be printed at the current location.
data Format = Fg String -- ^ Draw text in the given colour.
            | DefFg -- ^ Draw text in the default colour.
            | Bg String -- ^ Draw the background in the given colour.
            | DefBg -- ^ Draw the background in the default colour.
            | Text T.Text -- ^ Draw the given string.
              deriving (Show, Eq, Ord)

-- | A list of formatting commands, interpreted left-to-right.
type FormatString = [Format]

instance Mold FormatString where
  mold v = either (const Nothing) Just . parseFormatString =<< mold v
  unmold = StringV . unparseFormatString

-- | The human-readable part of a format string, with formatting
-- directives stripped.
textContents :: FormatString -> T.Text
textContents = T.concat . map txt
  where txt (Text s) = s
        txt _        = T.empty

-- | The first background colour preceding any default background
-- colour or text entry specified in the format string, if any.
startBg :: FormatString -> Maybe String
startBg = getBg <=< listToMaybe . dropWhile ign
  where ign (Text _) = False
        ign DefBg    = False
        ign (Bg _)   = False
        ign _        = True
        getBg (Bg bg) = Just bg
        getBg _       = Nothing

-- | Prettyprint a 'FormatString' to a string that, when parsed by
-- 'parseFormatString', results in the original 'FormatString'
unparseFormatString :: FormatString -> T.Text
unparseFormatString = T.concat . map f
  where f (Fg s)   = T.pack $ "fg(" ++ s ++ ")"
        f DefFg    = T.pack "fg()"
        f (Bg s)   = T.pack $ "bg(" ++ s ++ ")"
        f DefBg    = T.pack "bg()"
        f (Text s) = T.replace (T.pack "^") (T.pack "^^") s

-- | Parse a format string, returning either an error message or the
-- result of the parse.
parseFormatString :: T.Text -> Either String FormatString
parseFormatString s =
  eitherResult $ parse (many format <* endOfInput) s `feed` T.empty

format :: Parser Format
format = char '^' *> command <|> text

text :: Parser Format
text = Text <$> takeWhile1 (/='^')

command :: Parser Format
command =     Text <$> string (T.pack "^")
          <|> string (T.pack "fg(")
                *> (Fg <$> T.unpack <$> takeWhile1 (/=')') <|> pure DefFg)
                <* string (T.pack ")")
          <|> string (T.pack "bg(")
                *> (Bg <$> T.unpack <$> takeWhile1 (/=')') <|> pure DefBg)
                <* string (T.pack ")")
