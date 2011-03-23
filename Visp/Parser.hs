{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  portable
--
-- Parser for the Visp programming language
--
-----------------------------------------------------------------------------

module Visp.Parser()
    where

import Visp.Visp

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Token (LanguageDef, GenLanguageDef(..))
import Text.Parsec.Expr

import "mtl" Control.Monad.Identity
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S

type Position = (Integer, Integer)
type PPos a = (a, Position)
type FieldMap = M.Map String Expr

data Initializer = Named FieldMap
                 | Sequence [Expr]

data WidgetDecl = WidgetDecl {
      widgetName :: Maybe Identifier
    , widgetClass :: Identifier
    , widgetInitializer :: Initializer
    , widgetChilren :: [WidgetDecl]
    }

keywords :: [String]
keywords = ["if", "else", "while", "for", "do",
            "function", "return", "continue", "break"]

visplang :: LanguageDef ()
visplang = LanguageDef {
             commentStart = "/*"
           , commentEnd = "*/"
           , commentLine = "//"
           , nestedComments = True
           , identStart = letter
           , identLetter = alphaNum <|> char '_'
           , opStart = oneOf ""
           , opLetter = oneOf ""
           , reservedNames = keywords
           , reservedOpNames = ["+", "-", "/", "*", "&&", "||"]
           , caseSensitive = True
  }
           
operators :: OperatorTable String () Identity Expr
operators = [ [binary "*" Times AssocLeft, binary "/" Divided AssocLeft ]
            , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft ]
            , [binary ":=" Assign AssocRight ]
            ]
    where binary  name fun assoc = Infix (reservedOp name >> return fun) assoc
          prefix  name fun       = Prefix (reservedOp name >> return fun)
          postfix name fun       = Postfix (reservedOp name >> return fun)

expression :: Parser Expr
expression = buildExpressionParser operators expr <?> "expression"
    where expr =     parens expression
                 <|> Var <$> identifier
                 <|> Literal <$> IntegerV <$> integer
                 <|> Literal <$> StringV <$> stringLiteral

lexer :: P.TokenParser ()
lexer = P.makeTokenParser visplang
parens :: Parser Expr -> Parser Expr
parens = P.parens lexer
identifier :: Parser String
identifier = P.identifier lexer
integer :: Parser Integer
integer = P.integer lexer
stringLiteral = P.stringLiteral lexer
reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer
