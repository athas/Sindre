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
-- Parser for the Sindre programming language
--
-----------------------------------------------------------------------------

module Sindre.Parser(parseSindre)
    where

import Sindre.Sindre

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

parseSindre :: SourceName -> String -> Either ParseError Program
parseSindre = parse sindre

sindre :: Parser Program
sindre = do pgui <- gui
            pacts <- actions
            return Program {
                         programGUI = pgui
                       , programActions = pacts
              }

gui :: Parser GUI
gui = do
  name' <- try name <|> pure Nothing
  clss <- identifier
  args' <- M.fromList <$> args
  children' <- children <|> pure []
  lexeme (char ';')
  return GUI { widgetName = name'
             , widgetClass = clss
             , widgetArgs = args'
             , widgetChildren = children'
             }
    where name = Just <$> identifier <* lexeme (char '=')
          args = parens $ sepBy arg (lexeme $ char ',')
          arg = pure (,) <*> identifier <* lexeme (char '=') <*> expression
          children = braces $ many child
          child = ((,) Nothing) <$> gui

actions :: Parser (M.Map Pattern Action)
actions = M.fromList <$> many (pure (,) <*> pattern <*> action)

pattern :: Parser Pattern
pattern = pure KeyPattern <*> (lexeme (char '<') *> keypress <* lexeme (char '>'))

action :: Parser Action
action = ExprAction <$> braces exprs
    where exprs = sepBy expression (lexeme $ char ';')

key :: Parser Key
key = CharacterKey <$> (:[]) <$> lexeme alphaNum

modifier :: Parser KeyModifier
modifier = string "C" *> return Control

keypress :: Parser KeyPress
keypress = pure (,) <*> (S.fromList <$> many (try modifier <* char '-')) <*> key

keywords :: [String]
keywords = ["if", "else", "while", "for", "do",
            "function", "return", "continue", "break"]

sindrelang :: LanguageDef ()
sindrelang = LanguageDef {
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
            , [binary "=" Assign AssocRight ]
            ]
    where binary  name fun assoc = Infix (reservedOp name >> return fun) assoc
          prefix  name fun       = Prefix (reservedOp name >> return fun)
          postfix name fun       = Postfix (reservedOp name >> return fun)

expression :: Parser Expr
expression = buildExpressionParser operators term <?> "expression"
    where term = field <|> atomic

atomic :: Parser Expr
atomic =     parens expression
         <|> try fcall
         <|> Var <$> identifier
         <|> integer
         <|> stringLiteral

lexer :: P.TokenParser ()
lexer = P.makeTokenParser sindrelang
lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer
whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer
parens :: Parser a -> Parser a
parens = P.parens lexer
braces :: Parser a -> Parser a
braces = P.braces lexer
fcall :: Parser Expr
fcall = pure Funcall <*> identifier <*>
        parens (sepBy expression (lexeme $ char ','))
field :: Parser Expr
field =
  field' `chainl1` (char '.' *> pure comb)
    where comb e (Var v) = FieldOf v e
          comb e (Funcall v es) = Methcall e v es
          comb _ _ = undefined -- Will never happen
          field' = try fcall <|> Var <$> identifier
identifier :: Parser String
identifier = P.identifier lexer
integer :: Parser Expr
integer = Literal <$> IntegerV <$> P.integer lexer
stringLiteral :: Parser Expr
stringLiteral = Literal <$> StringV <$> P.stringLiteral lexer
reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer
