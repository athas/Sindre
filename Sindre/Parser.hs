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

data Directive = GUIDirective GUI
               | ActionDirective (Pattern, Action)

getGUIs :: [Directive] -> [GUI]
getGUIs = foldl f []
    where f l (GUIDirective x) = x:l
          f l _                = l

getActions :: [Directive] -> [(Pattern, Action)]
getActions = foldl f []
    where f l (ActionDirective x) = x:l
          f l _                   = l

applyDirectives :: [Directive] -> Program -> Either String Program
applyDirectives ds prog =
  case getGUIs ds of
    [gui'] -> Right prog {
                    programGUI = gui'
                  , programActions =
                      getActions ds ++ programActions prog
                  }
    []    -> Right prog {
                  programActions =
                      getActions ds ++ programActions prog
                  }
    l     -> Left $ "Multiple GUI definitions" ++ show l

parseSindre :: Program -> SourceName -> String -> Either ParseError Program
parseSindre prog = parse (sindre prog)

sindre :: Program -> Parser Program
sindre prog = do ds <- reverse <$> many directive <* eof
                 either fail return $ applyDirectives ds prog

directive :: Parser Directive
directive =     ActionDirective <$> reaction
            <|> GUIDirective <$> gui

gui :: Parser GUI
gui = do
  name' <- try name <|> pure Nothing
  clss <- identifier
  args' <- M.fromList <$> args <|> pure M.empty
  children' <- children <|> pure []
  return GUI { widgetName = name'
             , widgetClass = clss
             , widgetArgs = args'
             , widgetChildren = children'
             }
    where name = Just <$> identifier <* reservedOp "="
          args = parens $ commaSep arg
          arg = pure (,) <*> identifier <* reservedOp "=" <*> expression
          children = braces $ many child
          child = ((,) Nothing) <$> gui

reaction :: Parser (Pattern, Action)
reaction = pure (,) <*> try pattern <*> action

pattern :: Parser Pattern
pattern = simplepat `chainl1` (reservedOp "||" *> pure OrPattern)
    where simplepat =
                pure KeyPattern <*>
                     (reservedOp "<" *> keypress <* reservedOp ">")
            <|> pure SourcedPattern
                    <*> source <* char '.'
                    <*> identifier
                    <*> parens (commaSep identifier)

source :: Parser Source
source = NamedSource <$> identifier

action :: Parser Action
action = StmtAction <$> braces statements

key :: Parser Key
key = identifier

modifier :: Parser KeyModifier
modifier =     string "C" *> return Control
           <|> string "M" *> return Meta
           <|> string "S" *> return Super
           <|> string "H" *> return Hyper

keypress :: Parser KeyPress
keypress = pure (,) <*> (S.fromList <$> many (try modifier <* char '-')) <*> key

statements :: Parser [Stmt]
statements = many (statement <* skipMany semi) <?> "statement"

statement :: Parser Stmt
statement = (    try printstmt
             <|> try quitstmt
             <|> Expr <$> expression)
    where printstmt = reserved "print" *>
                      (Print <$> commaSep expression)
          quitstmt  = reserved "exit" *>
                      (Exit <$> (Just <$> expression <|> pure Nothing))

keywords :: [String]
keywords = ["if", "else", "while", "for", "do",
            "function", "return", "continue", "break",
            "exit", "print"]

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
           , reservedOpNames = [ "+", "-", "/", "*", "&&", "||", ";", ","
                               , "<", ">", "<=", ">="]
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
comma :: Parser String
comma = P.comma lexer
commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer
semi :: Parser String
semi = P.semi lexer
parens :: Parser a -> Parser a
parens = P.parens lexer
braces :: Parser a -> Parser a
braces = P.braces lexer
fcall :: Parser Expr
fcall = pure Funcall <*> identifier <*>
        parens (sepBy expression comma)
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
reserved :: String -> Parser ()
reserved = P.reserved lexer
