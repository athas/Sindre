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

import Control.Monad.Identity
import Control.Applicative
import Data.Char hiding (Control)
import qualified Data.Map as M
import qualified Data.Set as S

data Directive = GUIDirective GUI
               | ActionDirective (Pattern, Action)
               | ConstDirective (Identifier, Expr)
               | FuncDirective (Identifier, Function)

getGUI :: [Directive] -> Either String (Maybe GUI)
getGUI ds  = case foldl f [] ds of
               [gui'] -> Right $ Just gui'
               []     -> Right Nothing
               _      -> Left "Multiple GUI definitions"
    where f l (GUIDirective x) = x:l
          f l _                = l

getActions :: [Directive] -> [(Pattern, Action)]
getActions = foldl f []
    where f l (ActionDirective x) = x:l
          f l _                   = l

getConsts :: [Directive] -> Either String [(Identifier, Expr)]
getConsts = foldM f []
    where f m (ConstDirective x) = Right $ insert x m
          f m _                  = Right m
          insert (name, e) m
              | name `elem` map fst m =
                  error "Duplicate constant definitions"
              | otherwise = (name, e):m

getFunctions :: [Directive] -> Either String (M.Map Identifier Function)
getFunctions = foldM f M.empty
    where f m (FuncDirective x) = insert x m
          f m _                 = Right m
          insert (name, e) m
              | name `M.member` m =
                  Left "Duplicate function definitions"
              | otherwise = Right $ M.insert name e m

applyDirectives :: [Directive] -> Program -> Either String Program
applyDirectives ds prog = do
  consts <- getConsts ds
  funcs  <- getFunctions ds
  let prog' = prog {
                programActions =
                    getActions ds ++ programActions prog
              , programConstants =
                  programConstants prog ++ consts
              , programFunctions =
                  funcs `M.union` programFunctions prog
              }
  case getGUI ds of
    Left e -> Left e
    Right (Just gui') -> Right prog' {
                            programGUI = gui'
                          }
    Right Nothing     -> Right prog'

parseSindre :: Program -> SourceName -> String -> Either ParseError Program
parseSindre prog = parse (sindre prog)

sindre :: Program -> Parser Program
sindre prog = do ds <- reverse <$> many directive <* eof
                 either fail return $ applyDirectives ds prog

directive :: Parser Directive
directive = directive' <* skipMany semi
    where directive' =     ActionDirective <$> reaction
                       <|> GUIDirective <$> gui
                       <|> ConstDirective <$> constdef
                       <|> FuncDirective <$> functiondef

gui :: Parser GUI
gui = reserved "GUI" *> braces gui'
    where gui' = do
            name' <- try name <|> pure Nothing
            clss <- className
            args' <- M.fromList <$> args <|> pure M.empty
            children' <- children <|> pure []
            return GUI { widgetName = name'
                       , widgetClass = clss
                       , widgetArgs = args'
                       , widgetChildren = children'
                       }
          name = Just <$> varName <* reservedOp "="
          args = parens $ commaSep arg
          arg = pure (,) <*> varName <* reservedOp "=" <*> expression
          children = braces $ many child
          child = (,) Nothing <$> gui'

functiondef :: Parser (Identifier, Function)
functiondef = reserved "function" *> pure (,) <*> try varName <*> function
    where function = pure Function 
                     <*> parens (commaSep varName)
                     <*> braces statements
                     
reaction :: Parser (Pattern, Action)
reaction = pure (,) <*> try pattern <*> action

constdef :: Parser (Identifier, Expr)
constdef = pure (,) <*> try varName <* reservedOp "=" <*> expression

pattern :: Parser Pattern
pattern = simplepat `chainl1` (reservedOp "||" *> pure OrPattern)
    where simplepat =
                pure KeyPattern <*>
                     (reservedOp "<" *> keypress <* reservedOp ">")
            <|> pure SourcedPattern
                    <*> source <* char '.'
                    <*> varName
                    <*> parens (commaSep varName)

source :: Parser Source
source = NamedSource <$> varName

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
statement =      printstmt
             <|> quitstmt
             <|> returnstmt
             <|> reserved "next" *> pure Next
             <|> ifstmt
             <|> whilestmt
             <|> Expr <$> expression
    where printstmt = reserved "print" *>
                      (Print <$> commaSep expression)
          quitstmt  = reserved "exit" *>
                      (Exit <$> (Just <$> expression <|> pure Nothing))
          returnstmt = reserved "return" *>
                       (Return <$> (Just <$> expression <|> pure Nothing))
          ifstmt = (reserved "if" *> pure If)
                   <*> parens expression 
                   <*> braces statements
                   <*> (    reserved "else" *> braces statements
                        <|> return [])
          whilestmt = (reserved "while" *> pure While)
                      <*> parens expression
                      <*> braces statements

keywords :: [String]
keywords = ["if", "else", "while", "for", "do",
            "function", "return", "continue", "break",
            "exit", "print", "GUI"]

sindrelang :: LanguageDef ()
sindrelang = LanguageDef {
             commentStart = "/*"
           , commentEnd = "*/"
           , commentLine = "//"
           , nestedComments = True
           , identStart = letter
           , identLetter = alphaNum <|> char '_'
           , opStart = oneOf "+-/*&|;,<>"
           , opLetter = oneOf "=+-|&"
           , reservedNames = keywords
           , reservedOpNames = [ "++", "--", "+", "-", "/", "*"
                               , "&&", "||", ";", ","
                               , "<", ">", "<=", ">="
                               , "=", "*=", "/=", "+=", "-="]
           , caseSensitive = True
  }

operators :: OperatorTable String () Identity Expr
operators = [ [ prefix  "-" $ inplace Times $ Literal $ IntegerV $ -1 
              , prefix  "+" id ]
            , [ prefix "++" $
                flip (inplace Plus) $ Literal $ IntegerV 1
              , postfix "++" PostInc
              , prefix "--" $
                flip (inplace Plus) $ Literal $ IntegerV $ -1 
              , postfix "--" PostDec ]
            , [ binary "*" Times AssocLeft, binary "/" Divided AssocLeft ]
            , [ binary "+" Plus AssocLeft, binary "-" Minus AssocLeft ]
            , [ binary "==" Equal AssocNone 
              , binary "<" LessThan AssocNone 
              , binary ">" (flip LessThan) AssocNone 
              , binary "<=" LessEql AssocNone
              , binary ">=" (flip LessEql) AssocNone]
            , [ binary "&&" And AssocRight ]
            , [ binary "||" Or AssocRight ]
            , [ binary "=" Assign AssocRight
              , binary "*=" (inplace Times) AssocLeft
              , binary "/=" (inplace Divided) AssocLeft
              , binary "+=" (inplace Plus) AssocLeft
              , binary "-=" (inplace Minus) AssocLeft]
            ]
    where binary  name fun       = Infix (reservedOp name >> return fun)
          prefix  name fun       = Prefix (reservedOp name >> return fun)
          postfix name fun       = Postfix (reservedOp name >> return fun)
          inplace op e1 e2       = e1 `Assign` (e1 `op` e2)

expression :: Parser Expr
expression = buildExpressionParser operators term <?> "expression"
    where term = try atomic <|> compound

atomic :: Parser Expr
atomic =     parens expression
         <|> integer
         <|> stringLiteral
         <|> dictlookup

compound :: Parser Expr
compound =
  field' `chainl1` (char '.' *> pure comb)
    where comb e (Var v) = FieldOf v e
          comb e (Funcall v es) = Methcall e v es
          comb _ _ = undefined -- Will never happen
          field' = try fcall <|> Var <$> varName

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
brackets :: Parser a -> Parser a
brackets = P.brackets lexer
fcall :: Parser Expr
fcall = pure Funcall <*> varName <*>
        parens (sepBy expression comma)
dictlookup :: Parser Expr
dictlookup = pure Lookup <*> varName <*>
             brackets expression
className :: Parser String
className = lookAhead (satisfy isUpper) *> identifier <?> "class"
varName :: Parser String
varName = lookAhead (satisfy isLower) *> identifier <?> "variable"
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
