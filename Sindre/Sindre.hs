{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  portable
--
-- Definitions for the Sindre programming language
--
-----------------------------------------------------------------------------

module Sindre.Sindre ( Identifier
                     , Orientation
                     , Point
                     , Rectangle(..)
                     , splitHoriz
                     , splitVert
                     , rectTranspose
                     , KeyModifier(..)
                     , Key
                     , KeyPress
                     , Stmt(..)
                     , Expr(..)
                     , WidgetRef
                     , ObjectRef
                     , rootWidget
                     , Value(..)
                     , Event(..)
                     , Source(..)
                     , Function(..)
                     , Pattern(..)
                     , Action(..)
                     , GUI(..)
                     , Program(..)
                     )
    where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data KeyModifier = Control
                 | Meta
                 | Super
                 | Hyper
                   deriving (Eq, Ord, Show)

type Key = String

type KeyPress = (S.Set KeyModifier, Key)

type Point = (Integer, Integer)

data Rectangle = Rectangle {
      rectCorner :: Point
    , rectWidth :: Integer
    , rectHeight :: Integer
    } deriving (Show, Eq, Ord)

rectTranspose :: Rectangle -> Rectangle
rectTranspose (Rectangle (x, y) w h) =
  Rectangle (y, x) h w

splitHoriz :: Rectangle -> Integer -> [Rectangle]
splitHoriz rect@(Rectangle (x1, y1) w h) parts = map part [0..parts-1]
    where part i | i == parts-1 =
            Rectangle (x1,y1+quant*i) w (h-(quant*i))
          part i | otherwise =
            Rectangle (x1,y1+quant*i) w quant
          quant = rectHeight rect `div` parts

splitVert :: Rectangle -> Integer -> [Rectangle]
splitVert r = map rectTranspose . splitHoriz (rectTranspose r)

type WidgetRef = Int
type ObjectRef = Int

rootWidget :: WidgetRef
rootWidget = 0

type Identifier = String
type Orientation = String

data Value = StringV  String
           | IntegerV Integer
           | Reference ObjectRef
           | Dict (M.Map Value Value)
             deriving (Eq, Ord)

instance Show Value where
  show (StringV s)  = s
  show (IntegerV v) = show v
  show (Reference r) = "#<Object at " ++ show r ++ ">"
  show (Dict m) = "{ " ++ intercalate "," elems ++ " }"
      where elems = map elemf $ M.toList m
            elemf (k, v) = show k ++ ": " ++ show v

data Stmt = Print [Expr]
          | Exit (Maybe Expr)
          | Return (Maybe Expr)
          | If Expr [Stmt] [Stmt]
          | While Expr [Stmt]
          | Expr Expr
            deriving (Show)

data Expr = Literal Value
          | Var Identifier
          | FieldOf Identifier Expr
          | Lookup Identifier Expr
          | LessThan Expr Expr
          | LessEql Expr Expr
          | Equal Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Assign Expr Expr
          | PostInc Expr
          | PostDec Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divided Expr Expr
          | Funcall Identifier [Expr]
          | Methcall Expr Identifier [Expr]
            deriving (Show, Eq, Ord)

data Event = KeyPress KeyPress
           | NamedEvent { eventName   :: Identifier
                        , eventValue  :: [Value]
                        }
             deriving (Show)

data Source = NamedSource Identifier
            | GenericSource Identifier Identifier
              deriving (Eq, Ord, Show)

data Pattern = KeyPattern KeyPress
             | OrPattern Pattern Pattern
             | SourcedPattern { patternSource :: Source
                              , patternEvent  :: Identifier
                              , patternVars   :: [Identifier]
                              }
               deriving (Eq, Ord, Show)

data Function = Function [Identifier] [Stmt]
              deriving (Show)

data Action = StmtAction [Stmt]
              deriving (Show)

type WidgetArgs = M.Map Identifier Expr

data GUI = GUI {
      widgetName :: (Maybe Identifier) 
    , widgetClass :: Identifier 
    , widgetArgs :: WidgetArgs
    , widgetChildren :: [(Maybe Orientation, GUI)]
    } deriving (Show)

data Program = Program {
      programGUI       :: GUI
    , programActions   :: [(Pattern, Action)]
    , programConstants :: [(Identifier, Expr)]
    , programFunctions :: M.Map Identifier Function
    } deriving (Show)
