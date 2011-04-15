{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
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
                     , Dim(..)
                     , SpaceNeed
                     , SpaceUse
                     , splitHoriz
                     , splitVert
                     , rectTranspose
                     , KeyModifier(..)
                     , Key(..)
                     , KeyPress
                     , P(..)
                     , at
                     , SourcePos
                     , nowhere
                     , Stmt(..)
                     , Expr(..)
                     , ObjectNum
                     , ObjectRef
                     , WidgetRef
                     , rootWidget
                     , Value(..)
                     , true
                     , truth
                     , falsity
                     , Event(..)
                     , Source(..)
                     , Function(..)
                     , Pattern(..)
                     , Action(..)
                     , GUI(..)
                     , Program(..)
                     , SindreOption
                     , Arguments
                     )
    where

import Sindre.Util

import System.Console.GetOpt

import Control.Applicative
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data Rectangle = Rectangle {
      rectCorner :: Point
    , rectWidth :: Integer
    , rectHeight :: Integer
    } deriving (Show, Eq)

rectTranspose :: Rectangle -> Rectangle
rectTranspose (Rectangle (x, y) w h) =
  Rectangle (y, x) h w

zipper :: (([a], a, [a]) -> ([a], a, [a])) -> [a] -> [a]
zipper f = zipper' []
    where zipper' a (x:xs) = let (a', x', xs') = f (a, x, xs)
                             in zipper' (x':a') xs'
          zipper' a [] = reverse a

splitHoriz :: Rectangle -> [Dim] -> [Rectangle]
splitHoriz (Rectangle (x1, y1) w h) parts =
    snd $ mapAccumL mkRect y1 $ map fst $
        zipper adjust $ zip (divide h nparts) parts
    where nparts = genericLength parts
          mkRect y h' = (y+h', Rectangle (x1, y) w h')
          frob d (v, Min mv) =
              let v' = max mv $ v - d
              in ((v', Min mv), d - v' + v)
          frob d (v, Max mv) =
              let v' = min mv $ v - d
              in ((v', Max mv), d - v' + v)
          frob d (v, Unlimited) =
              let v' = v - d
              in ((v', Unlimited), 0)
          obtain v bef aft = let q = divide v (nparts-1)
                                 n = length bef
                                 (bef', _) = unzip $ zipWith frob q bef
                                 (aft', _) = unzip $ zipWith frob (drop n q) aft
                             in  (bef',aft')
          adjust (bef, (v, Min mv), aft)
              | v < mv =
                  let (bef', aft') = obtain (mv-v) bef aft
                  in (bef', (mv, Min mv), aft')
          adjust (bef, (v, Max mv), aft)
              | v > mv =
                  let (bef', aft') = obtain (mv-v) bef aft
                  in (bef', (mv, Max mv), aft')
          adjust x = x

splitVert :: Rectangle -> [Dim] -> [Rectangle]
splitVert r = map rectTranspose . splitHoriz (rectTranspose r)

data Dim = Max Integer | Min Integer | Unlimited
         deriving (Eq, Show, Ord)

type SpaceNeed = (Dim, Dim)
type SpaceUse = [Rectangle]

data KeyModifier = Control
                 | Meta
                 | Super
                 | Hyper
                   deriving (Eq, Ord, Show)

data Key = CharKey Char | CtrlKey String
    deriving (Show, Eq, Ord)

type KeyPress = (S.Set KeyModifier, Key)

type Point = (Integer, Integer)

type ObjectNum = Int
type ObjectRef = (ObjectNum, Identifier)
type WidgetRef = ObjectRef

rootWidget :: ObjectRef
rootWidget = (0, "root")

type Identifier = String
type Orientation = String

data Value = StringV  String
           | IntegerV Integer
           | Reference ObjectRef
           | Dict (M.Map Value Value)
             deriving (Eq, Ord, Show)

true :: Value -> Bool
true (IntegerV 0) = False
true _ = True

truth, falsity :: Value
truth = IntegerV 1
falsity = IntegerV 0

data Stmt = Print [P Expr]
          | Exit (Maybe (P Expr))
          | Return (Maybe (P Expr))
          | Next
          | If (P Expr) [P Stmt] [P Stmt]
          | While (P Expr) [P Stmt]
          | Expr (P Expr)
          | Focus (P Expr)
            deriving (Show, Eq)

type SourcePos = (String, Int, Int)

nowhere :: SourcePos
nowhere = ("<nowhere>", 0, 0)

data P a = P { sourcePos :: SourcePos, unP :: a }
    deriving (Show, Eq, Ord, Functor)

at :: Expr -> P Expr -> P Expr
at e1 e2 = const e1 <$> e2

data Expr = Literal Value
          | Var Identifier
          | FieldOf Identifier (P Expr)
          | Lookup Identifier (P Expr)
          | Not (P Expr)
          | LessThan (P Expr) (P Expr)
          | LessEql (P Expr) (P Expr)
          | Equal (P Expr) (P Expr)
          | And (P Expr) (P Expr)
          | Or (P Expr) (P Expr)
          | Assign (P Expr) (P Expr)
          | PostInc (P Expr)
          | PostDec (P Expr)
          | Plus (P Expr) (P Expr)
          | Minus (P Expr) (P Expr)
          | Times (P Expr) (P Expr)
          | Divided (P Expr) (P Expr)
          | Modulo (P Expr) (P Expr)
          | RaisedTo (P Expr) (P Expr)
          | Funcall Identifier [P Expr]
          | Methcall (P Expr) Identifier [P Expr]
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

data Function = Function [Identifier] [P Stmt]
              deriving (Show, Eq)

data Action = StmtAction [P Stmt]
              deriving (Show)

type WidgetArgs = M.Map Identifier (P Expr)

data GUI = GUI {
      widgetName :: Maybe Identifier
    , widgetClass :: P Identifier
    , widgetArgs :: WidgetArgs
    , widgetChildren :: [(Maybe Orientation, GUI)]
    } deriving (Show)

type SindreOption = OptDescr (Arguments -> Arguments)
type Arguments = M.Map String String

data Program = Program {
      programGUI       :: GUI
    , programActions   :: [P (Pattern, Action)]
    , programGlobals   :: [P (Identifier, P Expr)]
    , programOptions   :: [P (Identifier, (SindreOption, Maybe Value))]
    , programFunctions :: [P (Identifier, Function)]
    , programBegin     :: [P Stmt]
    }
