{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
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
                     , Rectangle(..)
                     , Dim(..)
                     , SpaceNeed
                     , SpaceUse
                     , fitRect
                     , sumPrim
                     , sumSec
                     , splitHoriz
                     , splitVert
                     , rectTranspose
                     , align
                     , Align(..)
                     , adjustRect
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
                     , Value(..)
                     , true
                     , truth
                     , falsity
                     , Event(..)
                     , EventSource(..)
                     , Source(..)
                     , Function(..)
                     , Pattern(..)
                     , Action(..)
                     , GUI(..)
                     , GUIRoot
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
      rectX      :: Integer
    , rectY      :: Integer
    , rectWidth  :: Integer
    , rectHeight :: Integer
    } deriving (Show, Eq)

rectTranspose :: Rectangle -> Rectangle
rectTranspose (Rectangle x y w h) = Rectangle y x h w

zipper :: (([a], a, [a]) -> ([a], a, [a])) -> [a] -> [a]
zipper f = zipper' []
    where zipper' a (x:xs) = let (a', x', xs') = f (a, x, xs)
                             in zipper' (x':a') xs'
          zipper' a [] = reverse a

splitHoriz :: Rectangle -> [Dim] -> [Rectangle]
splitHoriz (Rectangle x1 y1 w h) parts =
    snd $ mapAccumL mkRect y1 $ map fst $
        zipper adjust $ zip (divide h nparts) parts
    where nparts = genericLength parts
          mkRect y h' = (y+h', Rectangle x1 y w h')
          frob d (v, Min mv) = let v' = max mv $ v - d
                               in ((v', Min mv), v'-v+d)
          frob d (v, Max mv) = let v' = min mv $ v - d
                               in ((v', Max mv), v'-v+d)
          frob d (v, Unlimited) = ((v-d, Unlimited), 0)
          nunlims = genericLength . filter ((==Unlimited) . snd)
          frobunlim (d:ds) (v, Unlimited) = (ds, (v - d, Unlimited))
          frobunlim a x = (a, x)
          obtain v bef aft = let q = divide v (nparts-1)
                                 n = length bef
                                 (bef', x) = unzip $ zipWith frob q bef
                                 (aft', y) = unzip $ zipWith frob (drop n q) aft
                                 q' = divide (sum x+sum y) $ max 1 $ nunlims $ bef'++aft'
                                 (q'', bef'') = mapAccumL frobunlim q' bef'
                                 (_, aft'')   = mapAccumL frobunlim q'' aft'
                             in  (bef'',aft'')
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

data Dim = Min Integer | Max Integer | Unlimited
         deriving (Eq, Show, Ord)

type SpaceNeed = (Dim, Dim)
type SpaceUse = [Rectangle]

fitRect :: Rectangle -> SpaceNeed -> Rectangle
fitRect (Rectangle x y w h) (wn, hn) =
  Rectangle x y (fit w wn) (fit h hn)
    where fit d dn = case dn of
                      Max dn' -> min dn' d
                      Min dn' -> max dn' d
                      Unlimited -> d

sumPrim :: [Dim] -> Dim
sumPrim = foldl f (Min 0)
    where f (Min x) (Min y) = Min (x+y)
          f (Min x) (Max y) = Max (x+y)
          f (Max x) (Max y) = Max (x+y)
          f _ Unlimited = Unlimited
          f x _ = x

sumSec :: [Dim] -> Dim
sumSec = foldl f (Min 0)
    where f (Min x) (Min y) = Min $ max x y
          f (Max x) (Max y) = Max $ max x y
          f (Min x) (Max y) | y >= x = Max y
          f (Min x) (Max y) | x > y = Max x
          f _ Unlimited = Unlimited
          f x _ = x

data Align = AlignNeg | AlignPos | AlignCenter
             deriving (Show, Eq)

align :: Integral a => Align -> a -> a -> a -> a
align AlignCenter minp d maxp = minp + (maxp - minp - d) `div` 2
align AlignNeg minp _ _ = minp
align AlignPos _ d maxp = maxp - d

adjustRect :: (Align, Align) -> Rectangle -> Rectangle -> Rectangle
adjustRect (walign, halign) (Rectangle sx sy sw sh) (Rectangle _ _ w h) =
    Rectangle cx' cy' w h
    where cx' = frob walign sx w sw
          cy' = frob halign sy h sh
          frob AlignCenter c d maxv = c + (maxv - d) `div` 2
          frob AlignNeg c _ _ = c
          frob AlignPos c d maxv = c + maxv - d

data KeyModifier = Control
                 | Meta
                 | Super
                 | Hyper
                 | Shift
                   deriving (Eq, Ord, Show)

data Key = CharKey Char | CtrlKey String
    deriving (Show, Eq, Ord)

type KeyPress = (S.Set KeyModifier, Key)

type ObjectNum = Int
type ObjectRef = (ObjectNum, Identifier)
type WidgetRef = ObjectRef

type Identifier = String

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

data EventSource = FieldSrc ObjectRef Identifier
                 | ObjectSrc ObjectRef
                 | BackendSrc
                   deriving (Show)

data Source = NamedSource Identifier (Maybe Identifier)
            | GenericSource Identifier Identifier (Maybe Identifier)
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
    , widgetChildren :: [(Maybe Value, GUI)]
    } deriving (Show)

type GUIRoot = (Maybe Value, GUI)

type SindreOption = OptDescr (Arguments -> Arguments)
type Arguments = M.Map String String

data Program = Program {
      programGUI       :: GUIRoot
    , programActions   :: [P (Pattern, Action)]
    , programGlobals   :: [P (Identifier, P Expr)]
    , programOptions   :: [P (Identifier, (SindreOption, Maybe Value))]
    , programFunctions :: [P (Identifier, Function)]
    , programBegin     :: [P Stmt]
    }
