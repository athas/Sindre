{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
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
-- Definitions for the Visp programming language
--
-----------------------------------------------------------------------------

module Visp.Visp ( Identifier
                 , Point
                 , Rectangle(..)
                 , splitHoriz
                 , splitVert
                 , rectTranspose
                 , Key(..)
                 , Expr(..)
                 , WidgetRef
                 , rootWidget
                 , Value(..)
                 , Event(..)
                 , Source(..)
                 , Pattern(..)
                 , Action(..)
                 , GUI(..)
                 , Program(..)
                 )
    where

import qualified Data.Map as M
import qualified Data.Set as S

data KeyModifier = Control
                 | Meta
                 | Super
                 | Hyper
                   deriving (Eq, Ord, Show)

data Key = CharacterKey String
         | CommandKey   String
           deriving (Eq, Ord, Show)

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

type WidgetRef = [Int]

rootWidget :: WidgetRef
rootWidget = []

type Identifier = String

data Value = StringV  String
           | IntegerV Integer
           | Reference WidgetRef
             deriving (Eq, Ord)

instance Show Value where
  show (StringV s)  = s
  show (IntegerV v) = show v
  show (Reference r) = "#<Object at " ++ show r ++ ">"

data Expr = Literal Value
            | Var String
            | FieldOf String Expr
            | Assign Expr Expr
            | Plus Expr Expr
            | Print [Expr]
            | FCall Identifier [Expr]
            | MCall [Identifier] Identifier [Expr]
            | Quit

data Event = KeyPress KeyPress
           | SourcedEvent { eventSource :: Identifier
                          , eventName   :: Identifier
                          , eventValue  :: [Value]
                          }

data Source = NamedSource Identifier
            | GenericSource Identifier Identifier
              deriving (Eq, Ord)

data Pattern = KeyPattern KeyPress
             | OrPattern Pattern Pattern
             | SourcedPattern { patternSource :: Source
                              , patternEvent  :: Identifier
                              , patternVars   :: [Identifier]
                              }
               deriving (Eq, Ord)

data Action = ExprAction Expr

type WidgetArgs = M.Map Identifier Expr

data GUI = GUI {
      widgetName :: (Maybe Identifier) 
    , widgetClass :: Identifier 
    , widgetArgs :: WidgetArgs
    , widgetChildren :: [GUI]
    }

data Program = Program {
      programGUI      :: GUI
    , programActions  :: M.Map Pattern Action
    }
