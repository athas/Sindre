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
                 , Point(..)
                 , Rectangle(..)
                 , height
                 , width
                 , Expr(..)
                 , Value(..)
                 , Event(..)
                 , Source(..)
                 , Pattern(..)
                 , Action(..)
                 , GUI(..)
                 , Program(..)
                 )
    where

import Control.Applicative
import "monads-fd" Control.Monad.Trans
import "monads-fd" Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

data KeyModifier = Control
                 | Meta
                 | Super
                 | Hyper
                   deriving (Eq, Ord)

data Key = CharacterKey String
         | CommandKey   String
           deriving (Eq, Ord)

type KeyPress = (S.Set KeyModifier, Key)

type Point = (Integer, Integer)

data Rectangle = Rectangle Point Point

width :: Rectangle -> Integer
width (Rectangle (x1, _) (x2, _)) = abs (x2 - x1)

height :: Rectangle -> Integer
height (Rectangle (_, y1) (_, y2)) = abs (y2 - y1)

type Identifier = String

data Value = StringV  String
           | IntegerV Integer
           | NullV

data Expr = Literal Value
            | Var String
            | FieldOf String Expr
            | Assign Expr Expr
            | Print [Expr]
            | FCall Identifier [Expr]
            | MCall [Identifier] Identifier [Expr]

data Event = KeyEvent KeyPress
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

data GUI = GUI (Maybe Identifier) Identifier [Expr] [GUI]

data Program = Program {
      gui      :: GUI
    , actions  :: M.Map Pattern Action
    }
