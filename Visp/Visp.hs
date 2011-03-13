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
                 , WidgetBox(..)
                 , Widget(..)
                 , Event(..)
                 , Source(..)
                 , ObjectRef(..)
                 , WidgetRef(..)
                 , Pattern(..)
                 , Action(..)
                 , MonadVisp(..)
                 , Program(..)
                 )
    where

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

data Expr = Literal Value
          | Var String
          | FieldOf String Expr
          | Assign Expr Expr
          | Print [Expr]


data Event = KeyEvent KeyPress
           | SourcedEvent { eventSource :: WidgetRef
                          , eventName   :: Identifier
                          , eventValue  :: [Value]
                          }

data Source = NamedSource ObjectRef
            | GenericSource Identifier Identifier
              deriving (Eq, Ord)

data ObjectRef = WidgetRef WidgetRef
               | BuiltinRef String
                 deriving (Eq, Ord)

data Pattern = KeyPattern KeyPress
             | OrPattern Pattern Pattern
             | SourcedPattern { patternSource :: Source
                              , patternEvent  :: Identifier
                              , patternVars   :: [Identifier]
                              }
               deriving (Eq, Ord)

data Action = ExprAction Expr

data WidgetBox m = forall s . (Widget m s) =>
                   WidgetBox { widgetChildren :: [WidgetBox m]
                             , widgetState    :: s
                             }

type WidgetRef = [Int]

class ( Monad m
      , MonadReader (SubCfg m) m
      , Monad m) => MonadVisp m where
  type SubCfg m :: *
  type SubEvent m :: *
  
mutateWidgetBox :: MonadVisp m =>
                   (forall s . Widget m s => s -> m (a, s)) ->
                   WidgetBox m -> WidgetRef -> m (a, WidgetBox m)
mutateWidgetBox f w@(WidgetBox cs s) [] = do
  (evs, s') <- f s
  return (evs, WidgetBox cs s')
mutateWidgetBox f w@(WidgetBox cs s) (r:rs) = do
  case splitAt r cs of
    (bef, w:aft) -> do
      (evs, w') <- mutateWidgetBox f w rs
      return (evs, WidgetBox (bef++w':aft) s)
    _            -> error "Bad index"
  

delegateEvent :: Widget m s => WidgetBox m ->
                 WidgetRef -> Event -> m ([Event], WidgetBox m)
delegateEvent w rs e = mutateWidgetBox (flip recvEvent e) w rs 

delegateRawEvent :: Widget m s => WidgetBox m ->
                    WidgetRef -> SubEvent m -> m ([Event], WidgetBox m)
delegateRawEvent w rs e = mutateWidgetBox (flip recvRawEvent e) w rs

class MonadVisp m => Widget m s where
    fieldSet     :: s -> Identifier -> Value -> m s
    fieldGet     :: s -> Identifier -> Value
    compose      :: s -> m Rectangle
    draw         :: s -> Rectangle -> m s
    recvRawEvent :: s -> SubEvent m -> m ([Event], s)
    recvEvent    :: s -> Event -> m ([Event], s)

data Program m = Program {
      widgets   :: WidgetBox m
    , actions   :: M.Map Pattern Action
    }
