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

import Text.Parsec
import Text.Parsec.String

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
