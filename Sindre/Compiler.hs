{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Visp.Compiler
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  nonportable
--
-- Transforming a Visp program into a callable function.
--
-----------------------------------------------------------------------------

module Visp.Compiler ( ClassMap
                     , Construction
                     , Constructor
                     , compileExpr
                     , construct
                     , compileVisp
                     )
    where

import Visp.Visp
import Visp.Runtime
import Visp.Util

import Control.Arrow
import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Array
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

compileAction :: MonadVisp m => Action -> m ()
compileAction (ExprAction expr) = compileExpr expr *> return ()

compileExpr :: MonadVisp m => Expr -> m Value
compileExpr (Literal v) = return v
compileExpr (Print []) = do
  printVal "\n"
  return $ IntegerV 0
compileExpr (Print [x]) = do
  printVal =<< show <$> compileExpr x
  printVal "\n"
  return $ IntegerV 0
compileExpr (Print (x:xs)) = do
  printVal =<< show <$> compileExpr x
  printVal " "
  compileExpr $ Print xs
compileExpr (MCall [obj] method args) = do
  wr <- lookupObj obj
  vs <- mapM compileExpr args
  operate wr $ \s -> callMethod s method vs
compileExpr (Var v) = do
  lookupVal v
compileExpr (Var k `Assign` e) = do
  bnd <- lookupVar k
  v   <- compileExpr e
  let add = modify $ \s ->
        s { varEnv = M.insert k (VarBnd v) (varEnv s) }
  case bnd of
    Just (ConstBnd _) ->
      error $ "Cannot reassign constant"
    _  -> add
  return v
compileExpr (s `FieldOf` oe `Assign` e) = do
  o <- compileExpr oe
  v <- compileExpr e
  case o of
    Reference wr -> do _ <- fieldSet wr s v
                       return v
    _            -> error "Not an object"
compileExpr Quit = quit *> return (IntegerV 0)
compileExpr (s `FieldOf` oe) = do
  o <- compileExpr oe
  case o of
    Reference wr -> do fieldGet wr s
    _            -> error "Not an object"
compileExpr (e1 `Plus` e2) = do
  x <- compileExpr e1
  y <- compileExpr e2
  case (x, y) of
    (IntegerV x', IntegerV y') -> return $ IntegerV (x'+y')
    _ -> error "Can only add integers"

evalConstExpr :: Expr -> Value
evalConstExpr (Literal v) = v
evalConstExpr _           = error "Not a const"

construct :: Widget m s => (s, InitVal m) -> Construction m
construct (s, v) = return (WidgetState s, v)

type Construction m = InitM m (WidgetState m, InitVal m)
type Constructor m =
    InitVal m -> WidgetArgs -> [(Maybe Orientation, WidgetRef)] -> Construction m
data InstGUI m = InstGUI (Maybe Identifier)
                         WidgetRef
                         (Constructor m)
                         WidgetArgs
                         [(Maybe Orientation, InstGUI m)]

initGUI :: (MonadVisp m, Functor m) =>
           InitVal m -> InstGUI m -> InitM m [(WidgetRef, WidgetState m)]
initGUI x (InstGUI _ wr f args cs) = do
  (s, x') <- f x args childrefs
  children <- liftM concat $ mapM (initGUI x') $ map snd cs
  return $ (wr, s):children
    where childrefs = map (\(o, InstGUI _ wr' _ _ _) -> (o, wr')) cs

envFromGUI :: InstGUI m -> VarEnvironment
envFromGUI = M.map (ConstBnd . Reference) . names
    where names (InstGUI v wr _ _ cs) =
            let m = maybe M.empty (flip M.singleton wr) v
            in m `M.union` names' cs
          names' = M.unions . map (names . snd)

type ClassMap m = M.Map Identifier (Constructor m)

lookupClass :: Identifier -> ClassMap m -> Constructor m
lookupClass k m = case M.lookup k m of
                    Just f -> f
                    Nothing -> error $ "Unknown class '" ++ k ++ "'"

instantiateGUI :: MonadVisp m => ClassMap m -> GUI -> (WidgetRef, InstGUI m)
instantiateGUI m = inst 0
    where inst r (GUI v c es cs) =
            ( lastwr
            , InstGUI v r (lookupClass c m) (M.map evalConstExpr es)
                          $ zip orients children )
                where (orients, childwrs) = unzip cs
                      (lastwr, children) =
                        mapAccumL (inst . (+1)) (r+length cs) childwrs

compileVisp :: (MonadVisp m, MonadIO m) => Program -> ClassMap m ->
               InitVal m -> Either String (InitM m (VispState m), m ())
compileVisp prog m root = Right (state, mainloop)
    where (lastwr, gui') = instantiateGUI m $ programGUI prog
          state = do ws <- liftM (map $ second WidgetSlot) $ initGUI root gui'
                     return VispState {
                                  objects = array (0, lastwr) ws
                                , varEnv  = envFromGUI gui'
                                }
          mainloop = do
            forever $ do
              fullRedraw
              handleEvent (programActions prog) =<< getEvent

handleEvent :: MonadVisp m => M.Map Pattern Action -> Event -> m ()
handleEvent m (KeyPress kp) = mapM_ execute $ filter applies $ M.toList m
    where applies (KeyPattern kp2, _) = kp == kp2
          applies _                   = False
          execute (_, act) = compileAction act
handleEvent _ _ = return ()
