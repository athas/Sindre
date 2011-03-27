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
-- Module      :  Sindre.Compiler
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  nonportable
--
-- Transforming a Sindre program into a callable function.
--
-----------------------------------------------------------------------------

module Sindre.Compiler ( ClassMap
                       , Construction
                       , Constructor
                       , NewWidget(..)
                       , NewObject(..)
                       , compileExpr
                       , construct
                       , ObjectMap
                       , compileSindre
                       )
    where

import Sindre.Sindre
import Sindre.Runtime
import Sindre.Util

import System.Exit

import Control.Arrow
import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as Q

compileAction :: MonadSubstrate m => Action -> Sindre m ()
compileAction (StmtAction stmts) = mapM_ compileStmt stmts

compileStmt :: MonadSubstrate m => Stmt -> Sindre m ()
compileStmt (Print []) = do
  lift $ printVal "\n"
compileStmt (Print [x]) = do
  s <- show <$> compileExpr x
  lift $ do printVal s
            printVal "\n"
compileStmt (Print (x:xs)) = do
  v <- compileExpr x
  str <- case v of
           Reference wr -> fromMaybe (show v) <$> revLookup wr
           _            -> return $ show v
  lift $ do printVal str
            printVal " "
  compileStmt $ Print xs
compileStmt (Exit Nothing) = lift $ quit ExitSuccess
compileStmt (Exit (Just e)) = do
  v <- compileExpr e
  case v of
    IntegerV 0 -> lift $ quit ExitSuccess
    IntegerV x -> lift $ quit $ ExitFailure $ fi x
    _          -> error "Exit code must be an integer"
compileStmt (Expr e) = compileExpr e *> return ()

compileExpr :: MonadSubstrate m => Expr -> Sindre m Value
compileExpr (Literal v) = return v
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
compileExpr (_ `Assign` _) = error "Cannot assign to rvalue"
compileExpr (s `FieldOf` oe) = do
  o <- compileExpr oe
  case o of
    Reference wr -> fieldGet wr s
    _            -> error "Not an object"
compileExpr (Methcall oe meth argexps) = do
  argvs <- mapM compileExpr argexps
  o <- compileExpr oe
  case o of
    Reference wr -> callMethod wr meth argvs
    _            -> error "Not an object"
compileExpr (e1 `Plus` e2) = compileBinop (+) "add" e1 e2
compileExpr (e1 `Minus` e2) = compileBinop (-) "subtract" e1 e2
compileExpr (e1 `Times` e2) = compileBinop (*) "multiply" e1 e2
compileExpr (e1 `Divided` e2) = compileBinop div "divide" e1 e2

compileBinop :: MonadSubstrate m =>
                (Integer -> Integer -> Integer) ->
                String -> Expr -> Expr -> Sindre m Value
compileBinop op opstr e1 e2 = do
  x <- compileExpr e1
  y <- compileExpr e2
  case (x, y) of
    (IntegerV x', IntegerV y') -> return $ IntegerV (x' `op` y')
    _ -> error $ "Can only " ++ opstr ++ " integers"

evalConstExpr :: Expr -> Value
evalConstExpr (Literal v) = v
evalConstExpr _           = error "Not a const"

construct :: Widget m s => (s, InitVal m) -> Construction m
construct (s, v) = return (NewWidget s, v)

data NewWidget m = forall s . Widget m s => NewWidget s
data NewObject m = forall s . Object m s => NewObject s

toWslot :: NewWidget m -> DataSlot m
toWslot (NewWidget s) = WidgetSlot s
toOslot :: NewObject m -> DataSlot m
toOslot (NewObject s) = ObjectSlot s

type Construction m = m (NewWidget m, InitVal m)
type Constructor m =
    InitVal m -> WidgetArgs -> [(Maybe Orientation, WidgetRef)] -> Construction m
data InstGUI m = InstGUI (Maybe Identifier)
                         WidgetRef
                         (Constructor m)
                         WidgetArgs
                         [(Maybe Orientation, InstGUI m)]

initGUI :: MonadSubstrate m =>
           InitVal m -> InstGUI m -> m [(WidgetRef, NewWidget m)]
initGUI x (InstGUI _ wr f args cs) = do
  (s, x') <- f x args childrefs
  children <- liftM concat $ mapM (initGUI x') $ map snd cs
  return $ (wr, s):children
    where childrefs = map (\(o, InstGUI _ wr' _ _ _) -> (o, wr')) cs

revFromGUI :: InstGUI m -> M.Map WidgetRef Identifier
revFromGUI (InstGUI v wr _ _ cs) = m `M.union` names cs
    where m = maybe M.empty (M.singleton wr) v
          names = M.unions . map (revFromGUI . snd)

envFromGUI :: InstGUI m -> VarEnvironment
envFromGUI = M.map (ConstBnd . Reference) . mapinv . revFromGUI
    where mapinv = M.fromList . map (\(a,b) -> (b,a)) . M.toList

type ClassMap m = M.Map Identifier (Constructor m)

type ObjectMap m = M.Map Identifier (m (NewObject m))

lookupClass :: Identifier -> ClassMap m -> Constructor m
lookupClass k m = case M.lookup k m of
                    Just f -> f
                    Nothing -> error $ "Unknown class '" ++ k ++ "'"

instantiateGUI :: MonadSubstrate m => ClassMap m -> GUI -> (WidgetRef, InstGUI m)
instantiateGUI m = inst 0
    where inst r (GUI v c es cs) =
            ( lastwr
            , InstGUI v r (lookupClass c m) (M.map evalConstExpr es)
                          $ zip orients children )
                where (orients, childwrs) = unzip cs
                      (lastwr, children) =
                        mapAccumL (inst . (+1)) (r+length cs) childwrs

instantiateObjs :: MonadSubstrate m => ObjectRef -> ObjectMap m ->
                   m (ObjectRef, [(ObjectRef, NewObject m)],
                      M.Map Identifier VarBinding)
instantiateObjs r = foldM inst (r-1, [], M.empty) . M.toList
    where inst (r', l, bnds) (name, con) = do
            obj <- con
            return (r'+1, (r'+1, obj):l,
                    M.insert name (ConstBnd $ Reference $ r'+1) bnds)

compileSindre :: MonadSubstrate m => Program -> ClassMap m -> ObjectMap m ->
                 InitVal m -> Either String (m (SindreEnv m), Sindre m ())
compileSindre prog cm om root = Right (state, mainloop)
    where (lastwr, gui') = instantiateGUI cm $ programGUI prog
          state = do
            ws <- liftM (map $ second toWslot) $ initGUI root gui'
            (lastwr', objs, objenv) <- instantiateObjs (lastwr+1) om
            let os = (map $ second toOslot) objs
            return SindreEnv {
              objects = array (0, lastwr') $ ws++os
              , widgetRev = revFromGUI gui'
              , varEnv  = envFromGUI gui' `M.union` objenv
              , evtQueue = Q.empty
              }
          mainloop = do
            forever $ do
              fullRedraw
              handleEvent (programActions prog) =<< getEvent

handleEvent :: MonadSubstrate m => [(Pattern, Action)] -> (EventSource, Event) -> Sindre m ()
handleEvent m (_, KeyPress kp) = mapM_ execute $ filter (applies . fst) m
    where applies (KeyPattern kp2)  = kp == kp2
          applies (OrPattern p1 p2) = applies p1 || applies p2
          applies _                 = False
          execute (_, act) = compileAction act
handleEvent m (WidgetSrc wr, NamedEvent evn vs) = mapM_ execute =<< filterM (applies . fst) m
    where applies (OrPattern p1 p2) = pure (||) <*> applies p1 <*> applies p2
          applies (SourcedPattern (NamedSource wn) evn2 _) = do
            wr2 <- lookupObj wn
            return $ wr2 == wr && evn2 == evn
          applies _ = return False
          execute (SourcedPattern _ _ vars, act) = do
            modify $ \s -> s { varEnv = newenv `M.union` varEnv s}
            compileAction act
              where newenv = M.fromList $ zip vars $ map VarBnd vs
          execute (_, act) = compileAction act
handleEvent _ _ = return ()
