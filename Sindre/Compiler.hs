{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import Data.List
import Data.Maybe
import Data.Traversable hiding (mapM, forM, sequence)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Sequence as Q

data CompilerEnv m = CompilerEnv {
      lexicalScope :: M.Map Identifier IM.Key
    , functionRefs :: M.Map Identifier (ScopedExecution m Value)
    }

blankCompilerEnv :: CompilerEnv m
blankCompilerEnv = CompilerEnv {
                     lexicalScope = M.empty
                   , functionRefs = M.empty
                   }

type Compiler m a = Reader (CompilerEnv m) a

runCompiler :: CompilerEnv m -> Compiler m a -> a
runCompiler = flip runReader

function :: MonadSubstrate m => Identifier -> Compiler m (ScopedExecution m Value)
function k = do
  b <- M.lookup k <$> asks functionRefs
  return $ case b of
    Nothing -> error $ "Unknown function '"++k++"'"
    Just k' -> k'

value :: MonadSubstrate m => Identifier -> Compiler m (Execution m Value)
value k = do
  b <- M.lookup k <$> asks lexicalScope
  return $ maybe (sindre $ globalVal k) lexicalVal b

setValue :: MonadSubstrate m => Identifier -> Compiler m (Value -> Execution m ())
setValue k = do
  b <- M.lookup k <$> asks lexicalScope
  return $ maybe (sindre . setGlobal k) setLexical b

type EventHandler m = (EventSource, Event) -> Execution m ()
type CompiledProgram m = ( IM.IntMap (ScopedExecution m Value)
                         , EventHandler m)

compileProgram :: MonadSubstrate m => Program -> CompiledProgram m
compileProgram prog =
  let v@(funtable, _) = runCompiler (env funtable) $ do
        handler <- compileActions $ programActions prog
        funs <- forM id2funs $ \(k, f) -> do
          f' <- compileFunction f
          return (k, f')
        return (IM.fromList funs, handler)
  in v
    where env funs = blankCompilerEnv {
                       functionRefs = funmap funs
                     }
          funmap   = M.fromList . zip (map fst progfuns) . map snd . IM.toList
          id2funs  = zip [0..] $ map snd progfuns
          progfuns = M.toList $ programFunctions prog

compileFunction :: MonadSubstrate m => Function -> Compiler m (ScopedExecution m Value)
compileFunction (Function args body) =
  local (\s -> s { lexicalScope = argmap }) $ do
    exs <- mapM compileStmt body
    return $ ScopedExecution $ do
      sequence_ exs
      return (IntegerV 0)
      where argmap = M.fromList $ zip args [0..]

compileAction :: MonadSubstrate m => [Identifier] -> Action 
              -> Compiler m (ScopedExecution m ())
compileAction args (StmtAction body) =
  local (\s -> s { lexicalScope = argmap }) $ do
    exs <- mapM compileStmt body
    return $ ScopedExecution $
      sequence_ exs
      where argmap = M.fromList $ zip args [0..]

compilePattern :: MonadSubstrate m => Pattern 
               -> Compiler m ( (EventSource, Event) -> Execution m (Maybe [Value])
                           , [Identifier])
compilePattern (KeyPattern kp1) = return (f, [])
    where f (_, KeyPress kp2) | kp1 == kp2 = return $ Just []
                              | otherwise = return Nothing
          f _                 = return Nothing
compilePattern (OrPattern p1 p2) = do
  (p1', ids1) <- compilePattern p1
  (p2', ids2) <- compilePattern p2
  let check ev = do
        v1 <- p1' ev
        v2 <- p2' ev
        return $ case (v1, v2) of
          (Just vs1, Just vs2) -> Just $ vs1++vs2
          (Just vs1, Nothing)  -> Just vs1
          (Nothing, Just vs2)  -> Just vs2
          _                    -> Nothing
  return (check, ids1 ++ ids2)
compilePattern (SourcedPattern (NamedSource wn) evn args) = return (f, args)
    where f (ObjectSrc wr, NamedEvent evn2 vs) = do
            wr2 <- sindre $ lookupObj wn
            if wr2 == wr && evn2 == evn
              then return $ Just vs
              else return Nothing
          f _ = return Nothing
compilePattern (SourcedPattern _ _ _) =
  return (const $ return Nothing, [])

compileActions :: MonadSubstrate m => [(Pattern, Action)]
               -> Compiler m (EventHandler m)
compileActions reacts = do
  reacts' <- mapM compileReaction reacts
  return $ \(src, ev) -> forM_ reacts' $ \(applies, apply) -> do
    vs <- applies (src, ev)
    case vs of
      Just vs' -> enterScope vs' apply
      Nothing  -> return ()
      where compileReaction (pat, act) = do
              (pat', args) <- compilePattern pat
              act'         <- compileAction args act
              return (pat', act')

executeExpr :: MonadSubstrate m => Expr -> Sindre m Value
executeExpr = execute . runCompiler blankCompilerEnv . compileExpr

compileStmt :: MonadSubstrate m => Stmt -> Compiler m (Execution m ())
compileStmt (Print xs) = do
  xs' <- mapM compileExpr xs
  return $ do
    vs <- sequence xs'
    subst $ do
      printVal $ intercalate " " $ map show vs
      printVal "\n"  
compileStmt (Exit Nothing) =
  return $ sindre $ quitSindre ExitSuccess
compileStmt (Exit (Just e)) = do
  e' <- compileExpr e
  return $ do
    v <- e'
    sindre $ case v of
      IntegerV 0 -> quitSindre ExitSuccess
      IntegerV x -> quitSindre $ ExitFailure $ fi x
      _          -> error "Exit code must be an integer"
compileStmt (Expr e) = do
  e' <- compileExpr e
  return $ e' >> return ()
compileStmt (Return (Just e)) = do
  e' <- compileExpr e
  return $ doReturn =<< e'
compileStmt (Return Nothing) =
  return $ doReturn (IntegerV 0)
compileStmt Next = return doNext
compileStmt (If e trueb falseb) = do
  e' <- compileExpr e
  trueb' <- mapM compileStmt trueb
  falseb' <- mapM compileStmt falseb
  return $ do
    v <- e'
    case v of
      IntegerV 0 -> sequence_ falseb'
      _          -> sequence_ trueb'
    return ()
compileStmt e@(While c body) =
  compileStmt (If c (body++[e]) [])
compileExpr :: MonadSubstrate m => Expr -> Compiler m (Execution m Value)
compileExpr (Literal v) = return $ return v
compileExpr (Var v) = value v
compileExpr (Var k `Assign` e) = do
  e' <- compileExpr e
  set <- setValue k
  return $ do
    v <- e'
    set v
    return v
compileExpr (e1 `Equal` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    if v1 == v2 then IntegerV 1 else IntegerV 0
compileExpr (e1 `LessThan` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    if v1 < v2 then IntegerV 1 else IntegerV 0
compileExpr (e1 `LessEql` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    if v1 <= v2 then IntegerV 1 else IntegerV 0
compileExpr (e1 `And` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    IntegerV $ case (v1, v2) of
      (IntegerV 0, _) -> 0
      (_, IntegerV 0) -> 0
      _               -> 1
compileExpr (e1 `Or` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    IntegerV $ case (v1, v2) of
      (IntegerV 0, IntegerV 0) -> 0
      _                        -> 1
compileExpr (k `Lookup` e1 `Assign` e2) = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  k'  <- value k
  set <- setValue k
  return $ do
    v1 <- e1'
    v2 <- e2'
    o <- k'
    case o of
      Dict m ->
          set $ Dict $ M.insert v1 v2 m
      _ -> error "Not a dictionary"
    return v2
compileExpr (s `FieldOf` oe `Assign` e) = do
  oe' <- compileExpr oe
  e' <- compileExpr e
  return $ do
    o <- oe'
    v <- e'
    sindre $ case o of
      Reference wr -> do _ <- fieldSet wr s v
                         return v
      _            -> error "Not an object"
compileExpr (_ `Assign` _) = error "Cannot assign to rvalue"
compileExpr (k `Lookup` fe) = do
  fe' <- compileExpr fe
  k'  <- value k
  return $ do
    v <- fe'
    o <- k'
    case o of
      Dict m -> return $ fromMaybe (IntegerV 0) $ M.lookup v m
      _      -> error "Not a dictionary"
compileExpr (s `FieldOf` oe) = do
  oe' <- compileExpr oe
  return $ do
    o <- oe'
    sindre $ case o of
      Reference wr -> fieldGet wr s
      _            -> error "Not an object"
compileExpr (Methcall oe meth argexps) = do
  argexps' <- mapM compileExpr argexps
  o' <- compileExpr oe
  return $ do
    argvs <- sequence argexps'
    v     <- o'
    case v of
      Reference wr -> callMethod wr meth argvs
      _            -> error "Not an object"
compileExpr (Funcall f argexps) = do
  argexps' <- mapM compileExpr argexps
  f' <- function f
  return $ do
    argv <- sequence argexps'
    returnHere $
      enterScope argv f'
compileExpr (PostInc e) = do
  e' <- compileExpr e
  p' <- compileExpr (e `Assign` (e `Plus` Literal (IntegerV 1)))
  return $ e' <* p'
compileExpr (PostDec e) = do
  e' <- compileExpr e
  p' <- compileExpr (e `Assign` (e `Minus` Literal (IntegerV 1)))
  return $ e' <* p'
compileExpr (e1 `Plus` e2) = compileArithop (+) "add" e1 e2
compileExpr (e1 `Minus` e2) = compileArithop (-) "subtract" e1 e2
compileExpr (e1 `Times` e2) = compileArithop (*) "multiply" e1 e2
compileExpr (e1 `Divided` e2) = compileArithop div "divide" e1 e2

compileBinop :: MonadSubstrate m =>
                Expr -> Expr -> 
                (Value -> Value -> Value) ->
                Compiler m (Execution m Value)
compileBinop e1 e2 op = do
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  return $ do
    v1 <- e1'
    v2 <- e2'
    return $ op v1 v2

compileArithop :: MonadSubstrate m =>
                (Integer -> Integer -> Integer) ->
                String -> Expr -> Expr -> Compiler m (Execution m Value)
compileArithop op opstr e1 e2 = compileBinop e1 e2 $ \x y ->
  case (x, y) of
    (IntegerV x', IntegerV y') -> IntegerV (x' `op` y')
    _ -> error $ "Can only " ++ opstr ++ " integers"

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
           InitVal m -> InstGUI m -> Sindre m [(WidgetRef, NewWidget m)]
initGUI x (InstGUI _ wr f args cs) = do
  (s, x') <- subst $ f x args childrefs
  children <- liftM concat $ mapM (initGUI x' . snd) cs
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

type ObjectMap m = M.Map Identifier (ObjectRef -> m (NewObject m))

lookupClass :: Identifier -> ClassMap m -> Constructor m
lookupClass k = fromMaybe unknown . M.lookup k
    where unknown = error $ "Unknown class '" ++ k ++ "'"

instantiateGUI :: MonadSubstrate m => ClassMap m -> GUI -> Sindre m (WidgetRef, InstGUI m)
instantiateGUI m = inst 0
    where inst r (GUI k c es cs) = do
            vs <- traverse executeExpr es
            (lastwr, children) <-
                mapAccumLM (inst . (+1)) (r+length cs) childwrs
            return ( lastwr, InstGUI k r (lookupClass c m) vs
                               $ zip orients children )
                where (orients, childwrs) = unzip cs

instantiateObjs :: MonadSubstrate m => ObjectRef -> ObjectMap m ->
                   Sindre m (ObjectRef, [(ObjectRef, NewObject m)],
                             M.Map Identifier VarBinding)
instantiateObjs r = foldM inst (r-1, [], M.empty) . M.toList
    where inst (r', l, bnds) (name, con) = do
            obj <- subst $ con $ r'+1
            return (r'+1, (r'+1, obj):l,
                    M.insert name (ConstBnd $ Reference $ r'+1) bnds)

initConsts :: MonadSubstrate m => [(Identifier, Expr)] -> Sindre m ()
initConsts = mapM_ $ \(k, e) -> do
  bnd <- globalVar k
  v   <- executeExpr e
  let add = modify $ \s ->
        s { varEnv = M.insert k (VarBnd v) (varEnv s) }
  case bnd of
    Just _ ->
      error $ "Cannot reassign constant " ++ k
    _  -> add

compileSindre :: MonadSubstrate m => Program -> ClassMap m -> ObjectMap m ->
                 InitVal m -> Either String (m (SindreEnv m), Sindre m ())
compileSindre prog cm om root = Right (initstate, mainloop)
    where initstate = do
            let blankEnv = SindreEnv {
                             objects   = array (0, -1) []
                           , widgetRev = M.empty
                           , varEnv    = M.empty
                           , evtQueue  = Q.empty
                           , execFrame = IM.empty
                           , functions = funs
                           }
            liftM snd $ execSindre blankEnv $ do
              initConsts $ programConstants prog
              (lastwr, gui') <- instantiateGUI cm $ programGUI prog
              ws <- liftM (map $ second toWslot) $ initGUI root gui'
              (lastwr', objs, objenv) <- instantiateObjs (lastwr+1) om
              let os = (map $ second toOslot) objs
              modify $ \env -> env {
                                 objects = array (0, lastwr') $ ws++os
                               , widgetRev = revFromGUI gui'
                               , varEnv  = envFromGUI gui'
                                           `M.union` objenv
                                           `M.union` varEnv env
                               }
          (funs, handler) = compileProgram prog
          mainloop =
            forever $ do
              fullRedraw
              ev <- getEvent
              execute $ do
                nextHere $
                  handler ev
                return $ IntegerV 0
