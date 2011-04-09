{-# LANGUAGE ExistentialQuantification #-}
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
import Control.Monad.RWS
import Data.Array
import Data.List
import Data.Maybe
import Data.Traversable(traverse)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Sequence as Q

data Binding = Lexical IM.Key | Constant Value | Global IM.Key

data CompilerEnv m = CompilerEnv {
      lexicalScope :: M.Map Identifier IM.Key
    , constScope   :: M.Map Identifier Value
    , functionRefs :: M.Map Identifier (ScopedExecution m Value)
    }

blankCompilerEnv :: CompilerEnv m
blankCompilerEnv = CompilerEnv {
                     lexicalScope = M.empty
                   , constScope  = M.empty
                   , functionRefs = M.empty
                   }

data CompilerState m = CompilerState {
      globalScope   :: M.Map Identifier IM.Key
    , nextGlobal    :: IM.Key
    }

blankCompilerState :: CompilerState m
blankCompilerState = CompilerState {
                       globalScope   = M.empty
                     , nextGlobal    = 0
                     }

type Initialisation m = Sindre m ()

type Compiler m a = RWS (CompilerEnv m) (Initialisation m) (CompilerState m) a

runCompiler :: CompilerEnv m -> Compiler m a -> (a, Initialisation m)
runCompiler env m = evalRWS m env blankCompilerState

function :: MonadSubstrate m => Identifier -> Compiler m (ScopedExecution m Value)
function k =
  fromMaybe nofun <$> M.lookup k <$> asks functionRefs
    where nofun = error $ "Unknown function '"++k++"'"

defGlobal :: MonadSubstrate m => Identifier -> Compiler m IM.Key
defGlobal k = do
  known <- M.lookup k <$> gets globalScope
  case known of
    Just _ -> error $ "Variable " ++ k ++ " already defined"
    Nothing -> do
      i <- gets nextGlobal
      modify $ \s ->
        s { globalScope = M.insert k i $ globalScope s
          , nextGlobal = i + 1 }
      return i

binding :: MonadSubstrate m => Identifier -> Compiler m Binding
binding k = do
  lexical  <- asks lexicalScope
  global   <- gets globalScope
  consts   <- asks constScope
  case (    Lexical <$> M.lookup k lexical
        <|> Global <$> M.lookup k global
        <|> Constant <$> M.lookup k consts) of
    Just b -> return b
    Nothing -> Global <$> defGlobal k

value :: MonadSubstrate m => Identifier -> Compiler m (Execution m Value)
value k = do
  bnd <- binding k
  return $ case bnd of
    Lexical k' -> lexicalVal k'
    Global  k' -> sindre $ globalVal k'
    Constant v -> return v

setValue :: MonadSubstrate m => Identifier -> Compiler m (Value -> Execution m ())
setValue k = do
  bnd <- binding k
  return $ case bnd of
    Lexical k' -> setLexical k'
    Global  k' -> sindre . setGlobal k'
    Constant _ -> error $ "Cannot reassign constant '" ++ k ++ "'"

constant :: MonadSubstrate m => Identifier -> Compiler m Value
constant k =
  fromMaybe noconst <$> M.lookup k <$> asks constScope
    where noconst = error $ "Unknown constant '"++k++"'"

type WidgetArgs m = M.Map Identifier (Execution m Value)
type WidgetParams = M.Map Identifier Value
type Construction m = m (NewWidget m, InitVal m)
type Constructor m =
    InitVal m -> WidgetParams -> [(Maybe Orientation, WidgetRef)] -> Construction m
data InstGUI m = InstGUI (Maybe Identifier)
                         WidgetRef
                         (Constructor m)
                         (WidgetArgs m)
                         [(Maybe Orientation, InstGUI m)]
type InstObjs m = [((Identifier, ObjectRef),
                    ObjectRef -> m (NewObject m))]

initGUI :: MonadSubstrate m =>
           InitVal m -> InstGUI m -> Sindre m [(WidgetRef, NewWidget m)]
initGUI x (InstGUI _ wr f args cs) = do
  args' <- traverse execute args
  (s, x') <- subst $ f x args' childrefs
  children <- liftM concat $ mapM (initGUI x' . snd) cs
  return $ (wr, s):children
    where childrefs = map (\(o, InstGUI _ wr' _ _ _) -> (o, wr')) cs

revFromGUI :: InstGUI m -> M.Map WidgetRef Identifier
revFromGUI (InstGUI v wr _ _ cs) = m `M.union` names cs
    where m = maybe M.empty (M.singleton wr) v
          names = M.unions . map (revFromGUI . snd)

constsFromGUI :: InstGUI m -> M.Map Identifier Value
constsFromGUI = M.map Reference . mapinv . revFromGUI
    where mapinv = M.fromList . map (\(a,b) -> (b,a)) . M.toList

constsFromObjs :: InstObjs m -> M.Map Identifier Value
constsFromObjs = M.fromList . map (second Reference . fst)

type ClassMap m = M.Map Identifier (Constructor m)

type ObjectMap m = M.Map Identifier (ObjectRef -> m (NewObject m))

lookupClass :: Identifier -> ClassMap m -> Constructor m
lookupClass k = fromMaybe unknown . M.lookup k
    where unknown = error $ "Unknown class '" ++ k ++ "'"

initObjs :: MonadSubstrate m =>
            InstObjs m -> Sindre m [(ObjectRef, NewObject m)]
initObjs = mapM $ \((_, r), con) -> do
             o <- subst $ con r
             return (r, o)

compileGlobals :: MonadSubstrate m =>
                  [(Identifier, Expr)] -> Compiler m ()
compileGlobals = mapM_ $ \(k, e) -> do
                   e' <- compileExpr (Var k `Assign` e)
                   tell $ execute_ e'

compileObjs :: MonadSubstrate m =>
               ObjectRef -> ObjectMap m ->
               Compiler m (InstObjs m)
compileObjs r = zipWithM inst [r..] . M.toList
    where inst r' (k, f) = return ((k, r'), f)

compileGUI :: MonadSubstrate m => ClassMap m -> GUI
           -> Compiler m (WidgetRef, InstGUI m)
compileGUI m = inst 0
    where inst r (GUI k c es cs) = do
            es' <- traverse compileExpr es
            (lastwr, children) <-
                mapAccumLM (inst . (+1)) (r+length cs) childwrs
            return ( lastwr, InstGUI k r (lookupClass c m) es'
                               $ zip orients children )
                where (orients, childwrs) = unzip cs

compileProgram :: MonadSubstrate m => ClassMap m -> ObjectMap m 
               -> Program -> Sindre m ()
compileProgram cm om prog =
  let env = blankCompilerEnv {
              functionRefs = funmap funtable
            , constScope   = consttable
            }
      ((funtable, consttable, evhandler), initialiser) =
        runCompiler env $ do
          compileGlobals $ programGlobals prog
          (lastwr, gui) <- compileGUI cm $ programGUI prog
          objs <- compileObjs (lastwr+1) om
          let lastwr' = lastwr + length objs
          handler <- compileActions $ programActions prog
          tell $ do
            root <- gets rootVal
            ws <- map (second toWslot) <$> initGUI root gui
            os <- map (second toOslot) <$> initObjs objs
            modify $ \s -> s {
                             objects = array (0, lastwr') $ ws++os
                           , widgetRev = revFromGUI gui
                           }
          funs <- forM id2funs $ \(k, f) -> do
            f' <- compileFunction f
            return (k, f')
          return ( IM.fromList funs
                 , constsFromGUI gui
                   `M.union` constsFromObjs objs
                 , handler)
  in (initialiser *> eventLoop evhandler)
    where 
          funmap   = M.fromList . zip (map fst progfuns) . map snd . IM.toList
          id2funs  = zip [0..] $ map snd progfuns
          progfuns = M.toList $ programFunctions prog

compileFunction :: MonadSubstrate m => Function -> Compiler m (ScopedExecution m Value)
compileFunction (Function args body) =
  local (\s -> s { lexicalScope = argmap }) $ do
    exs <- mapM compileStmt body
    return $ ScopedExecution $ do
      sequence_ exs
      return falsity
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
compilePattern (SourcedPattern (NamedSource wn) evn args) = do
  cv <- constant wn
  case cv of
    Reference wr -> return (f wr, args)
    _ -> error $ "'" ++ wn ++ "' is not an object."
    where f wr (ObjectSrc wr2, NamedEvent evn2 vs) 
              | wr == wr2 && evn2 == evn = return $ Just vs
          f _ _ = return Nothing
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
    sequence_ $ if true v then trueb' else falseb'
compileStmt (While c body) = do
  body' <- mapM compileStmt body
  c'    <- compileExpr c
  let stmt = do
        v <- c'
        when (true v) $ sequence_ body' *> stmt
  return stmt

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
compileExpr (Not e) = do
  e' <- compileExpr e
  return $ do
    v <- e'
    return $ if true v then truth else falsity
compileExpr (e1 `Equal` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    if v1 == v2 then truth else falsity
compileExpr (e1 `LessThan` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    if v1 < v2 then truth else falsity
compileExpr (e1 `LessEql` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
    if v1 <= v2 then truth else falsity
compileExpr (e1 `And` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
      if true v1 && true v2 then truth else falsity
compileExpr (e1 `Or` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
      if true v1 || true v2 then truth else falsity
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
      Dict m -> return $ fromMaybe falsity $ M.lookup v m
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

compileSindre :: MonadSubstrate m => Program -> ClassMap m -> ObjectMap m ->
                 Either String (InitVal m -> m ExitCode)
compileSindre prog cm om = Right $ \root ->
  let env = SindreEnv {
              widgetRev = M.empty
            , objects   = array (0, -1) []
            , evtQueue  = Q.empty
            , globals   = IM.empty
            , execFrame = IM.empty
            , rootVal   = root
            }
  in execSindre env prog'
    where prog' = compileProgram cm om prog
