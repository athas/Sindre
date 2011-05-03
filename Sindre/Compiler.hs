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
                       , FuncMap
                       , compileSindre
                       )
    where

import Sindre.Runtime
import Sindre.Sindre
import Sindre.Util

import System.Exit

import Control.Applicative
import Control.Arrow
import Control.Monad.RWS.Lazy
import Data.Array
import Data.List
import Data.Maybe
import Data.Traversable(traverse)
import qualified Data.IntMap as IM
import qualified Data.Map as M

data Binding = Lexical IM.Key | Global GlobalBinding
data GlobalBinding = Constant Value | Mutable IM.Key

type ClassMap m  = M.Map Identifier (Constructor m)
type ObjectMap m = M.Map Identifier (ObjectRef -> m (NewObject m))
type FuncMap m   = M.Map Identifier ([Value] -> Sindre m Value)

data CompilerEnv m = CompilerEnv {
      lexicalScope :: M.Map Identifier IM.Key
    , functionRefs :: M.Map Identifier (ScopedExecution m Value)
    , currentPos   :: SourcePos
    }

blankCompilerEnv :: CompilerEnv m
blankCompilerEnv = CompilerEnv {
                     lexicalScope = M.empty
                   , functionRefs = M.empty
                   , currentPos = nowhere
                   }

data CompilerState m = CompilerState {
      globalScope :: M.Map Identifier GlobalBinding
    , nextMutable :: IM.Key
    }

blankCompilerState :: CompilerState m
blankCompilerState = CompilerState {
                       globalScope = M.empty
                     , nextMutable = 0
                     }

type Initialisation m = Sindre m ()

type Compiler m a = RWS (CompilerEnv m) (Initialisation m) (CompilerState m) a

runCompiler :: CompilerEnv m -> Compiler m a -> (a, Initialisation m)
runCompiler env m = evalRWS m env blankCompilerState

descend :: (a -> Compiler m b) -> P a -> Compiler m b
descend m (P p v) = local (\s -> s { currentPos = p }) $ m v

compileError :: String -> Compiler m a
compileError s = do pos <- position <$> asks currentPos
                    error $ pos ++ s

runtimeError :: Compiler m (String -> Execution m a)
runtimeError = do pos <- position <$> asks currentPos
                  return $ \s -> fail $ pos ++ s

function :: MonadBackend m => Identifier -> Compiler m (ScopedExecution m Value)
function k = maybe bad return =<< M.lookup k <$> asks functionRefs
    where bad = compileError $ "Unknown function '"++k++"'"

defName :: MonadBackend m =>
           Identifier -> GlobalBinding -> Compiler m ()
defName k b = do
  known <- M.lookup k <$> gets globalScope
  case known of
    Just _ -> compileError $ "Multiple definitions of '"++k++"'"
    Nothing -> modify $ \s -> s
                { globalScope = M.insert k b $ globalScope s }

defMutable :: MonadBackend m => Identifier -> Compiler m IM.Key
defMutable k = do
  i <- gets nextMutable
  modify $ \s -> s { nextMutable = i + 1 }
  defName k $ Mutable i
  return i

constant :: MonadBackend m => Identifier -> Compiler m Value
constant k = do
  global <- gets globalScope
  case M.lookup k global of
    Just (Constant v) -> return v
    _ -> compileError $ "Unknown constant '"++k++"'"

binding :: MonadBackend m => Identifier -> Compiler m Binding
binding k = do
  lexical  <- asks lexicalScope
  global   <- gets globalScope
  case M.lookup k lexical of
    Just b -> return $ Lexical b
    Nothing -> case M.lookup k global of
                 Just b -> return $ Global b
                 Nothing -> Global <$> Mutable <$> defMutable k

value :: MonadBackend m => Identifier -> Compiler m (Execution m Value)
value k = do
  bnd <- binding k
  return $ case bnd of
    Lexical k' -> lexicalVal k'
    Global (Mutable k') -> sindre $ globalVal k'
    Global (Constant v) -> return v

setValue :: MonadBackend m => Identifier -> Compiler m (Value -> Execution m ())
setValue k = do
  bnd <- binding k
  case bnd of
    Lexical k' -> return $ setLexical k'
    Global (Mutable k') -> return $ sindre . setGlobal k'
    Global _ -> compileError $ "Cannot reassign constant '"++k++"'"

type WidgetArgs m = M.Map Identifier (Execution m Value)
type Construction m = (NewWidget m, InitVal m)
type Constructor m =
    InitVal m -> Maybe Identifier -> [(Maybe Value, ObjectRef)] ->
    M.Map Identifier Value -> Sindre m (Construction m)
data InstGUI m = InstGUI (Maybe Identifier)
                         ObjectRef
                         (Constructor m)
                         (WidgetArgs m)
                         [(Maybe (Execution m Value), InstGUI m)]
type InstObjs m = [((Identifier, ObjectRef),
                    ObjectRef -> m (NewObject m))]

initGUI :: MonadBackend m =>
           InitVal m -> InstGUI m -> Sindre m [(ObjectNum, NewWidget m)]
initGUI x (InstGUI k (wr, _) f args cs) = do
  args' <- traverse execute args
  childrefs <- forM cs $ \(e, InstGUI _ r _ _ _) -> do
    v <- case e of Just e' -> Just <$> execute e'
                   Nothing -> return Nothing
    return (v,r)
  (s, x') <- f x k childrefs args'
  children <- liftM concat $ mapM (initGUI x' . snd) cs
  return $ (wr, s):children

revFromGUI :: InstGUI m -> M.Map ObjectNum Identifier
revFromGUI (InstGUI v (wr, _) _ _ cs) = m `M.union` names cs
    where m = maybe M.empty (M.singleton wr) v
          names = M.unions . map (revFromGUI . snd)

lookupClass :: ClassMap m -> Identifier -> Compiler m (Constructor m)
lookupClass m k = maybe unknown return $ M.lookup k m
    where unknown = compileError $ "Unknown class '" ++ k ++ "'"

initObjs :: MonadBackend m =>
            InstObjs m -> Sindre m [(ObjectNum, NewObject m)]
initObjs = mapM $ \((_, r@(r', _)), con) -> do
             o <- back $ con r
             return (r', o)

compileBackendGlobal :: MonadBackend m => (Identifier, m Value) -> Compiler m ()
compileBackendGlobal (k, v) = do
  k' <- defMutable k
  tell $ setGlobal k' =<< back v

compileGlobal :: MonadBackend m =>
                 (Identifier, P Expr) -> Compiler m ()
compileGlobal (k, e) = do
  k' <- defMutable k
  e' <- descend compileExpr e
  tell $ setGlobal k' =<< execute e'

compileOption :: MonadBackend m =>
                 (Identifier, (SindreOption, Maybe Value))
              -> Compiler m SindreOption
compileOption (k, (opt, def)) = do
  let defval = fromMaybe falsity def
  k' <- defMutable k
  tell $ do
    v <- M.lookup k <$> gets arguments
    setGlobal k' $ maybe defval StringV v
  return opt

compileObjs :: MonadBackend m =>
               ObjectNum -> ObjectMap m ->
               Compiler m (InstObjs m)
compileObjs r = zipWithM inst [r..] . M.toList
    where inst r' (k, f) = do
            let ref = (r', k)
            defName k $ Constant $ Reference ref
            return ((k, ref), f)

compileGUI :: MonadBackend m => ClassMap m -> GUI
           -> Compiler m (ObjectNum, InstGUI m)
compileGUI m = inst 0
    where inst r (GUI k c es cs) = do
            es' <- traverse (descend compileExpr) es
            (lastwr, children) <-
                mapAccumLM (inst . (+1)) (r+length cs) childwrs
            case k of
              Just k' -> defName k' $ Constant $ Reference (lastwr, unP c)
              Nothing -> return ()
            c' <- descend (lookupClass m) c
            orients' <- forM orients $ traverse $ descend compileExpr
            return ( lastwr, InstGUI k (r, unP c) c' es'
                               $ zip orients' children )
                where (orients, childwrs) = unzip cs

compileProgram :: MonadBackend m => ClassMap m -> ObjectMap m -> FuncMap m
               -> Program -> ([SindreOption], Sindre m () , WidgetRef)
compileProgram cm om fm prog =
  let env = blankCompilerEnv { functionRefs = funtable }
      ((funtable, evhandler, options, rootv, rootw), initialiser) =
        runCompiler env $ do
          mapM_ compileBackendGlobal $ M.toList backendGlobals
          opts <- mapM (descend compileOption) $ programOptions prog
          mapM_ (descend compileGlobal) $ programGlobals prog
          (lastwr, gui) <- compileGUI cm $ snd $ programGUI prog
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
          funs' <- forM funs $ descend $ \(k, f) ->
            case (filter ((==k) . fst . unP) funs, 
                  M.lookup k fm) of
              (_:_:_, _) -> compileError $
                            "Multiple definitions of function '"++k++"'"
              (_, Just _) -> compileError $
                             "Redefinition of built-in function '"++k++"'"
              _        -> do f' <- compileFunction f
                             return (k, f')
          fm' <- flip traverse fm $ \e -> return $ ScopedExecution $
            sindre . e =<< IM.elems <$> sindre (gets execFrame)
          begin <- mapM (descend compileStmt) $ programBegin prog
          tell $ execute_ $ nextHere $ sequence_ begin
          v <- traverse (descend compileExpr) $ fst $ programGUI prog
          return (M.fromList funs' `M.union` fm',
                  handler, opts, v, rootwref gui)
  in (options, initialiser >> eventLoop rootv rootw evhandler, rootw)
    where funs = programFunctions prog
          rootwref (InstGUI _ r _ _ _) = r

compileFunction :: MonadBackend m => Function -> Compiler m (ScopedExecution m Value)
compileFunction (Function args body) =
  local (\s -> s { lexicalScope = argmap }) $ do
    exs <- mapM (descend compileStmt) body
    return $ ScopedExecution $ do
      sequence_ exs
      return falsity
      where argmap = M.fromList $ zip args [0..]

compileAction :: MonadBackend m => [Identifier] -> Action 
              -> Compiler m (ScopedExecution m ())
compileAction args (StmtAction body) =
  local (\s -> s { lexicalScope = argmap }) $ do
    exs <- mapM (descend compileStmt) body
    return $ ScopedExecution $
      sequence_ exs
      where argmap = M.fromList $ zip args [0..]

compilePattern :: MonadBackend m => Pattern 
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
compilePattern (SourcedPattern (NamedSource wn fn) evn args) = do
  cv <- constant wn
  case cv of
    Reference wr -> return (f wr, args)
    _ -> compileError $ "'" ++ wn ++ "' is not an object."
    where f wr (FieldSrc wr2 fn2, NamedEvent evn2 vs)
              | wr == wr2, evn2 == evn, fn2 `fcmp` fn = return $ Just vs
          f wr (ObjectSrc wr2, NamedEvent evn2 vs)
              | wr == wr2, evn2 == evn, fn == Nothing = return $ Just vs
          f _ _ = return Nothing
compilePattern (SourcedPattern (GenericSource cn wn fn) evn args) =
  return (f, wn:args)
    where f (FieldSrc wr2@(_,cn2) fn2, NamedEvent evn2 vs)
              | cn==cn2, evn2 == evn, fn2 `fcmp` fn =
                  return $ Just $ Reference wr2 : vs
          f (ObjectSrc wr2@(_,cn2), NamedEvent evn2 vs)
              | cn==cn2, evn2 == evn, fn == Nothing =
                  return $ Just $ Reference wr2 : vs
          f _ = return Nothing

fcmp :: Identifier -> Maybe Identifier -> Bool
fcmp f = fromMaybe True . liftM (==f)

compileActions :: MonadBackend m => [P (Pattern, Action)]
               -> Compiler m (EventHandler m)
compileActions reacts = do
  reacts' <- mapM (descend compileReaction) reacts
  return $ \(src, ev) -> dispatch (src, ev) reacts' <*
                         (flip recvEvent ev =<< sindre (gets kbdFocus))
    where compileReaction (pat, act) = do
            (pat', args) <- compilePattern pat
            act'         <- compileAction args act
            return (pat', act')
          dispatch (src, ev) = mapM_ $ \(applies, apply) -> do
            vs <- applies (src, ev)
            case vs of
              Just vs' -> enterScope vs' apply
              Nothing  -> return ()

compileStmt :: MonadBackend m => Stmt -> Compiler m (Execution m ())
compileStmt (Print xs) = do
  xs' <- mapM (descend compileExpr) xs
  return $ do
    vs <- mapM (sindre . printed) =<< sequence xs'
    back $ do
      printVal $ intercalate " " vs
      printVal "\n"
compileStmt (Exit Nothing) =
  return $ sindre $ quitSindre ExitSuccess
compileStmt (Exit (Just e)) = do
  e' <- descend compileExpr e
  bad <- runtimeError
  return $ do
    v <- e'
    case mold v :: Maybe Integer of
      Just 0  -> sindre $ quitSindre ExitSuccess
      Just x  -> sindre $ quitSindre $ ExitFailure $ fi x
      Nothing -> bad "Exit code must be an integer"
compileStmt (Expr e) = do
  e' <- descend compileExpr e
  return $ e' >> return ()
compileStmt (Return (Just e)) = do
  e' <- descend compileExpr e
  return $ doReturn =<< e'
compileStmt (Return Nothing) =
  return $ doReturn falsity
compileStmt Next = return doNext
compileStmt (If e trueb falseb) = do
  e' <- descend compileExpr e
  trueb' <- mapM (descend compileStmt) trueb
  falseb' <- mapM (descend compileStmt) falseb
  return $ do
    v <- e'
    sequence_ $ if true v then trueb' else falseb'
compileStmt (While c body) = do
  body' <- mapM (descend compileStmt) body
  c'    <- descend compileExpr c
  let stmt = do
        v <- c'
        when (true v) $ sequence_ body' >> stmt
  return stmt
compileStmt (Focus e) = do
  e' <- descend compileExpr e
  bad <- runtimeError
  return $ do
    v <- e'
    case v of
      Reference r -> sindre $ modify $ \s -> s { kbdFocus = r }
      _ -> bad "Focus is not a widget reference"

compileExpr :: MonadBackend m => Expr -> Compiler m (Execution m Value)
compileExpr (Literal v) = return $ return v
compileExpr (Var v) = value v
compileExpr (P _ (Var k) `Assign` e) = do
  e' <- descend compileExpr e
  set <- setValue k
  return $ do
    v <- e'
    set v
    return v
compileExpr (Not e) = do
  e' <- descend compileExpr e
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
      if true v1 then v2 else falsity
compileExpr (e1 `Or` e2) =
  compileBinop e1 e2 $ \v1 v2 ->
      if true v1 then v1 else v2
compileExpr (P _ (k `Lookup` e1) `Assign` e2) = do
  e1' <- descend compileExpr e1
  e2' <- descend compileExpr e2
  k'  <- value k
  set <- setValue k
  bad <- runtimeError
  return $ do
    v1 <- e1'
    v2 <- e2'
    o <- k'
    case o of
      Dict m ->
          set $! Dict $! M.insert v1 v2 m
      _ -> bad "Not a dictionary"
    return v2
compileExpr (P _ (s `FieldOf` oe) `Assign` e) = do
  oe' <- descend compileExpr oe
  e' <- descend compileExpr e
  bad <- runtimeError
  return $ do
    o <- oe'
    v <- e'
    case o of
      Reference wr -> sindre $ do _ <- fieldSet wr s v
                                  return v
      _            -> bad "Not an object"
compileExpr (_ `Assign` _) = compileError "Cannot assign to rvalue"
compileExpr (k `Lookup` fe) = do
  fe' <- descend compileExpr fe
  k'  <- value k
  bad <- runtimeError
  return $ do
    v <- fe'
    o <- k'
    case o of
      Dict m -> return $ fromMaybe falsity $! M.lookup v m
      _      -> bad "Not a dictionary"
compileExpr (s `FieldOf` oe) = do
  oe' <- descend compileExpr oe
  bad <- runtimeError
  return $ do
    o <- oe'
    case o of
      Reference wr -> sindre $ fieldGet wr s
      _            -> bad "Not an object"
compileExpr (Methcall oe meth argexps) = do
  argexps' <- mapM (descend compileExpr) argexps
  o' <- descend compileExpr oe
  bad <- runtimeError
  return $ do
    argvs <- sequence argexps'
    v     <- o'
    case v of
      Reference wr -> callMethod wr meth argvs
      _            -> bad "Not an object"
compileExpr (Funcall f argexps) = do
  argexps' <- mapM (descend compileExpr) argexps
  f' <- function f
  return $ do
    argv <- sequence argexps'
    returnHere $ enterScope argv f'
compileExpr (Cond c trueb falseb) = do
  c' <- descend compileExpr c
  trueb' <- descend compileExpr trueb
  falseb' <- descend compileExpr falseb
  return $ do
    v <- c'
    if true v then trueb' else falseb'
compileExpr (PostInc e) = do
  e' <- descend compileExpr e
  p' <- compileExpr $ e `Assign` (Plus e (Literal (IntegerV 1) `at` e) `at` e)
  return $ e' <* p'
compileExpr (PostDec e) = do
  e' <- descend compileExpr e
  p' <- compileExpr $ e `Assign` (Minus e (Literal (IntegerV 1) `at` e) `at` e)
  return $ e' <* p'
compileExpr (e1 `Plus` e2) = compileArithop (+) "add" e1 e2
compileExpr (e1 `Minus` e2) = compileArithop (-) "subtract" e1 e2
compileExpr (e1 `Times` e2) = compileArithop (*) "multiply" e1 e2
compileExpr (e1 `Divided` e2) = compileArithop div "divide" e1 e2
compileExpr (e1 `Modulo` e2) = compileArithop mod "take modulo" e1 e2
compileExpr (e1 `RaisedTo` e2) = compileArithop (^) "exponentiate" e1 e2

compileBinop :: MonadBackend m =>
                P Expr -> P Expr -> 
                (Value -> Value -> Value) ->
                Compiler m (Execution m Value)
compileBinop e1 e2 op = do
  e1' <- descend compileExpr e1
  e2' <- descend compileExpr e2
  return $ do
    v1 <- e1'
    v2 <- e2'
    return $! op v1 v2

compileArithop :: MonadBackend m =>
                  (Integer -> Integer -> Integer)
               -> String -> P Expr -> P Expr
               -> Compiler m (Execution m Value)
compileArithop op opstr e1 e2 = do
  e1' <- descend compileExpr e1
  e2' <- descend compileExpr e2
  bad <- runtimeError
  return $ do
    v1 <- e1'
    v2 <- e2'
    case (mold v1, mold v2) of
      (Just v1', Just v2') -> return $ IntegerV $! v1' `op` v2'
      _ -> bad $ "Can only " ++ opstr ++ " integers"

construct :: (Widget im s, MonadSindre im m) =>
             (s, InitVal im) -> m im (Construction im)
construct (s, v) = return (NewWidget s, v)

data NewWidget m = forall s . Widget m s => NewWidget s
data NewObject m = forall s . Object m s => NewObject s

toWslot :: NewWidget m -> DataSlot m
toWslot (NewWidget s) = WidgetSlot s
toOslot :: NewObject m -> DataSlot m
toOslot (NewObject s) = ObjectSlot s

compileSindre :: MonadBackend m =>
                 Program -> ClassMap m -> ObjectMap m -> FuncMap m 
              -> Either String ( [SindreOption]
                               , Arguments -> InitVal m -> m ExitCode)
compileSindre prog cm om fm = Right (opts, start)
  where (opts, prog', rootw) = compileProgram cm om fm prog
        start argv root =
          let env = newEnv root rootw argv
          in execSindre env prog'
