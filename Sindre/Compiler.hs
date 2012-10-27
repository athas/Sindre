{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sindre.Compiler
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  provisional
-- Portability :  portable
--
-- Transforming a Sindre program into a callable function.
--
-----------------------------------------------------------------------------
module Sindre.Compiler (
  -- * Main Entry Point
  compileSindre,
  ClassMap,
  ObjectMap,
  FuncMap,
  GlobMap,
  -- * Object Construction
  Constructor,
  ConstructorM,
  Param(..),
  paramM,
  paramAs,
  param,
  noParam,
  badValue,
  -- * Compiler Interface

  -- | These definitions can be used in builtin functions that may
  -- need to change global variables.
  Compiler,
  value,
  setValue,
                       )
    where

import Sindre.Runtime
import Sindre.Sindre
import Sindre.Util

import System.Exit

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Data.Array
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Traversable (for, traverse)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text as T

-- | Given a Sindre program and its environment, compile the program
-- and return a pair of command-line options accepted by the program,
-- and a startup function.  The program can be executed by calling the
-- startup function with the command-like arguments and an initial
-- value for the root widget.  If compilation fails, an IO exception
-- is raised.
compileSindre :: MonadBackend m => Program
              -> ClassMap m -> ObjectMap m -> FuncMap m -> GlobMap m
              -> ([SindreOption], Arguments -> m ExitCode)
compileSindre prog cm om fm gm = (opts, start)
  where (opts, prog', rootw) = compileProgram prog cm om fm gm
        start argv =
          let env = newEnv rootw argv
          in execSindre env prog'

data Binding = Lexical IM.Key | Global GlobalBinding
data GlobalBinding = Constant Value | Mutable IM.Key

-- | Mapping from class names to constructors.
type ClassMap m  = M.Map Identifier (Constructor m)
-- | Mapping from object names to object constructor functions.
type ObjectMap m = M.Map Identifier (ObjectRef -> m (NewObject m))
-- | Mapping from function names to built-in functions.  These must
-- first be executed in the 'Compiler' monad as they may have specific
-- requirements of the environment.
type FuncMap m   = M.Map Identifier (Compiler m ([Value] -> Sindre m Value))
-- | Mapping from names of global variables to computations that yield
-- their initial values.
type GlobMap m   = M.Map Identifier (m Value)

data CompilerEnv m = CompilerEnv {
      lexicalScope :: M.Map Identifier IM.Key
    , functionRefs :: M.Map Identifier (Execution m Value)
    , currentPos   :: SourcePos
    }

blankCompilerEnv :: CompilerEnv m
blankCompilerEnv = CompilerEnv {
                     lexicalScope = M.empty
                   , functionRefs = M.empty
                   , currentPos = nowhere
                   }

data CompilerState = CompilerState {
      globalScope :: M.Map Identifier GlobalBinding
    , nextMutable :: IM.Key
    }

blankCompilerState :: CompilerState
blankCompilerState = CompilerState {
                       globalScope = M.empty
                     , nextMutable = 0
                     }

type Initialisation m = Sindre m ()

-- | Monad inside which compilation takes place.
type Compiler m a = RWS (CompilerEnv m) (Initialisation m) CompilerState a

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

function :: MonadBackend m => Identifier -> Compiler m (Execution m Value)
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

-- | Given a variable name, return a computation that will yield the
-- value of the variable when executed.
value :: MonadBackend m => Identifier -> Compiler m (Execution m Value)
value k = do
  bnd <- binding k
  return $ case bnd of
    Lexical k' -> lexicalVal k'
    Global (Mutable k') -> sindre $ globalVal k'
    Global (Constant v) -> return v

-- | Given a variable name, return a computation that can be used to
-- set the value of the variable when executed.
setValue :: MonadBackend m => Identifier -> Compiler m (Value -> Execution m ())
setValue k = do
  bnd <- binding k
  case bnd of
    Lexical k' -> return $ setLexical k'
    Global (Mutable k') -> return $ sindre . setGlobal k'
    Global _ -> compileError $ "Cannot reassign constant '"++k++"'"

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
    setGlobal k' $ maybe defval string v
  return opt

compileObjs :: MonadBackend m =>
               ObjectNum -> ObjectMap m ->
               Compiler m (InstObjs m)
compileObjs r = zipWithM inst [r..] . M.toList
    where inst r' (k, f) = do
            let ref = (r', k, Just k)
            defName k $ Constant $ Reference ref
            return ((k, ref), f)

compileGUI :: MonadBackend m => ClassMap m -> (Maybe (P Expr), GUI)
           -> Compiler m (ObjectNum, InstGUI m)
compileGUI m (pos, gui) = do
  case pos of
    Nothing -> return ()
    Just re -> do re' <- descend compileExpr re
                  tell $ setRootPosition =<< execute re'
  inst 0 gui
    where inst r (GUI k c es cs) = do
            es' <- traverse (descend compileExpr) es
            (lastwr, children) <-
                mapAccumLM (inst . (+1)) (r+length cs) childwrs
            case k of
              Just k' -> defName k' $ Constant $ Reference (lastwr, unP c, k)
              Nothing -> return ()
            c' <- descend (lookupClass m) c
            orients' <- forM orients $ traverse $ descend compileExpr
            return ( lastwr, InstGUI (r, unP c, k) c' es'
                               $ zip orients' children )
                where (orients, childwrs) = unzip cs

compileProgram :: MonadBackend m => Program ->
                  ClassMap m -> ObjectMap m -> FuncMap m -> GlobMap m
               -> ([SindreOption], Sindre m () , WidgetRef)
compileProgram prog cm om fm gm =
  let env = blankCompilerEnv { functionRefs = funtable }
      ((funtable, evhandler, options, rootw), initialiser) =
        runCompiler env $ do
          mapM_ compileBackendGlobal $ M.toList gm
          opts <- mapM (descend compileOption) $ programOptions prog
          mapM_ (descend compileGlobal) $ programGlobals prog
          (lastwr, gui) <- compileGUI cm $ programGUI prog
          objs <- compileObjs (lastwr+1) om
          let lastwr' = lastwr + length objs
          handler <- compileActions $ programActions prog
          tell $ do
            ws <- initGUI gui
            os <- initObjs objs
            modify $ \s -> s { objects = array (0, lastwr') $ ws++os }
          funs' <- forM funs $ descend $ \(k, f) ->
            case (filter ((==k) . fst . unP) funs,
                  M.lookup k fm) of
              (_:_:_, _) -> compileError $
                            "Multiple definitions of function '"++k++"'"
              (_, Just _) -> compileError $
                             "Redefinition of built-in function '"++k++"'"
              _        -> do f' <- compileFunction f
                             return (k, f')
          fm' <- for fm $ \e -> do
            e' <- e
            return $ sindre . e' =<< IM.elems <$> sindre (gets execFrame)
          begin <- mapM (descend compileStmt) $ programBegin prog
          tell $ execute_ $ nextHere $ sequence_ begin
          return (M.fromList funs' `M.union` fm',
                  handler, opts, rootwref gui)
  in (options, initialiser >> eventLoop evhandler, rootw)
    where funs = programFunctions prog
          rootwref (InstGUI r _ _ _) = r

compileFunction :: MonadBackend m => Function -> Compiler m (Execution m Value)
compileFunction (Function args body) =
  local (\s -> s { lexicalScope = argmap }) $ do
    exs <- mapM (descend compileStmt) body
    return $ do
      sequence_ exs
      return falsity
      where argmap = M.fromList $ zip args [0..]

compileAction :: MonadBackend m => [Identifier] -> Action
              -> Compiler m (Execution m ())
compileAction args (StmtAction body) =
  local (\s -> s { lexicalScope = argmap }) $ do
    exs <- mapM (descend compileStmt) body
    return $ sequence_ exs
      where argmap = M.fromList $ zip args [0..]

compilePattern :: MonadBackend m => Pattern
               -> Compiler m ( Event -> Execution m (Maybe [Value])
                             , [Identifier])
compilePattern (ChordPattern kp1) = return (f, [])
    where f (KeyPress kp2) | kp1 == kp2 = return $ Just []
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
    where f wr (NamedEvent evn2 vs (FieldSrc wr2 fn2))
              | wr == wr2, evn2 == evn, fn2 `fcmp` fn = return $ Just vs
          f wr (NamedEvent evn2 vs (ObjectSrc wr2))
              | wr == wr2, evn2 == evn, Nothing <- fn = return $ Just vs
          f _ _ = return Nothing
compilePattern (SourcedPattern (GenericSource cn wn fn) evn args) =
  return (f, wn:args)
    where f (NamedEvent evn2 vs (FieldSrc wr2@(_,cn2,_) fn2))
              | cn==cn2, evn2 == evn, fn2 `fcmp` fn =
                  return $ Just $ Reference wr2 : vs
          f (NamedEvent evn2 vs (ObjectSrc wr2@(_,cn2,_)))
              | cn==cn2, evn2 == evn, Nothing <- fn =
                  return $ Just $ Reference wr2 : vs
          f _ = return Nothing

fcmp :: Identifier -> Maybe Identifier -> Bool
fcmp f = fromMaybe True . liftM (==f)

compileActions :: MonadBackend m => [P (Pattern, Action)]
               -> Compiler m (EventHandler m)
compileActions reacts = do
  reacts' <- mapM (descend compileReaction) reacts
  return $ \ev -> do dispatch ev reacts'
                     case ev of
                       KeyPress _ ->
                         flip recvEventByRef ev =<< sindre (gets kbdFocus)
                       _ -> return ()
    where compileReaction (pat, act) = do
            (pat', args) <- compilePattern pat
            act'         <- compileAction args act
            return (pat', act')
          dispatch ev = mapM_ $ \(applies, apply) -> do
            vs <- applies ev
            case vs of
              Just vs' -> setScope vs' apply
              Nothing  -> return ()

compileStmt :: MonadBackend m => Stmt -> Compiler m (Execution m ())
compileStmt (Print xs) = do
  xs' <- mapM (descend compileExpr) xs
  return $ do
    vs <- map show <$> sequence xs'
    back $ do
      printVal $ unwords vs
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
  return $ void e'
compileStmt (Return (Just e)) = do
  e' <- descend compileExpr e
  return $ doReturn =<< e'
compileStmt (Return Nothing) =
  return $ doReturn falsity
compileStmt Next = return doNext
compileStmt Break = return doBreak
compileStmt Continue = return doCont
compileStmt (If e trueb falseb) = do
  e' <- descend compileExpr e
  trueb' <- mapM (descend compileStmt) trueb
  falseb' <- mapM (descend compileStmt) falseb
  return $ do
    v <- e'
    sequence_ $ if true v then trueb' else falseb'
compileStmt (While c body) =
  compileStmt $ For blank c blank body
    where blank = Literal falsity `at` c
compileStmt (For e1 e2 e3 body) = do
  body' <- mapM (descend compileStmt) body
  e1'   <- descend compileExpr e1
  e2'   <- descend compileExpr e2
  e3'   <- descend compileExpr e3
  let stmt = do
        v <- e2'
        when (true v) $ contHere (sequence_ body') >> e3' >> stmt
  return $ e1' >> breakHere stmt
compileStmt (Do body c) = do
  body' <- mapM (descend compileStmt) body
  loop' <- descend compileStmt $ While c body `at` c
  return $ breakHere $ contHere (sequence_ body') >> loop'
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
    return $ if true v then falsity else truth
compileExpr (e1 `Equal` e2) =
  compileBinop e1 e2 $ \v1 v2 _ ->
    return $! if v1 == v2 then truth else falsity
compileExpr (e1 `LessThan` e2) =
  compileBinop e1 e2 $ \v1 v2 _ ->
    return $! if v1 < v2 then truth else falsity
compileExpr (e1 `LessEql` e2) =
  compileBinop e1 e2 $ \v1 v2 _ ->
    return $! if v1 <= v2 then truth else falsity
compileExpr (P _ (P _ (Var k) `Lookup` e1) `Assign` e2) = do
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
      Reference wr -> do _ <- setFieldByRef wr s v
                         return v
      _            -> bad "Not an object"
compileExpr (_ `Assign` _) = compileError "Cannot assign to rvalue"
compileExpr (e `Lookup` fe) = do
  fe' <- descend compileExpr fe
  e'  <- descend compileExpr e
  bad <- runtimeError
  return $ do
    v <- fe'
    o <- e'
    case o of
      Dict m -> return $ fromMaybe falsity $! M.lookup v m
      _      -> bad "Not a dictionary"
compileExpr (s `FieldOf` oe) = do
  oe' <- descend compileExpr oe
  bad <- runtimeError
  return $ do
    o <- oe'
    case o of
      Reference wr -> getFieldByRef wr s
      _            -> bad "Not an object"
compileExpr (Methcall oe meth argexps) = do
  argexps' <- mapM (descend compileExpr) argexps
  o' <- descend compileExpr oe
  bad <- runtimeError
  return $ do
    argvs <- sequence argexps'
    v     <- o'
    case v of
      Reference wr -> callMethodByRef wr meth argvs
      _            -> bad "Not an object"
compileExpr (Funcall f argexps) = do
  argexps' <- mapM (descend compileExpr) argexps
  f' <- function f
  return $ do
    argv <- sequence argexps'
    enterScope argv $ returnHere f'
compileExpr (Cond c trueb falseb) = do
  c' <- descend compileExpr c
  trueb' <- descend compileExpr trueb
  falseb' <- descend compileExpr falseb
  return $ do
    v <- c'
    if true v then trueb' else falseb'
compileExpr (Concat e1 e2) = compileBinop e1 e2 $ \v1 v2 bad ->
  case (mold v1, mold v2) of
    (Just v1', Just v2') -> return $ StringV $! v1' `T.append` v2'
    _ -> bad "Can only concatenate strings"
compileExpr (PostInc e) = do
  e' <- descend compileExpr e
  p' <- compileExpr $ e `Assign` (Plus e (Literal (Number 1) `at` e) `at` e)
  return $ e' <* p'
compileExpr (PostDec e) = do
  e' <- descend compileExpr e
  p' <- compileExpr $ e `Assign` (Minus e (Literal (Number 1) `at` e) `at` e)
  return $ e' <* p'
compileExpr (e1 `Plus` e2) = compileArithop (+) "add" e1 e2
compileExpr (e1 `Minus` e2) = compileArithop (-) "subtract" e1 e2
compileExpr (e1 `Times` e2) = compileArithop (*) "multiply" e1 e2
compileExpr (e1 `Divided` e2) = compileArithop (/) "divide" e1 e2
compileExpr (e1 `Modulo` e2) = compileArithop mod' "take modulo" e1 e2
compileExpr (e1 `RaisedTo` e2) = compileArithop (**) "exponentiate" e1 e2

compileBinop :: MonadBackend m =>
                P Expr -> P Expr ->
                (Value -> Value -> (String -> Execution m a)
                 -> Execution m Value)
             -> Compiler m (Execution m Value)
compileBinop e1 e2 op = do
  e1' <- descend compileExpr e1
  e2' <- descend compileExpr e2
  bad <- runtimeError
  return $ do
    v1 <- e1'
    v2 <- e2'
    op v1 v2 bad

compileArithop :: MonadBackend m =>
                  (Double -> Double -> Double)
               -> String -> P Expr -> P Expr
               -> Compiler m (Execution m Value)
compileArithop op opstr e1 e2 = compileBinop e1 e2 $ \v1 v2 bad ->
  case (mold v1, mold v2) of
    (Just v1', Just v2') -> return $ Number $! v1' `op` v2'
    _ -> bad $ "Can only " ++ opstr ++ " numbers"

type WidgetArgs im = M.Map Identifier (Execution im Value)

-- | Function that, given an initial value, the name of itself if any,
-- and a list of children, yields a computation that constructs a new
-- widget.
type Constructor m =
    WidgetRef -> [(Maybe Value, ObjectRef)] ->
    ConstructorM m (NewWidget m)
data InstGUI m = InstGUI WidgetRef
                         (Constructor m)
                         (WidgetArgs m)
                         [(Maybe (Execution m Value), InstGUI m)]
type InstObjs m = [((Identifier, ObjectRef),
                    ObjectRef -> m (NewObject m))]

initGUI :: MonadBackend m => InstGUI m
        -> Sindre m [(ObjectNum, DataSlot m)]
initGUI (InstGUI r@(wn,_,_) f args cs) = do
  args' <- traverse execute args
  childrefs <- forM cs $ \(e, InstGUI r' _ _ _) -> do
    v <- case e of Just e' -> Just <$> execute e'
                   Nothing -> return Nothing
    return (v,r')
  let constructor = do
        minw <- Just <$> param "minwidth"  <|> return Nothing
        minh <- Just <$> param "minheight" <|> return Nothing
        maxw <- Just <$> param "maxwidth"  <|> return Nothing
        maxh <- Just <$> param "maxheight" <|> return Nothing
        nw <- f r childrefs
        return $ instWidget nw ((minw, maxw), (minh, maxh))
  s <- runConstructor constructor args'
  children <- liftM concat $ mapM (initGUI . snd) cs
  return $ (wn,s):children

lookupClass :: ClassMap m -> Identifier -> Compiler m (Constructor m)
lookupClass m k = maybe unknown return $ M.lookup k m
    where unknown = compileError $ "Unknown class '" ++ k ++ "'"

initObjs :: MonadBackend m =>
            InstObjs m -> Sindre m [(ObjectNum, DataSlot m)]
initObjs = mapM $ \((_, r@(r',_,_)), con) -> do
             no <- back $ con r
             return (r', instObject no)

-- | Class of types that a given backend can convert to from 'Value's.
-- In effect, a monadic version of 'Mold'.
class MonadBackend m => Param m a where
  -- | Attempt to convert the given Sindre value to the relevant
  -- Haskell value.
  moldM :: Value -> m (Maybe a)

data ParamError = NoParam Identifier | BadValue Identifier Value
                  deriving (Show)

instance Error ParamError where
  strMsg = flip BadValue falsity

-- | The monad in which widget construction takes place.  You can only
-- execute this by defining a 'Constructor' that is then used in a
-- Sindre program (see also 'ClassMap').  An example usage could be:
--
-- @
-- myWidget :: 'Constructor' MyBackEnd
-- myWidget w k cs : do
--   -- ConstructorM is an instance of 'Alternative', so we can provide
--   -- defaults or fallbacks for missing parameters.
--   arg <- 'param' \"myParam\" <|> return 12
--   /rest of construction/
-- @
newtype ConstructorM m a = ConstructorM (ErrorT ParamError
                                         (StateT (M.Map Identifier Value)
                                          (Sindre m))
                                         a)
    deriving ( MonadState (M.Map Identifier Value)
             , MonadError ParamError
             , Monad, Functor, Applicative)

-- | @noParam k@ signals that parameter @k@ is missing.
noParam :: String -> ConstructorM m a
noParam = throwError . NoParam

-- | @badValue k v@ signals that parameter @k@ is present with value
-- @v@, but that @v@ is an invalid value.
badValue :: String -> Value -> ConstructorM m a
badValue k = throwError . BadValue k

runConstructor :: MonadBackend m => ConstructorM m a
             -> M.Map Identifier Value -> Sindre m a
runConstructor (ConstructorM c) m = do
  (v, m') <- runStateT (runErrorT c) m
  case v of
    Left (NoParam k) -> fail $ "Missing argument '"++k++"'"
    Left (BadValue k v') -> fail $ "Bad value "++show v'++" for argument '"
                           ++k++"'"++maybe "" ((": "++) . show) (M.lookup k m)
                              
    Right _ | m' /= M.empty ->
      fail $ "Surplus arguments: " ++ intercalate "," (M.keys m')
    Right v' -> return v'

instance MonadBackend m => Alternative (ConstructorM m) where
  empty = noParam "<none>"
  x <|> y = x `catchError` f
      where f (NoParam k) = y `catchError` g k
            f (BadValue k v) | not $ true v = y `catchError` g k
            f e                             = throwError e
            g k1 (NoParam  _) = noParam  k1
            g _  e            = throwError e

instance MonadBackend im => MonadSindre im ConstructorM where
  sindre = ConstructorM . lift . lift

instance (MonadIO m, MonadBackend m) => MonadIO (ConstructorM m) where
  liftIO = back . io

-- | @k `paramAs` f@ yields the value of the widget parameter @k@,
-- using @f@ to convert it to the proper Haskell type.  If @f@ returns
-- 'Nothing', @'badValue' k @ is called.  If @k@ does not exist,
-- @'noParam' k@ is called.
paramAs :: MonadBackend m =>
           Identifier -> (Value -> Maybe a) -> ConstructorM m a
paramAs k f = paramAsM k (return . f)

-- | As 'paramAs', but the conversion function is monadic.
paramAsM :: MonadBackend m => Identifier
         -> (Value -> m (Maybe a)) -> ConstructorM m a
paramAsM k mf = do m <- get
                   case M.lookup k m of
                     Nothing -> noParam k
                     Just v -> do put (k `M.delete` m)
                                  back (mf v) >>=
                                     maybe (badValue k v) return

-- | As 'paramM', but 'moldM' is always used for conversion.
paramM :: (Param m a, MonadBackend m) => Identifier -> ConstructorM m a
paramM k = paramAsM k moldM

-- | As 'param', but 'mold' is always used for conversion.
param :: (Mold a, MonadBackend m) => Identifier -> ConstructorM m a
param k = paramAs k mold
