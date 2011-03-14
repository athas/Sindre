{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Troels Henriksen <athas@sigkill.dk>
-- License     :  MIT-style (see LICENSE)
--
-- Stability   :  unstable
-- Portability :  nonportable
--
-- Transforming a Visp program into a callable function.
--
-----------------------------------------------------------------------------

module Visp.Compiler ( initGUI
                     , MonadVisp(..)
                     , InstGUI(..)
                     , Object(..)
                     , Widget(..)
                     , VispState
                     , ClassMap
                     , WidgetCont(..)
                     , Constructor
                     , buildCont
                     , compileVisp
                     )
    where

import Visp.Visp

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S


type WidgetRef = [Int]
  
data VispState m = VispState {
      widgets :: WidgetBox m
    , varEnv  :: VarEnvironment
  }

class ( Monad m
      , MonadState (VispState m) m
      , Applicative m
      , Monad (InitM m)) => MonadVisp m where
  type SubCfg m :: *
  type SubEvent m :: *
  type InitM m :: * -> *
  type InitVal m :: *
  
  getSubEvent :: m (SubEvent m)

lookupVar :: MonadVisp m => Identifier -> m (Maybe VarBinding)
lookupVar k = M.lookup k <$> gets varEnv

lookupVal :: MonadVisp m => Identifier -> m Value
lookupVal k = do
  bnd <- lookupVar k
  case bnd of
    Just (ValueBinding v) -> return v
    Nothing               -> return (IntegerV 0)

lookupObj :: MonadVisp m => Identifier -> m WidgetRef
lookupObj k = do
  bnd <- lookupVar k
  case bnd of
    Just (ObjectBinding v) -> return v
    _                      -> err
    where err = (error $ "Unknown object '"++k++"'")

operate :: MonadVisp m => WidgetRef -> (WidgetCont m -> m (a, WidgetCont m)) -> m a
operate r f = do
  (v, wb) <- operate' r =<< gets widgets
  modify $ \s -> s { widgets = wb }
  return v
    where operate' [] (WidgetBox cs s) = do
            (v, s') <- f s
            return (v, WidgetBox cs s')
          operate' (r':rs) (WidgetBox cs s) = do
            case splitAt r' cs of
              (bef, w:aft) -> do
                (v, w') <- operate' rs w
                return (v, WidgetBox (bef++w':aft) s)
              _            -> error "Bad index"

delegateEvent :: MonadVisp m =>
                 WidgetRef -> Event -> m [Event]
delegateEvent rs = operate rs . flip recvEvent

delegateSubEvent :: MonadVisp m =>
                    WidgetRef -> SubEvent m -> m [Event]
delegateSubEvent rs = operate rs . flip recvSubEvent

class MonadVisp m => Object m s where
    callMethod :: s -> Identifier -> [Value] -> m (Value, s)
    callMethod _ m _ = fail $ "Unknown method '" ++ m ++ "'"
    fieldSet   :: s -> Identifier -> Value -> m s
    fieldSet _ f _ = fail $ "Unknown field '" ++ f ++ "'"
    fieldGet   :: s -> Identifier -> m Value
    fieldGet _ f = fail $ "Unknown field '" ++ f ++ "'"

class (Object m s, MonadVisp m) => Widget m s where
    compose      :: s -> m Rectangle
    draw         :: s -> Rectangle -> m s
    recvSubEvent :: s -> SubEvent m -> m ([Event], s)
    recvEvent    :: s -> Event -> m ([Event], s)

data WidgetCont m = forall s . (Widget m s, Object m s) =>
                    WidgetCont s

buildCont :: Widget m s => s -> WidgetCont m
buildCont = WidgetCont

applyCont = undefined

instance MonadVisp m => Object m (WidgetCont m) where
  callMethod (WidgetCont s) m vs = do (v, s') <- callMethod s m vs
                                      return $ (v, WidgetCont s')
  fieldSet (WidgetCont s) f v = do s' <- fieldSet s f v
                                   return $ WidgetCont s'
  fieldGet (WidgetCont s) = fieldGet s

instance MonadVisp m => Widget m (WidgetCont m) where
  compose (WidgetCont s) = compose s
  draw    (WidgetCont s) r = do s' <- draw s r
                                return $ WidgetCont s'
  recvSubEvent (WidgetCont s) e = do (es, s') <- recvSubEvent s e
                                     return $ (es, WidgetCont s')
  recvEvent (WidgetCont s) e = do (es, s') <- recvEvent s e
                                  return $ (es, WidgetCont s')

data WidgetBox m = WidgetBox { widgetChildren :: [WidgetBox m]
                             , widgetCont     :: WidgetCont m
                             }

allRefs :: WidgetBox m -> [WidgetRef]
allRefs (WidgetBox boxes _) =
  [] : zipWith (:) [0..] (concatMap allRefs boxes)

data VarBinding = ValueBinding Value
                | ObjectBinding WidgetRef
type VarEnvironment = M.Map Identifier VarBinding

compileAction :: MonadVisp m => Action -> m ()
compileAction (ExprAction expr) = compileExpr expr *> return ()

compileExpr :: MonadVisp m => Expr -> m Value
compileExpr (Print args) = do
  compileExpr $ MCall ["stdout"] "print" args
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
        s { varEnv = M.insert k (ValueBinding v) (varEnv s) }
  case bnd of
    Nothing                -> add
    Just (ValueBinding _)  -> add
    Just (ObjectBinding _) ->
      error $ "Cannot reassign object"
  return v

evalConstExpr :: Expr -> Value
evalConstExpr (Literal v) = v
evalConstExpr _           = error "Not a const"

type Constructor m =
  InitVal m -> [Value] -> InitM m (WidgetCont m, InitVal m)
data InstGUI m = InstGUI (Maybe Identifier)
                         (Constructor m)
                         [Value]
                         [InstGUI m]

initGUI :: (MonadVisp m, Functor m) =>
           InitVal m -> InstGUI m -> InitM m (WidgetBox m)
initGUI x (InstGUI _ f vs cs) = do
  (cont, x') <- f x vs
  boxes <- mapM (initGUI x') cs
  return $ WidgetBox boxes cont

envFromGUI :: InstGUI m -> VarEnvironment
envFromGUI = M.map ObjectBinding . names
    where names (InstGUI v _ _ cs) =
            let m = maybe M.empty (flip M.singleton []) v
            in m `M.union` names' cs
          names' = M.unions . map (snd . prepend . names)
          prepend = M.mapAccum (\a r -> (a+1, a:r)) 0

type ClassMap m = M.Map Identifier (Constructor m)

lookupClass :: Identifier -> ClassMap m -> Constructor m
lookupClass k m = case M.lookup k m of
                    Just f -> f
                    Nothing -> error $ "Unknown class '" ++ k ++ "'"

instantiateGUI :: MonadVisp m => ClassMap m -> GUI -> InstGUI m
instantiateGUI m (GUI v c es cs) =
  InstGUI v (lookupClass c m) (map evalConstExpr es) $
          map (instantiateGUI m) cs

compileVisp :: MonadVisp m => Program -> ClassMap m ->
               InitVal m -> Either String (InitM m (VispState m), m ())
compileVisp prog m root = Right (state, mainloop)
    where gui' = instantiateGUI m $ gui prog
          state = do ws <- initGUI root gui'
                     return VispState {
                                  widgets = ws
                                , varEnv  = envFromGUI gui'
                                }
          mainloop = forever $ do
            ev <- getSubEvent
            refs <- allRefs <$> gets widgets
            forM_ refs $ \ref ->
              delegateSubEvent ref ev
