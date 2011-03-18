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

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Maybe
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
        s { varEnv = M.insert k v (varEnv s) }
  case bnd of
    Just (Reference _) ->
      error $ "Cannot reassign object"
    _  -> add
  return v
compileExpr (s `FieldOf` oe `Assign` e) = do
  o <- compileExpr oe
  v <- compileExpr e
  case o of
    Reference wr -> do fieldSet wr s v
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
    InitVal m -> WidgetArgs -> [SubWidget] -> Construction m
data InstGUI m = InstGUI (Maybe Identifier)
                         (Constructor m)
                         WidgetArgs
                         [InstGUI m]

initGUI :: (MonadVisp m, Functor m) =>
           InitVal m -> InstGUI m -> InitM m (WidgetBox m)
initGUI = flip initGUI' []
    where initGUI' x r (InstGUI _ f vs cs) = do
            (cont, x') <- f x vs $ zipWith (\i _ -> SubWidget r [i]) [0..] cs
            boxes <- zipWithM (initGUI' x') (map (\i-> r++[i]) [0..]) cs
            return $ WidgetBox boxes cont

envFromGUI :: InstGUI m -> VarEnvironment
envFromGUI = M.map Reference . names
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
  InstGUI v (lookupClass c m) (M.map evalConstExpr es) $
          map (instantiateGUI m) cs

compileVisp :: (MonadVisp m, MonadIO m) => Program -> ClassMap m ->
               InitVal m -> Either String (InitM m (VispState m), m ())
compileVisp prog m root = Right (state, mainloop)
    where gui' = instantiateGUI m $ programGUI prog
          state = do ws <- initGUI root gui'
                     return VispState {
                                  widgets = ws
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
