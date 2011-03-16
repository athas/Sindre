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
                     , Object(..)
                     , Widget(..)
                     , VispState
                     , ClassMap
                     , Construction
                     , Constructor
                     , SubWidget(..)
                     , lookupObj
                     , compileExpr
                     , construct
                     , compileVisp
                     )
    where

import Visp.Visp
import Visp.Util

import System.IO

import Control.Applicative
import "monads-fd" Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type WidgetRef = [Int]

type WidgetArgs = [Value]
  
data VispState m = VispState {
      widgets   :: WidgetBox m
    , varEnv    :: VarEnvironment
  }

class ( Monad m
      , MonadState (VispState m) m
      , Applicative m
      , Monad (InitM m)) => MonadVisp m where
  type SubCfg m :: *
  type SubEvent m :: *
  type InitM m :: * -> *
  type InitVal m :: *
  
  rootRectangle :: m Rectangle
  getEvent :: m Event

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

operate :: MonadVisp m => WidgetRef -> (WidgetState m -> m (a, WidgetState m)) -> m a
operate r f = do
  (v, s') <- change' r =<< gets widgets
  modify $ \s -> s { widgets = replace' r (widgets s) s' }
  return v
    where replace' [] (WidgetBox cs _) s' = do
            WidgetBox cs s'
          replace' (r':rs) (WidgetBox cs s) s' = do
            case splitAt r' cs of
              (bef, w:aft) -> do
                WidgetBox (bef++(replace' rs w s'):aft) s
              _            -> error $ "Bad index " ++ show r
          change' [] (WidgetBox cs s) = do
            (v, s') <- f s
            return (v, s')
          change' (r':rs) (WidgetBox cs s) = do
            change' rs (cs !! r')

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
    recvSubEvent s _ = return ([], s)
    recvEvent    :: s -> Event -> m ([Event], s)
    recvEvent s _ = return ([], s)

refcall :: MonadVisp m => WidgetRef ->
           (WidgetState m -> m (a, WidgetState m)) -> m (a, WidgetRef)
refcall r f = do v <- operate r f
                 return (v, r)

instance MonadVisp m => Object m WidgetRef where
  callMethod r m vs = refcall r $ \s -> callMethod s m vs
  fieldSet r f v = snd <$> (refcall r $ \s -> ((,) ()) <$> fieldSet s f v)
  fieldGet r f = fst <$> (refcall r $ \s -> (flip (,) s) <$> fieldGet s f)

instance MonadVisp m => Widget m WidgetRef where
  compose r = fst <$> (refcall r $ \s -> (flip (,) s) <$> compose s)
  draw r rect = snd <$> (refcall r $ \s -> ((,) ()) <$> draw s rect)
  recvSubEvent r e = refcall r $ \s -> recvSubEvent s e
  recvEvent r e = refcall r $ \s -> recvEvent s e

data WidgetState m = forall s . (Widget m s, Object m s) =>
                     WidgetState s

inState :: Widget m s => m (a, s) -> m (a, WidgetState m)
inState f = do (v, s') <- f
               return $ (v, WidgetState s')

instance MonadVisp m => Object m (WidgetState m) where
  callMethod (WidgetState s) m vs =
    inState $ callMethod s m vs
  fieldSet (WidgetState s) f v =
    snd <$> (inState $ (,) v <$> fieldSet s f v)
  fieldGet (WidgetState s) = fieldGet s

instance MonadVisp m => Widget m (WidgetState m) where
  compose (WidgetState s) = compose s
  draw    (WidgetState s) = liftM WidgetState . draw s
  recvSubEvent (WidgetState s) e = inState $ recvSubEvent s e
  recvEvent (WidgetState s) e = inState $ recvEvent s e

data WidgetBox m = WidgetBox { widgetChildren :: [WidgetBox m]
                             , widgetCont     :: WidgetState m
                             }

allRefs :: WidgetBox m -> [WidgetRef]
allRefs (WidgetBox boxes _) =
  [] : concat (zipWith (\i -> map (i:)) [0..] (map allRefs boxes))

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

data SubWidget m = SubWidget WidgetRef WidgetRef
                 deriving (Show)

subcall :: MonadVisp m => SubWidget m ->
           (WidgetState m -> m (a, WidgetState m)) -> m (a, SubWidget m)
subcall sw@(SubWidget t s) f = do v <- operate (t++s) f
                                  return (v, sw)

instance MonadVisp m => Object m (SubWidget m) where
  callMethod sw m vs = subcall sw $ \s -> callMethod s m vs
  fieldSet sw f v = snd <$> (subcall sw $ \s -> ((,) ()) <$> fieldSet s f v)
  fieldGet sw f = fst <$> (subcall sw $ \s -> (flip (,) s) <$> fieldGet s f)

instance MonadVisp m => Widget m (SubWidget m) where
  compose sw = fst <$> (subcall sw $ \s -> (flip (,) s) <$> compose s)
  draw  sw r = snd <$> (subcall sw $ \s -> ((,) ()) <$> draw s r)
  recvSubEvent sw e = subcall sw $ \s -> recvSubEvent s e
  recvEvent sw e = subcall sw $ \s -> recvEvent s e

construct :: Widget m s => (s, InitVal m) -> Construction m
construct (s, v) = return (WidgetState s, v)

type Construction m = InitM m (WidgetState m, InitVal m)
type Constructor m =
    InitVal m -> WidgetArgs -> [SubWidget m] -> Construction m
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

compileVisp :: (MonadVisp m, MonadIO m) => Program -> ClassMap m ->
               InitVal m -> (Event -> m ()) -> Either String (InitM m (VispState m), m ())
compileVisp prog m root ehandle = Right (state, mainloop)
    where gui' = instantiateGUI m $ programGUI prog
          state = do ws <- initGUI root gui'
                     return VispState {
                                  widgets = ws
                                , varEnv  = envFromGUI gui'
                                }
          mainloop = do
            redraw
            forever $ do
              refs <- allRefs <$> gets widgets
              ehandle =<< getEvent
              --forM_ refs $ \ref ->
              --  delegateEvent ref ev
          redraw = do
            rect <- rootRectangle
            operate [] $ \s -> ((,) ()) <$> draw s rect
