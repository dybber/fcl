{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.FCL.TypeInference where

import Language.FCL.Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import Data.List (nub)

type TypeEnv = Map.Map VarName TypeScheme
type Subst = Map.Map TyVarName Type

insert :: TypeEnv -> VarName -> TypeScheme -> TypeEnv
insert env n t = Map.insert n t env

-- Type inference monad
newtype TI x = TI (State (Subst, Int) x)
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Subst, Int))

initState = (Map.empty, 0)

emptyTEnv = Map.empty

evalTI :: TI x -> (Subst, Int) -> x
evalTI (TI m) = evalState m

runTI :: TI x -> (Subst, Int) -> (x, (Subst, Int))
runTI (TI m) = runState m

-- Create a fresh type variable
freshTyVar :: TI Type
freshTyVar = do
 (s,i) <- get
 put (s, succ i)
 return $ TyVar i

-- | Extend the current substitution
extendSubst :: TyVarName -> Type -> TI ()
extendSubst n t = do
 (s,i) <- get
 put (Map.insert n t s, i)

-- | Apply substitution to a `Type`
applySubst :: Subst -> Type -> Type
applySubst _ IntT = IntT
applySubst _ DoubleT = DoubleT
applySubst _ BoolT = BoolT
applySubst s (ArrayT t) = ArrayT (applySubst s t)
applySubst s (t1 :*: t2) = applySubst s t1 :> applySubst s t2
applySubst s (t1 :> t2) = applySubst s t1 :> applySubst s t2
applySubst s (TyVar n) = case Map.lookup n s of
                          Just t -> t
                          Nothing -> TyVar n

-- | Instantiate a type scheme with fresh type variables
instantiate :: TypeScheme -> TI Type
instantiate (TypeScheme tvs t) = do
  s <- mkFreshvars tvs
  return $ applySubst s t
 where
   mkFreshvars :: [TyVarName] -> TI Subst
   mkFreshvars [] = return Map.empty
   mkFreshvars (tv:tvs') = do
     s <- mkFreshvars tvs'
     fresh <- freshTyVar
     return $ Map.insert tv fresh s

-- | Occurs check
-- Does the type variable `n` occur in the given type?
occurs :: Subst -> TyVarName -> Type -> Bool
occurs _ _ IntT = False
occurs _ _ DoubleT = False
occurs _ _ BoolT = False
occurs s n (ArrayT t) = occurs s n t
occurs s n (t1 :> t2) = occurs s n t1 || occurs s n t2
occurs s n (t1 :*: t2) = occurs s n t1 || occurs s n t2
occurs s n (TyVar n') =
 case Map.lookup n' s of
   Just t -> occurs s n t
   Nothing -> n == n'

-- | Return the list of type variables in t (possibly with duplicates)
freevars :: Type -> [TyVarName]
freevars IntT       = []
freevars DoubleT    = []
freevars BoolT      = []
freevars (t1 :> t2) = freevars t1 ++ freevars t2
freevars (t1 :*: t2) = freevars t1 ++ freevars t2
freevars (ArrayT t) = freevars t
freevars (TyVar v)  = [v]

-- | The list of all type variables that are allocated in TVE but
-- not bound there
free :: (Subst, Int) -> [TyVarName]
free (s,c) = filter (\v -> not $ Map.member v s) [0..c-1]

-- Unification
unify :: Type -> Type -> TI ()
unify t1 t2 = do
 t1' <- chase t1
 t2' <- chase t2
 unify' t1' t2'
  where
    chase :: Type -> TI Type
    chase (TyVar n) = do
      (s,_) <- get
      case Map.lookup n s of
        Just t -> chase t
        Nothing -> return $ TyVar n
    chase t = return t

    unify' :: Type -> Type -> TI ()
    unify' IntT IntT = return ()
    unify' DoubleT DoubleT = return ()
    unify' BoolT BoolT = return ()
    unify' (t1l :> t1r) (t2l :> t2r) = do
      unify' t1l t2l
      unify' t1r t2r
    unify' (TyVar n) t = unify_fv n t
    unify' t (TyVar n) = unify_fv n t
    unify' t11 t22 = error $ "type mismatch: " ++ show t11 ++ " vs. " ++ show t22


    unify_fv :: TyVarName -> Type -> TI ()
    unify_fv n t@(TyVar n') | n == n'   = return ()
                            | otherwise = extendSubst n t
    unify_fv n t = do
      (s,_) <- get
      let c = occurs s n t 
      if c then error "occurs check failed"
           else extendSubst n t

infer :: TypeEnv -> Exp a -> TI (Type, Exp Type)
infer _ (IntE e) = return (IntT, IntE e)
infer _ (DoubleE e) = return (DoubleT, DoubleE e)
infer _ (BoolE e) = return (BoolT, BoolE e)
infer env (VarE n _) =
  case Map.lookup n env of
    Just t -> do
      t' <- instantiate t
      return (t', VarE n t')
    Nothing -> error "unknown var"
infer env (LamE n _ e _) = do
  tv <- freshTyVar
  (t, e') <- infer (insert env n (TypeScheme [] tv)) e
  return $ (tv :> t, LamE n t e' tv)
infer env (AppE e1 e2 _) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- freshTyVar
  unify t1 (t2 :> tv)
  return (tv, AppE e1' e2' tv)
infer env (IfE e1 e2 e3 _) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify t1 BoolT
  unify t2 t3
  return (t2, IfE e1' e2' e3' t2)
infer env (PairE e1 e2) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  return (t1 :*: t2, PairE e1' e2')
infer env (Proj1E e) = do
  (tpair, e') <- infer env e
  tv1 <- freshTyVar
  tv2 <- freshTyVar
  unify tpair (tv1 :*: tv2)
  return (tv1, Proj1E e')
infer env (Proj2E e) = do
  (tpair, e') <- infer env e
  tv1 <- freshTyVar
  tv2 <- freshTyVar
  unify tpair (tv1 :*: tv2)
  return (tv2, Proj2E e')
infer env (VectorE es _) = do
  (ts,es') <- unzip <$> mapM (infer env) es
  tv <- freshTyVar
  mapM_ (unify tv) ts
  return (ArrayT tv, VectorE es' tv)
infer env (LetE x _ e body _) = do
  (ts,e') <- generalize (infer env e)
  (t,body') <- infer (insert env x ts) body
  return (t, LetE x ts e' body' t)
 where
   generalize :: TI (Type, Exp Type) -> TI (TypeScheme, Exp Type)
   generalize ta = do
    (s_before,i) <- get      -- type env before ta is executed
    (t,e')       <- ta
    (s_after,_)  <- get      -- type env after ta is executed
    let t' = applySubst s_after t
    let tvdep = tvdependentset (s_before,i) s_after
    let fv = filter (not . tvdep) (nub (freevars t'))
    return $ (TypeScheme fv t', e')

   -- | Compute (quite unoptimally) the characteristic function of the set 
   --  forall tvb \in fv(s_before). Union fv(tvsub(s_after,tvb))
   tvdependentset :: (Subst, Int) -> Subst -> (TyVarName -> Bool)
   tvdependentset (s_before, i) s_after = 
       \tv -> any (\tvb -> occurs s_after tv (TyVar tvb)) (free (s_before,i))


typeinfer :: TypeEnv -> Exp a -> Type
typeinfer env e =
 let ((t,_), (subst,_)) = runTI (infer env e) initState
 in (applySubst subst t)
