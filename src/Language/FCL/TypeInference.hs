{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.FCL.TypeInference where

import Language.FCL.Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import Data.List (nub)

data TypeScheme = TypeScheme [TyVarName] Type
type TypeEnv = Map.Map VarName TypeScheme
type Subst = Map.Map TyVarName Type

add :: TypeEnv -> VarName -> TypeScheme -> TypeEnv
add env n t = Map.insert n t env

-- Type inference monad
newtype TI x = TI (State (Subst, Int) x)
  deriving (Functor,
            Applicative,
            Monad,
            MonadState (Subst, Int))

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
applySubst _ UnitT = UnitT
applySubst s (ArrayT t size) = ArrayT (applySubst s t) size
applySubst s (t1 :*: t2) = applySubst s t1 :*: applySubst s t2
applySubst s (t1 :> t2) = applySubst s t1 :> applySubst s t2
applySubst s (size :=> t) = size :=> applySubst s t
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
occurs _ _ UnitT = False
occurs s n (ArrayT t _) = occurs s n t
occurs s n (t1 :*: t2) = occurs s n t1 || occurs s n t2
occurs s n (t1 :> t2) = occurs s n t1 || occurs s n t2
occurs s n (_ :=> t) = occurs s n t
occurs s n (TyVar n') =
 case Map.lookup n' s of
   Just t -> occurs s n t
   Nothing -> n == n'

-- | Return the list of type variables in t (possibly with duplicates)
freevars :: Type -> [TyVarName]
freevars IntT         = []
freevars DoubleT      = []
freevars BoolT        = []
freevars UnitT        = []
freevars (t1 :*: t2)  = freevars t1 ++ freevars t2
freevars (t1 :> t2)   = freevars t1 ++ freevars t2
freevars (_ :=> t)    = freevars t
freevars (ArrayT t _) = freevars t
freevars (TyVar v)    = [v]

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
    unify' (t1l :*: t1r) (t2l :*: t2r) = do
      unify' t1l t2l
      unify' t1r t2r
    unify' (t1l :> t1r) (t2l :> t2r) = do
      unify' t1l t2l
      unify' t1r t2r
    unify' (_ :=> t1r) (_ :=> t2r) = do
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

infer :: TypeEnv -> Exp NoType -> TI (Type, Exp Type)
infer _ (IntE e) = return (IntT, IntE e)
infer _ (DoubleE e) = return (DoubleT, DoubleE e)
infer _ (BoolE e) = return (BoolT, BoolE e)
infer _ UnitE = return (UnitT, UnitE)
infer env (VarE n) = case Map.lookup n env of
                      Just t -> do
                         t' <- instantiate t
                         return (t', VarE n)
                      Nothing -> error "unknown var"
infer env (LamE n _ e NoType) = do
  tv <- freshTyVar
  (t, e') <- infer (add env n (TypeScheme [] tv)) e
  return $ (tv :> t, LamE n t e' tv)
infer env (AppE e1 e2 NoType) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- freshTyVar
  unify t1 (t2 :> tv)
  return (tv, AppE e1' e2' tv)
infer env (IfE e1 e2 e3 NoType) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify t1 BoolT
  unify t2 t3
  return (t2, IfE e1' e2' e3' t2)
infer env (LetE x NoType e body) = do
  (ts,e') <- generalize (infer env e)
  (t,body') <- infer (add env x ts) body
  return (t, LetE x t e' body')
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
