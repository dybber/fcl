-- Inspired by http://okmij.org/ftp/Computation/FLOLAC/lecture.pdf
module Language.FCL.TypeInference (typeinferProg) where

import Data.List (nub)
import qualified Data.Map as Map

import Control.Monad.State

import Language.FCL.Syntax

type TyEnv = Map.Map Variable TypeScheme
type ExpEnv = Map.Map Variable (Exp Type)
type Subst = Map.Map TyVarName Type
data TVE = TVE Int Subst

type TI x = State TVE x

evalTI :: TI a -> TVE -> a
evalTI m = evalState m

runTI :: TI a -> TVE -> (a, TVE)
runTI m = runState m

newtv :: TI Type
newtv = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (TyVar (TV i))

tvext :: (TyVarName,Type) -> TI ()
tvext (x,ty) = do
 (TVE i env) <- get
 put (TVE i (Map.insert x ty env))

lkup :: TyEnv -> Variable -> TypeScheme
lkup env x = maybe err id $ Map.lookup x env 
 where err = error $ "Unbound variable " ++ x

ext :: TyEnv -> Variable -> TypeScheme -> TyEnv
ext env x ty = Map.insert x ty env

tvsub :: Subst -> Type -> Type
tvsub _ IntT = IntT
tvsub _ BoolT = BoolT
tvsub _ DoubleT = DoubleT
tvsub s (t1 :> t2) = tvsub s t1 :> tvsub s t2
tvsub s (t1 :*: t2) = tvsub s t1 :*: tvsub s t2
tvsub s (TyVar tv) =
  case Map.lookup tv s of
    Just t -> tvsub s t
    Nothing -> TyVar tv
tvsub s (ArrayT Block t) = ArrayT Block (tvsub s t)
tvsub _ (ArrayT _ _) = error "tvsub: only block level support at the moment"


tvsubExp :: Subst -> Exp Type -> Exp Type
tvsubExp _ (IntScalar i) = IntScalar i
tvsubExp _ (BoolScalar b) = BoolScalar b
tvsubExp _ (DoubleScalar d) = DoubleScalar d
tvsubExp s (UnOp op e) = UnOp op (tvsubExp s e)
tvsubExp s (BinOp op e1 e2) = BinOp op (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (Var x t) = Var x (tvsub s t)
tvsubExp s (Vec es t) = Vec (map (tvsubExp s) es) (tvsub s t)
tvsubExp s (Lamb x t1 e t2) = Lamb x (tvsub s t1) (tvsubExp s e) (tvsub s t2)
tvsubExp s (Let x e ebody t) = Let x (tvsubExp s e) (tvsubExp s ebody) (tvsub s t)
tvsubExp s (App e1 e2) = App (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (Cond e1 e2 e3 t) = Cond (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) (tvsub s t)
tvsubExp s (Pair e1 e2) = Pair (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (Proj1E e1) = Proj1E (tvsubExp s e1)
tvsubExp s (Proj2E e1) = Proj2E (tvsubExp s e1)

tvsubExp s (Index e1 e2) = Index (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (Length e1) = Length (tvsubExp s e1)

tvsubExp s (Fixpoint e1 e2 e3) = Fixpoint (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3)
tvsubExp s (Generate Block e1 e2) = Generate Block (tvsubExp s e1) (tvsubExp s e2)
tvsubExp _ (Generate _ _ _) = error "tvsubExp: only block level allowed at the moment"
tvsubExp s (Map e1 e2) = Map (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (ForceLocal e1) = ForceLocal (tvsubExp s e1)
tvsubExp s (Concat e1 e2) = Concat (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (Assemble e1 e2 e3) = Assemble (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3)
tvsubExp _ LocalSize = LocalSize



-- | `shallow' substitution; check if tv is bound to anything `substantial'
tvchase :: Type -> TI Type
tvchase (TyVar x) = do
  (TVE _ s) <- get
  case Map.lookup x s of
    Just t -> tvchase t
    Nothing -> return (TyVar x)
tvchase t = return t

-- | The unification. If unification failed, return the reason
unify :: Type -> Type -> TI ()
unify t1 t2 = do
  t1' <- tvchase t1
  t2' <- tvchase t2
  unify' t1' t2'

-- | If either t1 or t2 are type variables, they are definitely unbound
unify' :: Type -> Type -> TI ()
unify' IntT IntT = return ()
unify' BoolT BoolT = return ()
unify' DoubleT DoubleT = return ()
unify' (t1a :> t1r) (t2a :> t2r) =
  do unify t1r t2r
     unify t1a t2a
unify' (t1l :*: t1r) (t2l :*: t2r) =
  do unify t1l t2l
     unify t1r t2r
unify' (ArrayT Block t1) (ArrayT Block t2) = unify t1 t2
unify' (TyVar v1) t2 = unify_fv v1 t2
unify' t1 (TyVar v2) = unify_fv v2 t1
unify' t1 t2 = error (unwords ["type mismatch:",show t1,"and",
                               show t2])

unify_fv :: TyVarName -> Type -> TI ()
unify_fv tv t@(TyVar tv') | tv == tv'   = return ()
                          | otherwise = tvext (tv, t)
unify_fv tv t = do
  (TVE _ s) <- get
  let c = occurs s tv t 
  if c then error "occurs check failed"
       else tvext (tv, t)

occurs :: Subst -> TyVarName -> Type -> Bool
occurs _ _ IntT = False
occurs _ _ BoolT = False
occurs _ _ DoubleT = False
occurs s tv (t1 :> t2) = occurs s tv t1 || occurs s tv t2
occurs s tv (t1 :*: t2) = occurs s tv t1 || occurs s tv t2
occurs s tv (ArrayT Block t) = occurs s tv t
occurs _ _ (ArrayT _ _) = error "Only block level allowed ATM" -- TODO
occurs s tv (TyVar tv2) =
    case Map.lookup tv2 s of
         Just t  -> occurs s tv t
         Nothing -> tv == tv2

infer :: TyEnv -> Exp ty -> TI (Type, Exp Type)
infer _ (IntScalar i) = return (IntT, IntScalar i)
infer _ (BoolScalar b) = return (BoolT, BoolScalar b)
infer _ (DoubleScalar d) = return (DoubleT, DoubleScalar d)
infer env (Var x _) = do
  ty <- instantiate (lkup env x)
  return (ty, Var x ty)
infer env (App e1 e2) = do
  (t1,e1') <- infer env e1
  (t2,e2') <- infer env e2
  tv <- newtv
  unify t1 (t2 :> tv)
  return (tv, App e1' e2')
infer env (Lamb x _ e _) = do
  tv <- newtv
  (te, e') <- infer (ext env x (TypeScheme [] tv)) e
  return (tv :> te, Lamb x tv e' te)
infer env (Let x e ebody _) = do
  (ts,e') <- generalize (infer env e)
  (t,body) <- infer (ext env x ts) ebody
  return (t, Let x e' body t)
infer env (Cond e1 e2 e3 _) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify t1 BoolT
  unify t2 t3
  return (t2, Cond e1' e2' e3' t2)
infer env (Pair e1 e2) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  return (t1 :*: t2, Pair e1' e2')
infer env (Proj1E e) = do
  (t,e') <- infer env e
  tv1 <- newtv
  tv2 <- newtv
  unify t (tv1 :*: tv2)
  return (tv1, Proj1E e')
infer env (Proj2E e) = do
  (t, e') <- infer env e
  tv1 <- newtv
  tv2 <- newtv
  unify t (tv1 :*: tv2)
  return (tv2, e')
infer env (Index e1 e2) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- newtv
  unify t1 (ArrayT Block tv)
  unify t2 IntT
  return (tv, Index e1' e2')
infer env (Length e) = do
  (t, e') <- infer env e
  tv <- newtv
  unify t (ArrayT Block tv)
  return (IntT, Length e')
infer env (Fixpoint e1 e2 e3) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify t1 (t3 :> BoolT)
  unify t2 (t3 :> t3)
  return (t3, Fixpoint e1' e2' e3')
infer env (Map e1 e2) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv1 <- newtv
  tv2 <- newtv
  unify t1 (tv1 :> tv2)
  unify t2 (ArrayT Block tv1)
  return (ArrayT Block tv2, Map e1' e2')
infer env (Generate Block e1 e2) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- newtv
  unify t1 IntT
  unify t2 (IntT :> tv)
  return (ArrayT Block tv, Generate Block e1' e2')
infer _ (Generate _ _ _) = error "typeinfer: Only block level support at the moment"
infer env (ForceLocal e) = do
  (t,e') <- infer env e
  tv <- newtv
  unify t (ArrayT Block tv)
  return (t, ForceLocal e')
infer env (Assemble e1 e2 e3) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify t1 IntT
  unify t2 ((IntT :*: IntT) :> IntT)
  tv <- newtv
  unify t3 (ArrayT Block (ArrayT Block tv))
  return (ArrayT Block tv, Assemble e1' e2' e3')
infer _ LocalSize = return (IntT, LocalSize)
infer env (UnOp op e) = do
  (t, e') <- infer env e
  tret <- unifyUnOp op t
  return (tret, UnOp op e')
infer env (BinOp op e1 e2) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tret <- unifyBinOp op t1 t2
  return (tret, BinOp op e1' e2')

unify1 :: Type -> Type -> Type -> TI Type
unify1 t1' tret t1 = do
  unify t1 t1'
  return tret

unifyUnOp :: UnOp -> Type -> TI Type
unifyUnOp AbsI = unify1 IntT IntT
unifyUnOp SignI = unify1 IntT IntT
unifyUnOp NegateI = unify1 IntT IntT
unifyUnOp Not = unify1 BoolT BoolT


unify2 :: Type -> Type -> Type -> Type -> Type -> TI Type
unify2 t1' t2' tret t1 t2 = do
  unify t1 t1'
  unify t2 t2'
  return tret

unifyBinOp :: BinOp -> Type -> Type -> TI Type
unifyBinOp AddI = unify2 IntT IntT IntT
unifyBinOp SubI = unify2 IntT IntT IntT
unifyBinOp MulI = unify2 IntT IntT IntT
unifyBinOp DivI = unify2 IntT IntT IntT
unifyBinOp ModI = unify2 IntT IntT IntT
unifyBinOp MinI = unify2 IntT IntT IntT
unifyBinOp EqI = unify2 IntT IntT BoolT
unifyBinOp NeqI = unify2 IntT IntT BoolT
unifyBinOp _ = error "Language.FCL.TypeInference: unhandled case in checkBinOp" -- TODO


instantiate :: TypeScheme -> TI Type
instantiate (TypeScheme tyvars t) = do
  s <- mkFreshvars tyvars
  return (tvsub s t)
 where
   mkFreshvars :: [TyVarName] -> TI Subst
   mkFreshvars [] = return Map.empty
   mkFreshvars (tv:tvs) = do
     s <- mkFreshvars tvs
     fresh <- newtv
     return $ Map.insert tv fresh s

-- | Return the list of type variables in t (possibly with duplicates)
freevars :: Type -> [TyVarName]
freevars IntT       = []
freevars BoolT       = []
freevars DoubleT       = []
freevars (t1 :> t2) = freevars t1 ++ freevars t2
freevars (t1 :*: t2) = freevars t1 ++ freevars t2
freevars (ArrayT Block t) = freevars t
freevars (ArrayT _ _) = error "Only block level allowed ATM" -- TODO
freevars (TyVar v)  = [v]

-- | Give the list of all type variables that are allocated in TVE but
-- not bound there
free :: (Subst, Int) -> [TyVarName]
free (s,c) = filter (\v -> not $ Map.member v s) (map TV [0..c-1])

generalize :: TI (Type, Exp Type) -> TI (TypeScheme, Exp Type)
generalize ta = do
 before <- get      -- type env before ta is executed
 (t,e')       <- ta
 TVE _ s_after  <- get      -- type env after ta is executed
 let t' = tvsub s_after t
 let tvdep = tvdependentset before s_after
 let fv = filter (not . tvdep) (nub (freevars t'))
 return (TypeScheme fv t', e')

-- | Compute (quite unoptimally) the characteristic function of the set 
--  forall tvb \in fv(s_before). Union fv(tvsub(s_after,tvb))
tvdependentset :: TVE -> Subst -> (TyVarName -> Bool)
tvdependentset (TVE i s_before) s_after = 
  \tv -> any (\tvb -> occurs s_after tv (TyVar tvb)) (free (s_before,i))

initEnv :: TVE
initEnv = TVE 0 Map.empty

typeinfer :: TyEnv -> Exp Untyped -> (Type, Exp Type)
typeinfer env e =
 let ((t,e'), TVE _ s) = runTI (infer env e) initEnv
 in (tvsub s t,
     tvsubExp s e')

typeinferProg :: Show ty => Prog ty -> [(Variable, Exp Type)]
typeinferProg prog =
 evalTI (typecheckProg Map.empty Map.empty prog) initEnv

typecheckProg :: Show ty => TyEnv -> ExpEnv -> Prog ty -> TI [(Variable, Exp Type)]
typecheckProg _ _ [] = return []
typecheckProg tenv eenv (KernelDef v : ds) = 
  case Map.lookup v eenv of
    Just e -> do
      rest <- typecheckProg tenv eenv ds
      return ((v,e) : rest)
    Nothing -> error ("Variable " ++ v ++ " not defined in kernel declaration")
typecheckProg tenv eenv (Definition v _ e : ds) = do
  (ty, ety) <- generalize (infer tenv e)
  typecheckProg (Map.insert v ty tenv) (Map.insert v ety eenv) ds
