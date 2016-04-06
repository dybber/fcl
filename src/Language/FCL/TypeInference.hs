-- Inspired by http://okmij.org/ftp/Computation/FLOLAC/lecture.pdf
module Language.FCL.TypeInference (typeinfer, TypeError(..)) where

import Data.List (nub)
import qualified Data.Map as Map

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.FCL.Syntax

type TyEnv = Map.Map Variable (TypeScheme Type)
type Subst = Map.Map TyVar Type
data TVE = TVE Int Subst


data TypeError = UnificationError String
               | NotImplementedError String
               | UnboundTypeVariableError Variable
               | OccursCheckFailed
 deriving (Show, Eq)

type TI x = StateT TVE (Except TypeError) x

throwError :: TypeError -> TI a
throwError err = lift (throwE err)

runTI :: TI a -> TVE -> Either TypeError (a, TVE)
runTI m s = runExcept (runStateT m s)

newtv :: TI Type
newtv = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (VarT (TyVar i Nothing))

newNamedTV :: String -> TI Type
newNamedTV name = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (VarT (TyVar i (Just name)))

tvext :: (TyVar,Type) -> TI ()
tvext (x,ty) = do
 (TVE i env) <- get
 put (TVE i (Map.insert x ty env))

lkup :: TyEnv -> Variable -> TI (TypeScheme Type)
lkup env x =
  case Map.lookup x env of
    Just ty  -> return ty
    Nothing -> throwError (UnboundTypeVariableError x)

ext :: TyEnv -> Variable -> TypeScheme Type -> TyEnv
ext env x ty = Map.insert x ty env

tvsub :: Subst -> Type -> Type
tvsub _ IntT = IntT
tvsub _ BoolT = BoolT
tvsub _ DoubleT = DoubleT
tvsub s (t1 :> t2) = tvsub s t1 :> tvsub s t2
tvsub s (t1 :*: t2) = tvsub s t1 :*: tvsub s t2
tvsub s (VarT tv) =
  case Map.lookup tv s of
    Just t -> tvsub s t
    Nothing -> VarT tv
tvsub s (ArrayT lvl t) = ArrayT lvl (tvsub s t)

tvsubExp :: Subst -> Exp Type -> Exp Type
tvsubExp _ (IntScalar i reg) = IntScalar i reg
tvsubExp _ (BoolScalar b reg) = BoolScalar b reg
tvsubExp _ (DoubleScalar d reg) = DoubleScalar d reg
tvsubExp s (UnOp op e reg) = UnOp op (tvsubExp s e) reg
tvsubExp s (BinOp op e1 e2 reg) = BinOp op (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Var x t reg) = Var x (tvsub s t) reg
tvsubExp s (Vec es t reg) = Vec (map (tvsubExp s) es) (tvsub s t) reg
tvsubExp s (Lamb x t1 e t2 reg) = Lamb x (tvsub s t1) (tvsubExp s e) (tvsub s t2) reg
tvsubExp s (Let x e ebody t reg) = Let x (tvsubExp s e) (tvsubExp s ebody) (tvsub s t) reg
tvsubExp s (App e1 e2) = App (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (Cond e1 e2 e3 t reg) = Cond (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) (tvsub s t) reg
tvsubExp s (Pair e1 e2 reg) = Pair (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Proj1E e1 reg) = Proj1E (tvsubExp s e1) reg
tvsubExp s (Proj2E e1 reg) = Proj2E (tvsubExp s e1) reg

tvsubExp s (Index e1 e2 reg) = Index (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Length e1 reg) = Length (tvsubExp s e1) reg

tvsubExp s (While e1 e2 e3 reg) = While (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (Generate e1 e2 reg) = Generate (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Map e1 e2 reg) = Map (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (ForceLocal e1 reg) = ForceLocal (tvsubExp s e1) reg
tvsubExp s (Concat e1 e2 reg) = Concat (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp _ (LocalSize reg) = LocalSize reg
tvsubExp s (Scanl e1 e2 e3 reg) = Scanl (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg

prepareSig :: [(String, Type)] -> Type -> TI (Type, [(String, Type)])
prepareSig env IntT = return (IntT,env)
prepareSig env DoubleT = return (DoubleT,env)
prepareSig env BoolT = return (BoolT,env)
prepareSig env (t0 :> t1) =
  do (t0',env') <- prepareSig env t0
     (t1',env'') <- prepareSig env' t1
     return (t0' :> t1', env'')
prepareSig env (t0 :*: t1) =
  do (t0',env') <- prepareSig env t0
     (t1',env'') <- prepareSig env' t1
     return (t0' :*: t1', env'')  
prepareSig env (ArrayT lvl t) =
  do (t', env') <- prepareSig env t
     return (ArrayT lvl t', env')
prepareSig env (VarT (TyVar _ Nothing)) =
  do tv <- newtv
     return (tv, env)
prepareSig env (VarT (TyVar _ (Just x))) =
  case lookup x env of
    Just tv -> return (tv, env)
    Nothing ->
      do tv <- newNamedTV x
         return (tv, env)

-- | `shallow' substitution; check if tv is bound to anything `substantial'
tvchase :: Type -> TI Type
tvchase (VarT x) = do
  (TVE _ s) <- get
  case Map.lookup x s of
    Just t -> tvchase t
    Nothing -> return (VarT x)
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
unify' (ArrayT _ t1) (ArrayT _ t2) = unify t1 t2
unify' (VarT v1) t2 = unify_fv v1 t2
unify' t1 (VarT v2) = unify_fv v2 t1
unify' t1 t2 = throwError (UnificationError (unwords ["type mismatch:",show t1,"and",
                                                      show t2]))

unify_fv :: TyVar -> Type -> TI ()
unify_fv tv t@(VarT tv') | tv == tv'   = return ()
                         | otherwise = tvext (tv, t)
unify_fv tv t = do
  (TVE _ s) <- get
  let c = occurs s tv t 
  if c then throwError OccursCheckFailed
       else tvext (tv, t)

occurs :: Subst -> TyVar -> Type -> Bool
occurs _ _ IntT = False
occurs _ _ BoolT = False
occurs _ _ DoubleT = False
occurs s tv (t1 :> t2) = occurs s tv t1 || occurs s tv t2
occurs s tv (t1 :*: t2) = occurs s tv t1 || occurs s tv t2
occurs s tv (ArrayT _ t) = occurs s tv t
occurs s tv (VarT tv2) =
    case Map.lookup tv2 s of
         Just t  -> occurs s tv t
         Nothing -> tv == tv2

infer :: TyEnv -> Exp ty -> TI (Type, Exp Type)
infer _ (IntScalar i reg) = return (IntT, IntScalar i reg)
infer _ (BoolScalar b reg) = return (BoolT, BoolScalar b reg)
infer _ (DoubleScalar d reg) = return (DoubleT, DoubleScalar d reg)
infer env (Var x _ reg) = do
  ty <- lkup env x
  ty' <- instantiate ty
  return (ty', Var x ty' reg)
infer env (App e1 e2) = do
  (t1,e1') <- infer env e1
  (t2,e2') <- infer env e2
  tv <- newtv
  unify t1 (t2 :> tv)
  return (tv, App e1' e2')
infer env (Lamb x _ e _ reg) = do
  tv <- newtv
  (te, e') <- infer (ext env x (TypeScheme [] tv)) e
  return (tv :> te, Lamb x tv e' te reg)
infer env (Let x e ebody _ reg) = do
  (ts,e') <- generalize (infer env e)
  (t,body) <- infer (ext env x ts) ebody
  return (t, Let x e' body t reg)
infer env (Cond e1 e2 e3 _ reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify t1 BoolT
  unify t2 t3
  return (t2, Cond e1' e2' e3' t2 reg)
infer env (Pair e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  return (t1 :*: t2, Pair e1' e2' reg)
infer env (Proj1E e reg) = do
  (t,e') <- infer env e
  tv1 <- newtv
  tv2 <- newtv
  unify t (tv1 :*: tv2)
  return (tv1, Proj1E e' reg)
infer env (Proj2E e reg) = do
  (t, e') <- infer env e
  tv1 <- newtv
  tv2 <- newtv
  unify t (tv1 :*: tv2)
  return (tv2, Proj2E e' reg)
infer env (Index e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- newtv
  unify t1 (ArrayT Block tv)
  unify t2 IntT
  return (tv, Index e1' e2' reg)
infer env (Length e reg) = do
  (t, e') <- infer env e
  tv <- newtv
  unify t (ArrayT Block tv)
  return (IntT, Length e' reg)
infer env (While e1 e2 e3 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify t1 (t3 :> BoolT)
  unify t2 (t3 :> t3)
  return (t3, While e1' e2' e3' reg)
infer env (Map e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv1 <- newtv
  tv2 <- newtv
  unify t1 (tv1 :> tv2)
  unify t2 (ArrayT Block tv1)
  return (ArrayT Block tv2, Map e1' e2' reg)
infer env (Generate e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- newtv
  unify t1 IntT
  unify t2 (IntT :> tv)
  return (ArrayT Block tv, Generate e1' e2' reg)
infer env (ForceLocal e reg) = do
  (t,e') <- infer env e
  tv <- newtv
  unify t (ArrayT Block tv)
  return (t, ForceLocal e' reg)
infer env (Concat e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  unify t1 IntT
  tv <- newtv
  unify t2 (ArrayT Block (ArrayT Block tv))
  return (ArrayT Block tv, Concat e1' e2' reg)
infer _ (LocalSize reg) = return (IntT, LocalSize reg)
infer env (UnOp op e reg) = do
  (t, e') <- infer env e
  tret <- unifyUnOp op t
  return (tret, UnOp op e' reg)
infer env (BinOp op e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tret <- unifyBinOp op t1 t2
  return (tret, BinOp op e1' e2' reg)
infer env (Vec es _ reg) = do
  tes <- mapM (infer env) es
  t <- unifyAll (map fst tes)
  return (ArrayT Block t, Vec (map snd tes) t reg)
infer env (Scanl e1 e2 e3 reg) = do
  (tf, e1') <- infer env e1
  (ta, e2') <- infer env e2
  (tbs, e3') <- infer env e3
  tb <- newtv
  unify tf (ta :> tb :> ta)
  unify tbs (ArrayT Block tb)
  return (ArrayT Block ta, Scanl e1' e2' e3' reg)

unifyAll :: [Type] -> TI Type
unifyAll [] = newtv
unifyAll [t] = return t
unifyAll (t1 : t2 : ts) =
  do unify t1 t2
     unifyAll (t2 : ts)

unify1 :: Type -> Type -> Type -> TI Type
unify1 t1' tret t1 = do
  unify t1 t1'
  return tret

unifyUnOp :: UnOp -> Type -> TI Type
unifyUnOp AbsI = unify1 IntT IntT
unifyUnOp SignI = unify1 IntT IntT
unifyUnOp NegateI = unify1 IntT IntT
unifyUnOp Not = unify1 BoolT BoolT
unifyUnOp I2D = unify1 IntT DoubleT

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
unifyBinOp PowI = unify2 IntT IntT IntT
unifyBinOp ShiftLI = unify2 IntT IntT IntT
unifyBinOp ShiftRI = unify2 IntT IntT IntT
unifyBinOp AndI = unify2 IntT IntT IntT
unifyBinOp XorI = unify2 IntT IntT IntT
unifyBinOp DivR = unify2 DoubleT DoubleT DoubleT
unifyBinOp PowR = unify2 DoubleT DoubleT DoubleT

instantiate :: TypeScheme Type -> TI Type
instantiate (TypeScheme tyvars t) = do
  s <- mkFreshvars tyvars
  return (tvsub s t)
 where
   mkFreshvars :: [TyVar] -> TI Subst
   mkFreshvars [] = return Map.empty
   mkFreshvars (tv:tvs) = do
     s <- mkFreshvars tvs
     fresh <- newtv
     return (Map.insert tv fresh s)

-- | Give the list of all type variables that are allocated in TVE but
-- not bound there
free :: (Subst, Int) -> [TyVar]
free (s,c) = filter (\v -> not (Map.member v s)) (map (\x -> TyVar x Nothing) [0..c-1])

generalize :: TI (Type, Exp Type) -> TI (TypeScheme Type, Exp Type)
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
tvdependentset :: TVE -> Subst -> (TyVar -> Bool)
tvdependentset (TVE i s_before) s_after = 
  \tv -> any (\tvb -> occurs s_after tv (VarT tvb)) (free (s_before,i))

initEnv :: TVE
initEnv = TVE 0 Map.empty

typeinfer :: Program Untyped -> Either TypeError (Program Type)
typeinfer prog =
  do (typrog, TVE _ s) <- runTI (typecheckProg Map.empty prog) initEnv
     return (map (mapBody (tvsubExp s)) typrog)

typecheckProg :: TyEnv -> Program Untyped -> TI (Program Type)
typecheckProg _ [] = return []
typecheckProg tenv (d : ds) = do
  (tysc@(TypeScheme _ ty), ety) <- generalize (infer tenv (defBody d))
  case defSignature d of
    Just sig ->
      do (newTy, _) <- prepareSig [] sig -- TODO
         unify newTy ty
    Nothing -> return ()
  rest <- typecheckProg (Map.insert (defVar d) tysc tenv) ds
  let typedDef = Definition
                   { defVar = defVar d
                   , defSignature = Nothing
                   , defTypeScheme = tysc
                   , defEmitKernel = defEmitKernel d
                   , defBody = ety
                   }  
  return (typedDef : rest)
