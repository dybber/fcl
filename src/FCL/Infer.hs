-- | FCL type inference
-- Heavily inspired by http://okmij.org/ftp/Computation/FLOLAC/lecture.pdf
module FCL.Infer (typeinfer, TypeError(..)) where

import Data.List (nub)
import qualified Data.Map as Map
import Control.Monad.Trans.State (get)

import FCL.Core.SourceRegion
import FCL.Core.Identifier
import FCL.Core.Syntax

import FCL.Infer.Substitution
import FCL.Infer.Monad
import FCL.Infer.Unification

type TypeEnvironment = Map.Map Identifier (TypeScheme Type)

lkup :: TypeEnvironment -> Identifier -> TI (TypeScheme Type)
lkup env x =
  case Map.lookup x env of
    Just ty  -> return ty
    Nothing -> throwError (UnboundVariableError x)

ext :: TypeEnvironment -> Identifier -> TypeScheme Type -> TypeEnvironment
ext env x ty = Map.insert x ty env

-- | Return the list of type variables in t (possibly with duplicates)
freeTyVars :: Type -> [TyVar]
freeTyVars IntT        = []
freeTyVars BoolT       = []
freeTyVars DoubleT     = []
freeTyVars StringT     = []
freeTyVars UnitT       = []
freeTyVars (_ :-> t)   = freeTyVars t
freeTyVars (t1 :> t2)  = freeTyVars t1 ++ freeTyVars t2
freeTyVars (t1 :*: t2) = freeTyVars t1 ++ freeTyVars t2
freeTyVars (PullArrayT t)   = freeTyVars t
freeTyVars (PushArrayT _ t) = freeTyVars t
freeTyVars (VarT v)         = [v]
freeTyVars (ProgramT _ t)   = freeTyVars t

infer :: TypeEnvironment -> Exp ty -> TI (Type, Exp Type)
infer _ (Literal l reg) =
  let ty =
        case l of
          LiteralInt _    -> IntT
          LiteralDouble _ -> DoubleT
          LiteralBool _   -> BoolT
          LiteralString _ -> StringT
          Unit            -> UnitT
  in return (ty, Literal l reg)
infer env (Var x _ reg) = do
  ty <- lkup env x
  ty' <- instantiate ty
  return (ty', Var x ty' reg)
infer env (App e1 e2) = do
  (t1,e1') <- infer env e1
  (t2,e2') <- infer env e2
  tv <- newtv
  unify Missing t1 (t2 :> tv)
  return (tv, App e1' e2')
infer env (Lamb x _ e _ reg) = do
  tv <- newtv
  (te, e') <- infer (ext env x (TypeScheme [] tv)) e
  return (tv :> te, Lamb x tv e' te reg)
infer env (AppLvl e lvl) = do
  lvlvar <- newLvlVar
  unifyLvlVar lvlvar lvl
  (te,e') <- infer env e
  tv <- newtv
  unify Missing te (lvlvar :-> tv)
  return (tv, AppLvl e' lvl)
infer env (LambLvl lvlvar ebody _ reg) = do
  (te, ebody') <- infer env ebody
  return (lvlvar :-> te, LambLvl lvlvar ebody' te reg)
infer env (Let x e ebody _ reg) = do
  (ts,e') <- generalize (infer env e)
  (t,body) <- infer (ext env x ts) ebody
  return (t, Let x e' body t reg)
infer env (Cond e1 e2 e3 _ reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify reg t1 BoolT
  unify reg t2 t3
  return (t2, Cond e1' e2' e3' t2 reg)
infer env (Pair e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  return (t1 :*: t2, Pair e1' e2' reg)
infer env (Proj1E e reg) = do
  (t,e') <- infer env e
  tv1 <- newtv
  tv2 <- newtv
  unify reg t (tv1 :*: tv2)
  return (tv1, Proj1E e' reg)
infer env (Proj2E e reg) = do
  (t, e') <- infer env e
  tv1 <- newtv
  tv2 <- newtv
  unify reg t (tv1 :*: tv2)
  return (tv2, Proj2E e' reg)
infer env (Index e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- newtv
  unify reg t1 (PullArrayT tv)
  unify reg t2 IntT
  return (tv, Index e1' e2' reg)
infer env (LengthPull e reg) = do
  (t, e') <- infer env e
  tv <- newtv
  unify reg t (PullArrayT tv)
  return (IntT, LengthPull e' reg)
infer env (LengthPush e reg) = do
  (t, e') <- infer env e
  tv <- newtv
  lvlVar <- newLvlVar
  unify reg t (PushArrayT (VarL lvlVar) tv)
  return (IntT, LengthPush e' reg)
infer env (For e1 e2 e3 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  tv <- newtv
  unify reg t1 IntT
  unify reg t2 IntT
  unify reg t3 ((IntT :> tv) :> (IntT :> (IntT :*: tv)))
  return (PushArrayT Zero tv, For e1' e2' e3' reg)
infer env (Power e1 e2 e3 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  tv <- newtv
  lvlVar <- newLvlVar
  let lvl = VarL lvlVar
  unify reg t1 IntT
  unify reg t2 (IntT :> (PullArrayT tv :> ProgramT lvl (PushArrayT lvl tv)))
  unify reg t3 (ProgramT lvl (PushArrayT lvl tv))
  return (ProgramT lvl (PullArrayT tv), Power e1' e2' e3' reg)
infer env (While e1 e2 e3 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  tv <- newtv
  lvlVar <- newLvlVar
  let lvl = VarL lvlVar
  unify reg t1 (PullArrayT tv :> BoolT)
  unify reg t2 (PullArrayT tv :> ProgramT lvl (PushArrayT lvl tv))
  unify reg t3 (ProgramT lvl (PushArrayT lvl tv))
  return (ProgramT lvl (PullArrayT tv), While e1' e2' e3' reg)
infer env (WhileSeq e1 e2 e3 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify reg t1 (t3 :> BoolT)
  unify reg t2 (t3 :> t3)
  return (t3, WhileSeq e1' e2' e3' reg)
infer env (MapPull e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv1 <- newtv
  tv2 <- newtv
  unify reg t1 (tv1 :> tv2)
  unify reg t2 (PullArrayT tv1)
  return (PullArrayT tv2, MapPull e1' e2' reg)
infer env (MapPush e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv1 <- newtv
  tv2 <- newtv
  lvlVar <- newLvlVar
  unify reg t1 (tv1 :> tv2)
  unify reg t2 (PushArrayT (VarL lvlVar) tv1)
  return (PushArrayT (VarL lvlVar) tv2, MapPull e1' e2' reg)
infer env (GeneratePull e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tv <- newtv
  unify reg t1 IntT
  unify reg t2 (IntT :> tv)
  return (PullArrayT tv, GeneratePull e1' e2' reg)
infer env (Force e reg) = do
  (t,e') <- infer env e
  tv <- newtv
  lvlVar <- newLvlVar
  unify reg t (PushArrayT (VarL lvlVar) tv)
  return (ProgramT (VarL lvlVar) (PullArrayT tv), Force e' reg)
infer env (ForceAndPrint e1 e2 reg) = do
  (te1, e1') <- infer env e1
  (te2, e2') <- infer env e2
  unify reg te1 IntT
  unify reg te2 (PushArrayT gridLevel IntT)
  return (ProgramT gridLevel (PullArrayT IntT), ForceAndPrint e1' e2' reg)
infer env (Benchmark e1 e2 reg) = do
  (te1, e1') <- infer env e1
  (te2, e2') <- infer env e2
  tv <- newtv
  unify reg te1 IntT
  unify reg te2 (ProgramT gridLevel tv)
  return (ProgramT gridLevel UnitT, Benchmark e1' e2' reg)
infer env (Interleave e1 e2 e3 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  (t3, e3') <- infer env e3
  unify reg t1 IntT
  unify reg t2 ((IntT :*: IntT) :> IntT)
  tv <- newtv
  lvlVar <- newLvlVar
  unify reg t3 (PullArrayT (ProgramT (VarL lvlVar) (PushArrayT (VarL lvlVar) tv)))
  return (ProgramT (Step (VarL lvlVar)) (PushArrayT (Step (VarL lvlVar)) tv), Interleave e1' e2' e3' reg)
infer _ (BlockSize reg) = return (IntT, BlockSize reg)
infer env (UnaryOp op e reg) = do
  (t, e') <- infer env e
  tret <- unifyUnOp op reg t
  return (tret, UnaryOp op e' reg)
infer env (BinaryOp op e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tret <- unifyBinOp op reg t1 t2
  return (tret, BinaryOp op e1' e2' reg)
infer env (Vec es _ reg) = do
  tes <- mapM (infer env) es
  t <- unifyAll reg (map fst tes)
  return (ProgramT gridLevel (PullArrayT t), Vec (map snd tes) t reg)
infer env (Push lvl e1 reg) = do
  (te, e1') <- infer env e1
  telem <- newtv
  unify reg te (PullArrayT telem)
  let ty = PushArrayT lvl telem
  return (ty, Push lvl e1' reg)
infer env (Return lvl e1 reg) = do
  (te, e1') <- infer env e1
  return (ProgramT lvl te, Return lvl e1' reg)
infer env (Bind e1 e2 reg) = do
  (te1, e1') <- infer env e1
  (te2, e2') <- infer env e2
  ta <- newtv
  tb <- newtv
  lvlVar <- newLvlVar
  unify reg te1 (ProgramT (VarL lvlVar) ta)
  unify reg te2 (ta :> ProgramT (VarL lvlVar) tb)
  let ty = ProgramT (VarL lvlVar) tb
  return (ty, Bind e1' e2' reg)
infer env (ReadIntCSV e1 reg) = do
  (te1, e1') <- infer env e1
  unify reg te1 StringT
  return (ProgramT gridLevel (PullArrayT IntT), ReadIntCSV e1' reg)

instantiate :: TypeScheme Type -> TI Type
instantiate (TypeScheme tyvars t) = do
  s <- mkFreshvars tyvars
  return (tvsub s t)
 where
   mkFreshvars :: [TyVar] -> TI Subst
   mkFreshvars [] = return (Map.empty, Map.empty)
   mkFreshvars (tv:tvs) = do
     (stv, slvl) <- mkFreshvars tvs
     fresh <- newtv
     return (Map.insert tv fresh stv, slvl)

-- | Give the list of all type variables that are allocated in TVE but
-- not bound there
free :: (Subst, Int) -> [TyVar]
free ((s,_),c) = filter (\v -> not (Map.member v s)) (map (\x -> TyVar x Nothing) [0..c-1])

generalize :: TI (Type, Exp Type) -> TI (TypeScheme Type, Exp Type)
generalize ta = do
 before <- get      -- type env before ta is executed
 (t,e')       <- ta
 TVE _ s_after  <- get      -- type env after ta is executed
 let t' = tvsub s_after t
 let tvdep = tvdependentset before s_after
 let fv = filter (not . tvdep) (nub (freeTyVars t'))
 return (TypeScheme fv t', e')

-- | Compute (quite unoptimally) the characteristic function of the set 
--  forall tvb \in fv(s_before). Union fv(tvsub(s_after,tvb))
tvdependentset :: TVE -> Subst -> (TyVar -> Bool)
tvdependentset (TVE i s_before) s_after = 
  \tv -> any (\tvb -> occurs s_after tv (VarT tvb)) (free (s_before,i))

initEnv :: TVE
initEnv = TVE 0 (Map.empty, Map.empty)

typeinfer :: [Definition a] -> Either TypeError [Definition Type]
typeinfer prog =
  do (typrog, TVE _ s) <- runTI (typecheckProg Map.empty prog) initEnv
     return (map (mapBody (tvsubExp s)) typrog)

typecheckProg :: TypeEnvironment -> [Definition a] -> TI [Definition Type]
typecheckProg _ [] = return []
typecheckProg tenv (d : ds) = do
  (tysc@(TypeScheme _ ty), ety) <- generalize (infer tenv (defBody d))
  case defSignature d of
    Just sig -> unify Missing sig ty
    Nothing -> return ()
  -- TODO TypeScheme tysc should be updated w. info from signature !
  rest <- typecheckProg (Map.insert (defVar d) tysc tenv) ds
  let typedDef = d { defTypeScheme = tysc
                   , defBody = ety
                   }  
  return (typedDef : rest)
