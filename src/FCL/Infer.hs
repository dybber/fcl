-- | FCL type inference
-- Heavily inspired by http://okmij.org/ftp/Computation/FLOLAC/lecture.pdf
module FCL.Infer (typeinfer, TypeError(..)) where

import Data.List (nub)
import qualified Data.Map as Map
import Control.Monad.Trans.State (get)

import FCL.Core.SourceRegion
import FCL.External.Syntax
import FCL.Type.Polymorphic

import FCL.Infer.Substitution
import FCL.Infer.Monad
import FCL.Infer.Unification
import FCL.Infer.TypeEnvironment

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
infer env (Symbol x _ reg) = do
  ty <- lkup env x
  ty' <- instantiate ty
  return (ty', Symbol x ty' reg)
infer env (App e1 e2 reg) = do
  (t1,e1') <- infer env e1
  (t2,e2') <- infer env e2
  tv <- newtv
  unify reg t1 (t2 :> VarT tv)
  return (VarT tv, App e1' e2' reg)
infer env (Lamb x _ e _ reg) = do
  tv <- newtv
  (te, e') <- infer (ext env x (TypeScheme [] [] (VarT tv))) e
  return ((VarT tv) :> te, Lamb x (VarT tv) e' te reg)
infer env (AppLvl e lvl reg) = do
  lvlvar <- newLvlVar
  unifyLvlVar lvlvar lvl
  (te,e') <- infer env e
  tv <- newtv
  unify reg te (lvlvar :-> VarT tv)
  return (VarT tv, AppLvl e' lvl reg)
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

instantiate :: TypeScheme Type -> TI Type
instantiate (TypeScheme tyvars lvlvars t) = do
  s <- mkFreshvars tyvars
  return (tvsub s t)
 where
   mkFreshvars :: [TyVar] -> TI Subst
   mkFreshvars [] = return (Map.empty, Map.empty)
   mkFreshvars (tv:tvs) = do
     (stv, slvl) <- mkFreshvars tvs
     fresh <- newtv
     return (Map.insert tv (VarT fresh) stv, slvl)

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
 let freeLvlVars = error "TODO: generalize lvlvars"
 return (TypeScheme fv freeLvlVars t', e')

-- | Compute (quite unoptimally) the characteristic function of the set 
--  forall tvb \in fv(s_before). Union fv(tvsub(s_after,tvb))
tvdependentset :: TVE -> Subst -> (TyVar -> Bool)
tvdependentset (TVE i s_before) s_after = 
  \tv -> any (\tvb -> occurs s_after tv (VarT tvb)) (free (s_before,i))

typeinfer :: [Definition a] -> Either TypeError [Definition Type]
typeinfer prog =
  do (typrog, TVE _ s) <- runTI (typecheck prog) initEnv
     return (map (mapBody (tvsubExp s)) typrog)

typecheck :: [Definition a] -> TI [Definition Type]
typecheck prog =
  do initTypeEnvironment <- initialTypeEnvironment
     typecheckProg initTypeEnvironment prog

typecheckProg :: TypeEnvironment -> [Definition a] -> TI [Definition Type]
typecheckProg _ [] = return []
typecheckProg tenv (d : ds) = do
  (tysc@(TypeScheme _ _ ty), ety) <- generalize (infer tenv (defBody d))
  case defSignature d of
    Just sig -> unify (SourceRegion Missing Missing) sig ty
    Nothing -> return ()
  -- TODO TypeScheme tysc should be updated w. info from signature !
  rest <- typecheckProg (Map.insert (defVar d) tysc tenv) ds
  let typedDef = d { defTypeScheme = tysc
                   , defBody = ety
                   }  
  return (typedDef : rest)
