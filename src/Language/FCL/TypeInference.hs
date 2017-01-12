-- | FCL type inference
-- Inspired by http://okmij.org/ftp/Computation/FLOLAC/lecture.pdf
module Language.FCL.TypeInference (typeinfer, TypeError(..)) where

import Data.List (nub)
import qualified Data.Map as Map

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.FCL.SourceRegion
import Language.FCL.Syntax

type TyEnv = Map.Map Name (TypeScheme Type)

type Subst = (Map.Map TyVar Type,
              Map.Map LvlVar Level)
data TVE = TVE Int Subst

data TypeError = UnificationError Region Type Type
               | LevelUnificationError Level Level
               | NotImplementedError String
               | UnboundVariableError Name
               | UnboundLevelVariableError Name
               | UnboundTypeVariableError Name
               | OccursCheckFailed TyVar Type
               | OccursCheckFailedLevel
 deriving (Eq, Show)

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

tvext :: (TyVar,Type) -> TI ()
tvext (x,ty) = do
 (TVE i (stv, slvl)) <- get
 put (TVE i (Map.insert x ty stv, slvl))

newLvlVar :: TI LvlVar
newLvlVar = do
 (TVE i s) <- get
 put (TVE (i+1) s)
 return (LvlVar i Nothing)

lvlVarExt :: (LvlVar,Level) -> TI ()
lvlVarExt (lvlVar,lvl) = do
 (TVE i (stv, slvl)) <- get
 put (TVE i (stv, Map.insert lvlVar lvl slvl))

lkup :: TyEnv -> Name -> TI (TypeScheme Type)
lkup env x =
  case Map.lookup x env of
    Just ty  -> return ty
    Nothing -> throwError (UnboundVariableError x)

ext :: TyEnv -> Name -> TypeScheme Type -> TyEnv
ext env x ty = Map.insert x ty env


tvsub :: Subst -> Type -> Type
tvsub _ IntT = IntT
tvsub _ BoolT = BoolT
tvsub _ DoubleT = DoubleT
tvsub _ StringT = StringT
tvsub _ UnitT = UnitT
tvsub s (t1 :> t2) = tvsub s t1 :> tvsub s t2
tvsub s (lvl :-> t2) = lvl :-> tvsub s t2
tvsub s (t1 :*: t2) = tvsub s t1 :*: tvsub s t2
tvsub (stv, slvl) (VarT tv) =
  case Map.lookup tv stv of
    Just t -> tvsub (stv, slvl) t
    Nothing -> VarT tv
tvsub s (PullArrayT t) = PullArrayT (tvsub s t)
tvsub s (PushArrayT lvl t) = PushArrayT (lvlVarSub s lvl) (tvsub s t)
tvsub s (ProgramT lvl t) = ProgramT (lvlVarSub s lvl) (tvsub s t)

lvlVarSub :: Subst -> Level -> Level
lvlVarSub _ Zero = Zero
lvlVarSub s (Step l) = Step (lvlVarSub s l)
lvlVarSub (stv,slvl) (VarL lvlVar) =
  case Map.lookup lvlVar slvl of
    Just t -> lvlVarSub (stv,slvl) t
    Nothing -> VarL lvlVar

tvsubExp :: Subst -> Exp Type -> Exp Type
tvsubExp _ (IntScalar i reg) = IntScalar i reg
tvsubExp _ (BoolScalar b reg) = BoolScalar b reg
tvsubExp _ (DoubleScalar d reg) = DoubleScalar d reg
tvsubExp _ (String str reg) = String str reg
tvsubExp s (UnOp op e reg) = UnOp op (tvsubExp s e) reg
tvsubExp s (BinOp op e1 e2 reg) = BinOp op (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Var x t reg) = Var x (tvsub s t) reg
tvsubExp s (Vec es t reg) = Vec (map (tvsubExp s) es) (tvsub s t) reg
tvsubExp s (Lamb x t1 e t2 reg) = Lamb x (tvsub s t1) (tvsubExp s e) (tvsub s t2) reg
tvsubExp s (App e1 e2) = App (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (LambLvl lvlvar e t reg) = LambLvl lvlvar (tvsubExp s e) (tvsub s t) reg
tvsubExp s (AppLvl e lvl) = AppLvl (tvsubExp s e) (lvlVarSub s lvl)
tvsubExp s (Let x e ebody t reg) = Let x (tvsubExp s e) (tvsubExp s ebody) (tvsub s t) reg
tvsubExp s (Cond e1 e2 e3 t reg) = Cond (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) (tvsub s t) reg
tvsubExp s (Pair e1 e2 reg) = Pair (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Proj1E e1 reg) = Proj1E (tvsubExp s e1) reg
tvsubExp s (Proj2E e1 reg) = Proj2E (tvsubExp s e1) reg
tvsubExp s (Index e1 e2 reg) = Index (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (LengthPull e1 reg) = LengthPull (tvsubExp s e1) reg
tvsubExp s (LengthPush e1 reg) = LengthPush (tvsubExp s e1) reg
tvsubExp s (For e1 e2 e3 reg) = For (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (Power e1 e2 e3 reg) = Power (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (While e1 e2 e3 reg) = While (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (WhileSeq e1 e2 e3 reg) = WhileSeq (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (GeneratePull e1 e2 reg) = GeneratePull (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (MapPull e1 e2 reg) = MapPull (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (MapPush e1 e2 reg) = MapPush (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Force e1 reg) = Force (tvsubExp s e1) reg
tvsubExp s (Push lvl e1 reg) = Push (lvlVarSub s lvl) (tvsubExp s e1) reg
tvsubExp s (Interleave e1 e2 e3 reg) = Interleave (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp _ (BlockSize reg) = BlockSize reg
tvsubExp s (Return lvl e1 reg) = Return (lvlVarSub s lvl) (tvsubExp s e1) reg
tvsubExp s (Bind e1 e2 reg) = Bind (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (ReadIntCSV e1 reg) = ReadIntCSV (tvsubExp s e1) reg
tvsubExp s (ForceAndPrint e1 e2 reg) = ForceAndPrint (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Benchmark e1 e2 reg) = Benchmark (tvsubExp s e1) (tvsubExp s e2) reg

-- | `shallow' substitution; check if tv is bound to anything `substantial'
tvchase :: Type -> TI Type
tvchase (VarT x) = do
  (TVE _ (stv,_)) <- get
  case Map.lookup x stv of
    Just t -> tvchase t
    Nothing -> return (VarT x)
tvchase t = return t

-- | The unification. If unification failed, return the reason
unify :: Region -> Type -> Type -> TI ()
unify reg t1 t2 = do
  t1' <- tvchase t1
  t2' <- tvchase t2
  unify' reg t1' t2'

-- | If either t1 or t2 are type variables, they are definitely unbound
unify' :: Region -> Type -> Type -> TI ()
unify' reg t1 t2 =
  case (t1, t2) of
    (IntT, IntT) -> return ()
    (BoolT, BoolT) -> return ()
    (DoubleT, DoubleT) -> return ()
    (UnitT, UnitT) -> return ()
    (t1a :> t1r, t2a :> t2r) ->
        do unify reg t1r t2r
           unify reg t1a t2a
    (lvl1 :-> t1r, lvl2 :-> t2r) ->
        do unify reg t1r t2r
           unifyLvls (VarL lvl1) (VarL lvl2)
    (t1l :*: t1r, t2l :*: t2r) ->
        do unify reg t1l t2l
           unify reg t1r t2r
    (PullArrayT t1', PullArrayT t2') -> unify reg t1' t2'
    (PushArrayT lvl1 t1', PushArrayT lvl2 t2') ->
      do unifyLvls lvl1 lvl2
         unify reg t1' t2'
    (ProgramT lvl1 t1', ProgramT lvl2 t2') ->
      do unifyLvls lvl1 lvl2
         unify reg t1' t2'
    (VarT v1, t2') -> unify_fv v1 t2'
    (t1', VarT v2) -> unify_fv v2 t1'
    (t1', t2') -> throwError (UnificationError reg t1' t2')

unify_fv :: TyVar -> Type -> TI ()
unify_fv tv t@(VarT tv') | tv == tv'   = return ()
                         | otherwise = tvext (tv, t)
unify_fv tv t = do
  (TVE _ s) <- get
  let c = occurs s tv t 
  if c then throwError (OccursCheckFailed tv t)
       else tvext (tv, t)

unifyLvls :: Level -> Level -> TI ()
unifyLvls Zero Zero = return ()
unifyLvls (Step l0) (Step l1) = unifyLvls l0 l1
unifyLvls (VarL lvlVar) lvl = unifyLvlVar lvlVar lvl
unifyLvls lvl (VarL lvlVar) = unifyLvlVar lvlVar lvl
unifyLvls lvl1 lvl2 = throwError (LevelUnificationError lvl1 lvl2)

unifyLvlVar :: LvlVar -> Level -> TI ()
unifyLvlVar lvlVar1 lvl@(VarL lvlVar2) | lvlVar1 == lvlVar2 = return ()
                                       | otherwise = lvlVarExt (lvlVar1, lvl)
unifyLvlVar lvlVar1 lvl = do
  (TVE _ s) <- get
  let c = occursLvl s lvlVar1 lvl
  if c then throwError OccursCheckFailedLevel
       else lvlVarExt (lvlVar1, lvl)

occurs :: Subst -> TyVar -> Type -> Bool
occurs _ _ IntT = False
occurs _ _ BoolT = False
occurs _ _ DoubleT = False
occurs _ _ StringT = False
occurs _ _ UnitT = False
occurs s tv (t1 :> t2) = occurs s tv t1 || occurs s tv t2
occurs s tv (_ :-> t) = occurs s tv t
occurs s tv (t1 :*: t2) = occurs s tv t1 || occurs s tv t2
occurs s tv (PullArrayT t) = occurs s tv t
occurs s tv (PushArrayT _ t) = occurs s tv t
occurs s tv (ProgramT _ t) = occurs s tv t
occurs (stv, slvl) tv (VarT tv2) =
    case Map.lookup tv2 stv of
         Just t  -> occurs (stv, slvl) tv t
         Nothing -> tv == tv2

occursLvl :: Subst -> LvlVar -> Level -> Bool
occursLvl _ _ Zero = False
occursLvl s lvlVar (Step lvl) = occursLvl s lvlVar lvl
occursLvl (stv, slvl) lvlVar (VarL lvlVar2) =
    case Map.lookup lvlVar2 slvl of
         Just t  -> occursLvl (stv, slvl) lvlVar t
         Nothing -> lvlVar == lvlVar2

infer :: TyEnv -> Exp ty -> TI (Type, Exp Type)
infer _ (IntScalar i reg) = return (IntT, IntScalar i reg)
infer _ (BoolScalar b reg) = return (BoolT, BoolScalar b reg)
infer _ (DoubleScalar d reg) = return (DoubleT, DoubleScalar d reg)
infer _ (String str reg) = return (StringT, String str reg)
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
  unify reg t1 (PullArrayT tv :> BoolT)
  unify reg t2 (PullArrayT tv :> PushArrayT (VarL lvlVar) tv)
  unify reg t3 (PushArrayT (VarL lvlVar) tv)
  return (ProgramT (VarL lvlVar) (PullArrayT tv), While e1' e2' e3' reg)
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
  unify reg te1 IntT
  unify reg te2 (PushArrayT gridLevel IntT)
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
infer env (UnOp op e reg) = do
  (t, e') <- infer env e
  tret <- unifyUnOp op reg t
  return (tret, UnOp op e' reg)
infer env (BinOp op e1 e2 reg) = do
  (t1, e1') <- infer env e1
  (t2, e2') <- infer env e2
  tret <- unifyBinOp op reg t1 t2
  return (tret, BinOp op e1' e2' reg)
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

unifyAll :: Region -> [Type] -> TI Type
unifyAll _ [] = newtv
unifyAll _ [t] = return t
unifyAll r (t1 : t2 : ts) =
  do unify r t1 t2
     unifyAll r (t2 : ts)

unify1 :: Region -> Type -> Type -> Type -> TI Type
unify1 r t1' tret t1 = do
  unify r t1 t1'
  return tret

unifyUnOp :: UnOp -> Region -> Type -> TI Type
unifyUnOp AbsI r = unify1 r IntT IntT
unifyUnOp SignI r = unify1 r IntT IntT
unifyUnOp NegateI r = unify1 r IntT IntT
unifyUnOp Not r = unify1 r BoolT BoolT
unifyUnOp I2D r = unify1 r IntT DoubleT
unifyUnOp B2I r = unify1 r BoolT IntT
unifyUnOp CLZ r = unify1 r IntT IntT

unify2 :: Region -> Type -> Type -> Type -> Type -> Type -> TI Type
unify2 r t1' t2' tret t1 t2 = do
  unify r t1 t1'
  unify r t2 t2'
  return tret

unifyBinOp :: BinOp -> Region -> Type -> Type -> TI Type
unifyBinOp AddI r = unify2 r IntT IntT IntT
unifyBinOp SubI r = unify2 r IntT IntT IntT
unifyBinOp MulI r = unify2 r IntT IntT IntT
unifyBinOp DivI r = unify2 r IntT IntT IntT
unifyBinOp ModI r = unify2 r IntT IntT IntT
unifyBinOp MinI r = unify2 r IntT IntT IntT
unifyBinOp MaxI r = unify2 r IntT IntT IntT
unifyBinOp AddR r = unify2 r DoubleT DoubleT DoubleT
unifyBinOp EqI r = unify2 r IntT IntT BoolT
unifyBinOp NeqI r = unify2 r IntT IntT BoolT
unifyBinOp LtI r = unify2 r IntT IntT BoolT
unifyBinOp PowI r = unify2 r IntT IntT IntT
unifyBinOp ShiftLI r = unify2 r IntT IntT IntT
unifyBinOp ShiftRI r = unify2 r IntT IntT IntT
unifyBinOp AndI r = unify2 r IntT IntT IntT
unifyBinOp OrI r = unify2 r IntT IntT IntT
unifyBinOp XorI r = unify2 r IntT IntT IntT
unifyBinOp DivR r = unify2 r DoubleT DoubleT DoubleT
unifyBinOp PowR r = unify2 r DoubleT DoubleT DoubleT

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

typecheckProg :: TyEnv -> [Definition a] -> TI [Definition Type]
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
