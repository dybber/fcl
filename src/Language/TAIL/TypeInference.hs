module Language.TAIL.TypeInference where

import Data.List (nub)
import qualified Data.Map as Map

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAIL.Syntax

type TyEnv = Map.Map Name (TypeScheme Type)


type SubstTy = Map.Map TyVar Type
type SubstRank = Map.Map RankVar Rank
type SubstBase = Map.Map BaseVar BaseType

type Subst = (SubstTy, SubstRank, SubstBase)

emptySubst :: Subst
emptySubst = (Map.empty, Map.empty, Map.empty)

substTyVar :: Subst -> SubstTy
substTyVar (stv, _, _) = stv
substRank :: Subst -> SubstRank
substRank (_, srv, _) = srv
substBase :: Subst -> SubstBase
substBase (_, _, sbv) = sbv

data TVE = TVE {
    tyVarCounter :: Int
  , rankVarCounter :: Int
  , baseVarCounter :: Int
  , substitution :: Subst
  }

initTVE :: TVE
initTVE =
  TVE {
    tyVarCounter = 0
  , rankVarCounter = 0
  , baseVarCounter = 0
  , substitution = emptySubst
  }

data TypeError = UnificationError Type Type
               | BaseTypeUnificationError BaseType BaseType
               | RankUnificationError Rank Rank
               | NotImplementedError String
               | UnboundVariableError Name
               | UnboundTypeVariableError Name
               | OccursCheckFailed

type TI x = StateT TVE (Except TypeError) x

throwError :: TypeError -> TI a
throwError err = lift (throwE err)

runTI :: TI a -> TVE -> Either TypeError (a, TVE)
runTI m s = runExcept (runStateT m s)

newTyVar :: TI Type
newTyVar =
  do counter <- gets tyVarCounter
     modify (\s -> s {tyVarCounter = tyVarCounter s + 1})
     return (TyVarT counter)

newRankVar :: TI Rank
newRankVar =
  do counter <- gets rankVarCounter
     modify (\s -> s {rankVarCounter = rankVarCounter s + 1})
     return (RankVar counter)

newBaseVar :: TI BaseType
newBaseVar =
  do counter <- gets baseVarCounter
     modify (\s -> s {baseVarCounter = baseVarCounter s + 1})
     return (BaseVar counter)

extendSubstTyVar :: (TyVar,Type) -> TI ()
extendSubstTyVar (x,ty) =
  do (stv, srv, sbv) <- gets substitution
     modify (\s -> s { substitution = (Map.insert x ty stv, srv, sbv) })

extendSubstRankVar :: (RankVar,Rank) -> TI ()
extendSubstRankVar (x,ty) =
  do (stv, srv, sbv) <- gets substitution
     modify (\s -> s { substitution = (stv, Map.insert x ty srv, sbv) })

extendSubstBaseVar :: (BaseVar,BaseType) -> TI ()
extendSubstBaseVar (x,ty) =
  do (stv, srv, sbv) <- gets substitution
     modify (\s -> s { substitution = (stv, srv, Map.insert x ty sbv) })

lkup :: TyEnv -> Name -> TI (TypeScheme Type)
lkup env x =
  case Map.lookup x env of
    Just ty  -> return ty
    Nothing -> throwError (UnboundVariableError x)

ext :: TyEnv -> Name -> TypeScheme Type -> TyEnv
ext env x ty = Map.insert x ty env

baseVarSub :: SubstBase -> BaseType -> BaseType
baseVarSub sbv (BaseVar bv) =
  case Map.lookup bv sbv of
    Just bty -> baseVarSub sbv bty
    Nothing -> BaseVar bv
baseVarSub _ bty = bty

rankVarSub :: SubstRank -> Rank -> Rank
rankVarSub rsv (RankVar rv) =
  case Map.lookup rv rsv of
    Just r -> rankVarSub rsv r
    Nothing -> RankVar rv
rankVarSub _ rnk = rnk

-- apply substitution to type
applySub :: Subst -> Type -> Type
applySub s (ArrayT bty rnk) = ArrayT (baseVarSub (substBase s) bty) (rankVarSub (substRank s) rnk)
applySub s (VectorT bty rnk) = VectorT (baseVarSub (substBase s) bty) (rankVarSub (substRank s) rnk)
applySub s (SingleT bty rnk) = SingleT (baseVarSub (substBase s) bty) (rankVarSub (substRank s) rnk)
applySub s (SingleVecT bty rnk) = SingleVecT (baseVarSub (substBase s) bty) (rankVarSub (substRank s) rnk)
applySub s (t1 :> t2) = applySub s t1 :> applySub s t2
applySub (stv, rsv, sbv) (TyVarT tv) =
  case Map.lookup tv stv of
    Just t -> applySub (stv, rsv, sbv) t
    Nothing -> TyVarT tv

-- apply substitution to expression
applySubExp :: Subst -> Exp Type -> Exp Type
applySubExp _ (Scalar v)            = Scalar v
applySubExp s (UnaryOp op e)        = UnaryOp op (applySubExp s e)
applySubExp s (BinOp op e1 e2)      = BinOp op (applySubExp s e1) (applySubExp s e2)
applySubExp s (Var x t)             = Var x (applySub s t)
applySubExp s (Vector es t)         = Vector (map (applySubExp s) es) (applySub s t)
applySubExp s (Lam x t1 e t2)       = Lam x (applySub s t1) (applySubExp s e) (applySub s t2)
applySubExp s (Let x e ebody t)     = Let x (applySubExp s e) (applySubExp s ebody) (applySub s t)
applySubExp s (If e1 e2 e3 t)       = If (applySubExp s e1) (applySubExp s e2) (applySubExp s e3) (applySub s t)
applySubExp s (Infinity t)          = Infinity (applySub s t)
applySubExp s (ZipWith e1 e2 e3 t)  = ZipWith (applySubExp s e1) (applySubExp s e2) (applySubExp s e3) (applySub s t)
applySubExp s (Each e1 e2 t)        = Each (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Catenate e1 e2 t)    = Catenate (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (TypeAnnotation e1 t) = TypeAnnotation (applySubExp s e1) (applySub s t)
applySubExp s (Reduce e1 e2 e3 t)   = Reduce (applySubExp s e1) (applySubExp s e2) (applySubExp s e3) (applySub s t)
applySubExp s (Foldl e1 e2 t)       = Foldl (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Scan e1 e2 t)        = Scan (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Reshape e1 e2 t)     = Reshape (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Ravel e1 t)          = Ravel (applySubExp s e1) (applySub s t)
applySubExp s (Transpose e1 t)      = Transpose (applySubExp s e1) (applySub s t)
applySubExp s (Transpose2 e1 e2 t)  = Transpose2 (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Drop e1 e2 t)        = Drop (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Take e1 e2 t)        = Take (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Iota e1 t)           = Iota (applySubExp s e1) (applySub s t)
applySubExp s (Shape e1 t)          = Shape (applySubExp s e1) (applySub s t)
applySubExp s (Cons e1 e2 t)        = Cons (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Snoc e1 e2 t)        = Snoc (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (Rotate e1 e2 t)      = Rotate (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (EachV e1 e2 t)       = EachV (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (CatenateV e1 e2 t)   = CatenateV (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (DropV e1 e2 t)       = DropV (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (TakeV e1 e2 t)       = TakeV (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (IotaV e1 t)          = IotaV (applySubExp s e1) (applySub s t)
applySubExp s (ShapeV e1 t)         = ShapeV (applySubExp s e1) (applySub s t)
applySubExp s (ConsV e1 e2 t)       = ConsV (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (SnocV e1 e2 t)       = SnocV (applySubExp s e1) (applySubExp s e2) (applySub s t)
applySubExp s (RotateV e1 e2 t)     = RotateV (applySubExp s e1) (applySubExp s e2) (applySub s t)

unifyBaseType :: BaseType -> BaseType -> TI ()
unifyBaseType b1 b2 =
 let
   chaseBaseVar :: BaseType -> TI BaseType
   chaseBaseVar (BaseVar x) =
     do sbv <- gets (substBase . substitution)
        case Map.lookup x sbv of
          Just t -> chaseBaseVar t
          Nothing -> return (BaseVar x)
   chaseBaseVar t = return t

   unifyB :: BaseType -> BaseType -> TI ()
   unifyB IntT IntT = return ()
   unifyB DoubleT DoubleT = return ()
   unifyB BoolT BoolT = return ()
   unifyB CharT CharT = return ()
   unifyB (BaseVar v1) (BaseVar v2) | v1 == v2 = return ()
   unifyB (BaseVar v) bty = extendSubstBaseVar (v, bty)
   unifyB bty (BaseVar v) = extendSubstBaseVar (v, bty)
   unifyB bty1 bty2 = throwError (BaseTypeUnificationError bty1 bty2)
 in 
  do b1' <- chaseBaseVar b1
     b2' <- chaseBaseVar b2
     unifyB b1' b2'

unifyRank :: Rank -> Rank -> TI ()
unifyRank r1 r2 =
  do r1' <- chaseRank r1
     r2' <- chaseRank r2
     unifyR r1' r2'

chaseRank :: Rank -> TI Rank
chaseRank (RankVar x) =
     do srv <- gets (substRank . substitution)
        case Map.lookup x srv of
          Just t -> chaseRank t
          Nothing -> return (RankVar x)
chaseRank r = return r

unifyR :: Rank -> Rank -> TI ()
unifyR r1@(Rank i1) r2@(Rank i2)
  | i1 == i2  = return ()
  | otherwise = throwError (RankUnificationError r1 r2)
unifyR (RankVar rv1) (RankVar rv2) | rv1 == rv2 = return ()
unifyR (RankVar rv) r = extendSubstRankVar (rv, r)
unifyR r (RankVar rv) = extendSubstRankVar (rv, r)
-- unifyR (Rank i) (RankAdd r1 r2) = undefined
--- TODO

tvchase :: Type -> TI Type
tvchase (TyVarT x) =
  do stv <- gets (substTyVar . substitution)
     case Map.lookup x stv of
       Just t -> tvchase t
       Nothing -> return (TyVarT x)
tvchase t = return t

-- | The unification. If unification failed, return the reason
unify :: Type -> Type -> TI ()
unify t1 t2 = do
  t1' <- tvchase t1
  t2' <- tvchase t2
  unify' t1' t2'

-- | If either t1 or t2 are type variables, they are definitely unbound
unify' :: Type -> Type -> TI ()
unify' (t1a :> t1r) (t2a :> t2r) =
  do unify t1r t2r
     unify t1a t2a
unify' (ArrayT b1 r1) (ArrayT b2 r2) =
  do unifyBaseType b1 b2
     unifyRank r1 r2
unify' (VectorT b1 r1) (VectorT b2 r2) =
  do unifyBaseType b1 b2
     unifyRank r1 r2
unify' (SingleT b1 r1) (SingleT b2 r2) =
  do unifyBaseType b1 b2
     unifyRank r1 r2
unify' (SingleVecT b1 r1) (SingleVecT b2 r2) =
  do unifyBaseType b1 b2
     unifyRank r1 r2
unify' (TyVarT v1) t2 = unify_fv v1 t2
unify' t1 (TyVarT v2) = unify_fv v2 t1
unify' t1 t2 = throwError (UnificationError t1 t2)

unify_fv :: TyVar -> Type -> TI ()
unify_fv tv t@(TyVarT tv') | tv == tv'   = return ()
                         | otherwise = extendSubstTyVar (tv, t)
unify_fv tv t = do
  s <- gets substitution
  let c = occurs s tv t 
  if c then throwError OccursCheckFailed
       else extendSubstTyVar (tv, t)

occurs :: Subst -> TyVar -> Type -> Bool
occurs _ _ (ArrayT _ _) = False
occurs _ _ (VectorT _ _) = False
occurs _ _ (SingleT _ _) = False
occurs _ _ (SingleVecT _ _) = False
occurs s tv (t1 :> t2) = occurs s tv t1 || occurs s tv t2
occurs (stv, rsv, sbv) tv (TyVarT tv2) =
    case Map.lookup tv2 stv of
         Just t  -> occurs (stv, rsv, sbv) tv t
         Nothing -> tv == tv2

-- -- TODO
infer :: TyEnv -> Exp ty -> TI (Type, Exp Type)
infer _ (Scalar (IntV i)) = return (scalarTy IntT, Scalar (IntV i))
infer _ (Scalar (BoolV b)) = return (scalarTy BoolT, Scalar (BoolV b))
infer _ (Scalar (DoubleV d)) = return (scalarTy DoubleT, Scalar (DoubleV d))
infer env (Var x _) = do
  ty <- lkup env x
  ty' <- instantiate ty
  return (ty', Var x ty')
infer env (Lam x _ e _) = do
  tv <- newTyVar
  (te, e') <- infer (ext env x (TypeScheme ([],[],[]) tv)) e
  return (tv :> te, Lam x tv e' te)
-- infer env (Let x e ebody _) = do
--   (ts,e') <- generalize (infer env e)
--   (t,body) <- infer (ext env x ts) ebody
--   return (t, Let x e' body t)

unifyAll :: [Type] -> TI Type
unifyAll [] = newTyVar
unifyAll [t] = return t
unifyAll (t1 : t2 : ts) =
  do unify t1 t2
     unifyAll (t2 : ts)

instantiate :: TypeScheme Type -> TI Type
instantiate (TypeScheme (tyvars, rvs, bvs) t) = do
  stv <- mkFreshTyVars tyvars
  srv <- mkFreshRankVars rvs
  sbv <- mkFreshBaseVars bvs
  return (applySub (stv, srv, sbv) t)
 where
   mkFreshTyVars :: [TyVar] -> TI SubstTy
   mkFreshTyVars [] = return Map.empty
   mkFreshTyVars (tv:tvs) = do
     stv <- mkFreshTyVars tvs
     fresh <- newTyVar
     return (Map.insert tv fresh stv)

   mkFreshRankVars :: [RankVar] -> TI SubstRank
   mkFreshRankVars [] = return Map.empty
   mkFreshRankVars (tv:tvs) = do
     stv <- mkFreshRankVars tvs
     fresh <- newRankVar
     return (Map.insert tv fresh stv)

   mkFreshBaseVars :: [BaseVar] -> TI SubstBase
   mkFreshBaseVars [] = return Map.empty
   mkFreshBaseVars (tv:tvs) = do
     stv <- mkFreshBaseVars tvs
     fresh <- newBaseVar
     return (Map.insert tv fresh stv)

freeTyVar :: (SubstTy, Int) -> [TyVar]
freeTyVar (stv,countTV) =
  filter (\v -> not (Map.member v stv)) [0..countTV-1]

freeRankVar :: (SubstRank, Int) -> [RankVar]
freeRankVar (srv,countRV) =
  filter (\v -> not (Map.member v srv)) [0..countRV-1]

freeBaseVar :: (SubstBase, Int) -> [BaseVar]
freeBaseVar (sbv,countBV) =
  filter (\v -> not (Map.member v sbv)) [0..countBV-1]

generalize :: TI (Type, Exp Type) -> TI (TypeScheme Type, Exp Type)
generalize ta = do
 envBefore <- get                  -- type env before ta is executed
 (t,e') <- ta
 s_after <- gets substitution  -- type env after ta is executed
 let t' = applySub s_after t
 let s_before = substitution envBefore
 let tvdep = tvdependentset (tyVarCounter envBefore)  (substTyVar s_before) s_after
 let rvdep = rvdependentset (rankVarCounter envBefore) (substRank s_before) s_after
 let bvdep = bvdependentset (baseVarCounter envBefore) (substBase s_before) s_after
 let fv = filter (not . tvdep) (nub (freeTyVars t'))
 let frv = filter (not . rvdep) (nub (freeBaseVars t'))
 let fbv = filter (not . bvdep) (nub (freeRankVars t'))
 return (TypeScheme (fv, frv, fbv) t', e')

-- | Compute (quite unoptimally) the characteristic function of the set 
--  forall tvb \in fv(s_before). Union fv(applySub(s_after,tvb))
tvdependentset :: Int -> SubstTy -> Subst -> (TyVar -> Bool)
tvdependentset i s_before s_after =
  \tv -> any (\tvb -> occurs s_after tv (TyVarT tvb)) (freeTyVar (s_before,i))

rvdependentset :: Int -> SubstRank -> Subst -> (RankVar -> Bool)
rvdependentset i s_before s_after =
  \tv -> any (\tvb -> occurs s_after tv (RankVar tvb)) (freeRankVar (s_before,i))

bvdependentset :: Int -> SubstBase -> Subst -> (BaseVar -> Bool)
bvdependentset i s_before s_after =
  \tv -> any (\tvb -> occurs s_after tv (BaseVar tvb)) (freeBaseVar (s_before,i))
