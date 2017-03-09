module FCL.Substitution (
  Substitutable(..),
  FTV(..),
  emptySubst,
  singleton,
  union,
  insertTy, lookupTy, memberTy,
  insertLvl, lookupLvl, memberLvl,
  Subst(..)
)
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import FCL.Core.PolyLevel
import FCL.Core.Polytyped

import FCL.Infer.TypeEnvironment

------------------
-- Substitution --
------------------
data Subst = Subst (Map.Map TyVar Type) (Map.Map LvlVar Level)
  deriving (Eq, Show)

emptySubst :: Subst
emptySubst = Subst Map.empty Map.empty

singleton :: TyVar -> Type -> Subst
singleton var type' = Subst (Map.singleton var type') Map.empty

union :: Subst -> Subst -> Subst
(Subst s1 slvl1) `union` (Subst s2 slvl2) = Subst (s1 `Map.union` s2) (slvl1 `Map.union` slvl2)

insertTy :: Subst -> (TyVar, Type) -> Subst
insertTy (Subst s slvl) (tvar, type') = Subst (Map.insert tvar type' s) slvl

lookupTy :: TyVar -> Subst -> Maybe Type
lookupTy tvar (Subst s _) = Map.lookup tvar s

insertLvl :: Subst -> (LvlVar, Level) -> Subst
insertLvl (Subst s slvl) (lvlvar, lvl) = Subst s (Map.insert lvlvar lvl slvl)

lookupLvl :: LvlVar -> Subst -> Maybe Level
lookupLvl lvlvar (Subst _ slvl) = Map.lookup lvlvar slvl

memberTy :: TyVar -> Subst -> Bool
memberTy tvar (Subst s _) = Map.member tvar s

memberLvl :: LvlVar -> Subst -> Bool
memberLvl lvar (Subst _ slvl) = Map.member lvar slvl

class Substitutable a where
  apply :: Subst -> a -> a

class FTV a where
  ftv :: a -> Set.Set TyVar

instance Substitutable Level where
  apply _ Zero               = Zero
  apply s (Step lvl)         = Step (apply s lvl)
  apply s lvl@(VarL lvlvar) =
    case lookupLvl lvlvar s of
      Nothing -> lvl
      Just lvl' ->
        if lvl' /= lvl
        then apply s lvl'
        else lvl'

instance Substitutable Type where
  apply _ IntT               = IntT
  apply _ BoolT              = BoolT
  apply _ DoubleT            = DoubleT
  apply _ StringT            = StringT
  apply _ UnitT              = UnitT
  apply s t@(VarT tyvar) =
    case lookupTy tyvar s of
      Nothing -> t
      Just t' ->
        if t' /= t
        then apply s t'
        else t'
  apply s (t1 :> t2)         = (apply s t1) :> (apply s t2)
  apply s (t1 :*: t2)        = (apply s t1) :*: (apply s t2)
  apply s (PullArrayT t)     = PullArrayT (apply s t)
  apply s (PushArrayT lvl t) = PushArrayT (apply s lvl) (apply s t)
  apply s (ProgramT lvl t)   = ProgramT (apply s lvl) (apply s t)

instance FTV Type where
  ftv IntT             = Set.empty
  ftv BoolT            = Set.empty
  ftv DoubleT          = Set.empty
  ftv StringT          = Set.empty
  ftv UnitT            = Set.empty
  ftv (t1 :> t2)       = ftv t1 `Set.union` ftv t2
  ftv (t1 :*: t2)      = ftv t1 `Set.union` ftv t2
  ftv (VarT v)         = Set.singleton v
  ftv (PullArrayT t)   = ftv t
  ftv (PushArrayT _ t) = ftv t
  ftv (ProgramT _ t)   = ftv t

instance Substitutable Exp where
  apply s e =
    case e of
      Literal l ty       -> Literal l (apply s ty)
      Unit ty            -> Unit (apply s ty)
      Symbol x lvlargs ty -> Symbol x (map (apply s) lvlargs) (apply s ty)
      App f p ty         -> App (apply s f) (apply s p) (apply s ty)
      Pair e1 e2 ty      -> Pair (apply s e1) (apply s e2) (apply s ty)
      Cond e1 e2 e3 ty   -> Cond (apply s e1) (apply s e2) (apply s e3) (apply s ty)
      Lamb x b ty        -> Lamb x (apply s b) (apply s ty)
      Let x tysc lvlparams e0 e1 ty ->
        Let x (apply s tysc) lvlparams (apply s e0) (apply s e1) (apply s ty)

instance Substitutable TypeScheme where
  apply (Subst s slvl) (TypeScheme lvls qs t) =
    let s' = Subst (foldr Map.delete s qs) (foldr Map.delete slvl lvls)
    in TypeScheme lvls qs (apply s' t)

instance FTV TypeScheme where
  ftv (TypeScheme _ qs t) = ftv t `Set.difference` (Set.fromList qs)

instance Substitutable TypeEnvironment where
  apply s (TypeEnvironment env) = TypeEnvironment (Map.map (apply s) env)

instance FTV TypeEnvironment where
  ftv (TypeEnvironment env) = foldr (Set.union . ftv) Set.empty (Map.elems env)

