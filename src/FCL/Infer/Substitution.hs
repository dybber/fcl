module FCL.Infer.Substitution
  (Subst, emptySubst, union, tvsub, tvsubExp)
where

import qualified Data.Map as Map

import FCL.External.Syntax
import FCL.Type.Polymorphic

type Subst = (Map.Map TyVar Type,
              Map.Map LvlVar Level)

union :: Subst -> Subst -> Subst
(substTy0, substlvl0) `union` (substTy1, substlvl1) =
  (substTy0 `Map.union` substTy1, substlvl0 `Map.union` substlvl1)

emptySubst :: Subst
emptySubst = (Map.empty, Map.empty)

tvsub :: Subst -> Type -> Type
tvsub _ IntT                = IntT
tvsub _ BoolT               = BoolT
tvsub _ DoubleT             = DoubleT
tvsub _ StringT             = StringT
tvsub _ UnitT               = UnitT
tvsub s (t1 :> t2)          = tvsub s t1 :> tvsub s t2
tvsub s (lvl :-> t2)        = lvl :-> tvsub s t2
tvsub s (t1 :*: t2)         = tvsub s t1 :*: tvsub s t2
tvsub s (PullArrayT t)      = PullArrayT (tvsub s t)
tvsub s (PushArrayT lvl t)  = PushArrayT (lvlVarSub s lvl) (tvsub s t)
tvsub s (ProgramT lvl t)    = ProgramT (lvlVarSub s lvl) (tvsub s t)
tvsub (stv, slvl) (VarT tv) =
  case Map.lookup tv stv of
    Just t -> tvsub (stv, slvl) t
    Nothing -> VarT tv

lvlVarSub :: Subst -> Level -> Level
lvlVarSub _ Zero                   = Zero
lvlVarSub s (Step l)               = Step (lvlVarSub s l)
lvlVarSub (stv,slvl) (VarL lvlVar) =
  case Map.lookup lvlVar slvl of
    Just t -> lvlVarSub (stv,slvl) t
    Nothing -> VarL lvlVar

tvsubExp :: Subst -> Exp Type -> Exp Type
tvsubExp _ (Literal l reg)           = Literal l reg
tvsubExp s (UnaryOp op e reg)        = UnaryOp op (tvsubExp s e) reg
tvsubExp s (BinaryOp op e1 e2 reg)   = BinaryOp op (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Symbol x t reg)          = Symbol x (tvsub s t) reg
tvsubExp s (Vec es t reg)            = Vec (map (tvsubExp s) es) (tvsub s t) reg
tvsubExp s (Lamb x t1 e t2 reg)      = Lamb x (tvsub s t1) (tvsubExp s e) (tvsub s t2) reg
tvsubExp s (App e1 e2 reg)           = App (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (LambLvl lvlvar e t reg)  = LambLvl lvlvar (tvsubExp s e) (tvsub s t) reg
tvsubExp s (AppLvl e lvl reg)        = AppLvl (tvsubExp s e) (lvlVarSub s lvl) reg
tvsubExp s (Let x e ebody t reg)     = Let x (tvsubExp s e) (tvsubExp s ebody) (tvsub s t) reg
tvsubExp s (Cond e1 e2 e3 t reg)     = Cond (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) (tvsub s t) reg
tvsubExp s (Pair e1 e2 reg)          = Pair (tvsubExp s e1) (tvsubExp s e2) reg
