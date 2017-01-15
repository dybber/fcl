module FCL.Infer.Substitution
  (Subst, tvsub, tvsubExp)
where

import qualified Data.Map as Map

import FCL.Core.Syntax

type Subst = (Map.Map TyVar Type,
              Map.Map LvlVar Level)

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
tvsubExp s (Var x t reg)             = Var x (tvsub s t) reg
tvsubExp s (Vec es t reg)            = Vec (map (tvsubExp s) es) (tvsub s t) reg
tvsubExp s (Lamb x t1 e t2 reg)      = Lamb x (tvsub s t1) (tvsubExp s e) (tvsub s t2) reg
tvsubExp s (App e1 e2)               = App (tvsubExp s e1) (tvsubExp s e2)
tvsubExp s (LambLvl lvlvar e t reg)  = LambLvl lvlvar (tvsubExp s e) (tvsub s t) reg
tvsubExp s (AppLvl e lvl)            = AppLvl (tvsubExp s e) (lvlVarSub s lvl)
tvsubExp s (Let x e ebody t reg)     = Let x (tvsubExp s e) (tvsubExp s ebody) (tvsub s t) reg
tvsubExp s (Cond e1 e2 e3 t reg)     = Cond (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) (tvsub s t) reg
tvsubExp s (Pair e1 e2 reg)          = Pair (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Proj1E e1 reg)           = Proj1E (tvsubExp s e1) reg
tvsubExp s (Proj2E e1 reg)           = Proj2E (tvsubExp s e1) reg
tvsubExp s (Index e1 e2 reg)         = Index (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (LengthPull e1 reg)       = LengthPull (tvsubExp s e1) reg
tvsubExp s (LengthPush e1 reg)       = LengthPush (tvsubExp s e1) reg
tvsubExp s (For e1 e2 e3 reg)        = For (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (Power e1 e2 e3 reg)      = Power (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (While e1 e2 e3 reg)      = While (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (WhileSeq e1 e2 e3 reg)   = WhileSeq (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp s (GeneratePull e1 e2 reg)  = GeneratePull (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (MapPull e1 e2 reg)       = MapPull (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (MapPush e1 e2 reg)       = MapPush (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Force e1 reg)            = Force (tvsubExp s e1) reg
tvsubExp s (Push lvl e1 reg)         = Push (lvlVarSub s lvl) (tvsubExp s e1) reg
tvsubExp s (Interleave e1 e2 e3 reg) = Interleave (tvsubExp s e1) (tvsubExp s e2) (tvsubExp s e3) reg
tvsubExp _ (BlockSize reg)           = BlockSize reg
tvsubExp s (Return lvl e1 reg)       = Return (lvlVarSub s lvl) (tvsubExp s e1) reg
tvsubExp s (Bind e1 e2 reg)          = Bind (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (ReadIntCSV e1 reg)       = ReadIntCSV (tvsubExp s e1) reg
tvsubExp s (ForceAndPrint e1 e2 reg) = ForceAndPrint (tvsubExp s e1) (tvsubExp s e2) reg
tvsubExp s (Benchmark e1 e2 reg)     = Benchmark (tvsubExp s e1) (tvsubExp s e2) reg
