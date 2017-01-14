-- | Inlines all top-level definitions that should not be
-- generated. Only keeps those where OpenCL-kernels should be emitted.
-- The result is a list of definitions without references to any other
-- top-level definition.
module FCL.Inline (inline) where

import qualified Data.Map as Map

import FCL.Core.SourceRegion
import FCL.Core.Identifier
import FCL.Core.Syntax

type Env = Map.Map Identifier (TypeScheme Type, Exp Type)

emptyEnv :: Env
emptyEnv = Map.empty

inline :: [Definition Type] -> Definition Type
inline prog = inlineFuncs emptyEnv prog

inlineFuncs :: Env -> [Definition Type] -> Definition Type
inlineFuncs _ [] = error "No main function found"
inlineFuncs env (d : ds) =
  let e' = inlineAll env (defBody d)
  in 
    if identToString (defVar d) == "main"
    then d { defBody = e'}
    else inlineFuncs (Map.insert (defVar d) (defTypeScheme d,e') env) ds
  
inlineAll :: Env -> Exp Type -> Exp Type
inlineAll _ e@(Literal _ _) = e
inlineAll _ e@(BlockSize _) = e
inlineAll env (Var v ty _) =
  case Map.lookup v env of
    Just (_, e) -> inlineAll env e  -- TODO fix up types (currently we perform a re-run of the typechecker after inlining)
    Nothing -> Var v ty Missing
inlineAll env (UnaryOp op e0 reg)        = UnaryOp op   (inlineAll env e0) reg
inlineAll env (BinaryOp op e0 e1 reg)    = BinaryOp op  (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Vec es ety reg)           = Vec          (map (inlineAll env) es) ety reg
inlineAll env (Lamb v ty0 ebody ty1 reg) = Lamb v ty0   (inlineAll (Map.delete v env) ebody) ty1 reg
inlineAll env (LambLvl lvlvar ebody ty reg) = LambLvl lvlvar (inlineAll env ebody) ty reg
inlineAll env (Let v e ebody ty reg)     = Let v        (inlineAll env e) (inlineAll (Map.delete v env) ebody) ty reg
inlineAll env (App e0 e1)                = App          (inlineAll env e0) (inlineAll env e1)
inlineAll env (AppLvl e lvl)             = AppLvl       (inlineAll env e) lvl
inlineAll env (Cond e0 e1 e2 ty reg)     = Cond         (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) ty reg
inlineAll env (Pair e0 e1 reg)           = Pair         (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Proj1E e0 reg)            = Proj1E       (inlineAll env e0) reg
inlineAll env (Proj2E e0 reg)            = Proj2E       (inlineAll env e0) reg
inlineAll env (Index e0 e1 reg)          = Index        (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (LengthPull e0 reg)        = LengthPull   (inlineAll env e0) reg
inlineAll env (LengthPush e0 reg)        = LengthPush   (inlineAll env e0) reg
inlineAll env (For e0 e1 e2 reg)         = For          (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg
inlineAll env (Power e0 e1 e2 reg)       = Power        (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg
inlineAll env (While e0 e1 e2 reg)       = While        (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg
inlineAll env (WhileSeq e0 e1 e2 reg)    = WhileSeq     (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg
inlineAll env (GeneratePull e0 e1 reg)   = GeneratePull (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (MapPull e0 e1 reg)        = MapPull      (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (MapPush e0 e1 reg)        = MapPush      (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Push lvl e0 reg)          = Push         lvl (inlineAll env e0) reg
inlineAll env (Force e0 reg)             = Force        (inlineAll env e0) reg
inlineAll env (Interleave e0 e1 e2 reg)  = Interleave   (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg
inlineAll env (Return lvl e0 reg)        = Return       lvl (inlineAll env e0) reg
inlineAll env (Bind e0 e1 reg)           = Bind         (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (ReadIntCSV e0 reg)        = ReadIntCSV (inlineAll env e0) reg
inlineAll env (ForceAndPrint e0 e1 reg)  = ForceAndPrint (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Benchmark e0 e1 reg)      = Benchmark (inlineAll env e0) (inlineAll env e1) reg
