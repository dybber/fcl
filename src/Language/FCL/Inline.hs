-- Inspired by http://okmij.org/ftp/Computation/FLOLAC/lecture.pdf
module Language.FCL.Inline (inline) where

import qualified Data.Map as Map

import Language.FCL.SourceRegion
import Language.FCL.Syntax

type Env = Map.Map Name (TypeScheme Type, Exp Type)

emptyEnv :: Env
emptyEnv = Map.empty

inline :: Program Type -> Program Type
inline prog = inlineFuncs emptyEnv prog

inlineFuncs :: Env -> Program Type -> Program Type
inlineFuncs _ [] = []
inlineFuncs env (d : ds) =
  let e' = inlineAll env (defBody d)
      rest = inlineFuncs (Map.insert (defVar d) (defTypeScheme d,e') env) ds
  in if defEmitKernel d
     then d { defBody = e'} : rest
     else rest
  
inlineAll :: Env -> Exp Type -> Exp Type
inlineAll _ e@(IntScalar _ _) = e
inlineAll _ e@(DoubleScalar _ _) = e
inlineAll _ e@(BoolScalar _ _) = e
inlineAll _ e@(LocalSize _) = e
inlineAll env (Var v ty _) =
  case Map.lookup v env of
    Just (_, e) -> inlineAll env e  -- TODO fix up types
    Nothing -> Var v ty Missing
inlineAll env (UnOp op e0 reg)           = UnOp op      (inlineAll env e0) reg
inlineAll env (BinOp op e0 e1 reg)       = BinOp op     (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Vec es ety reg)           = Vec          (map (inlineAll env) es) ety reg
inlineAll env (Lamb v ty0 ebody ty1 reg) = Lamb v ty0   (inlineAll (Map.delete v env) ebody) ty1 reg
inlineAll env (Let v e ebody ty reg)     = Let v        (inlineAll env e) (inlineAll (Map.delete v env) ebody) ty reg
inlineAll env (App e0 e1)                = App          (inlineAll env e0) (inlineAll env e1)
inlineAll env (Cond e0 e1 e2 ty reg)     = Cond         (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) ty reg
inlineAll env (Pair e0 e1 reg)           = Pair         (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Proj1E e0 reg)            = Proj1E       (inlineAll env e0) reg
inlineAll env (Proj2E e0 reg)            = Proj2E       (inlineAll env e0) reg
inlineAll env (Index e0 e1 reg)          = Index        (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (LengthPull e0 reg)        = LengthPull   (inlineAll env e0) reg
inlineAll env (LengthPush e0 reg)        = LengthPush   (inlineAll env e0) reg
inlineAll env (While e0 e1 e2 reg)       = While        (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg
inlineAll env (WhileSeq e0 e1 e2 reg)    = WhileSeq     (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg
inlineAll env (GeneratePull e0 e1 reg)   = GeneratePull (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (MapPull e0 e1 reg)        = MapPull      (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (MapPush e0 e1 reg)        = MapPush      (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Push lvl e0 t reg)        = Push         lvl (inlineAll env e0) t reg
inlineAll env (Force e0 reg)             = Force        (inlineAll env e0) reg
inlineAll env (Concat e0 e1 reg)         = Concat       (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Scanl e0 e1 e2 reg)       = Scanl        (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) reg

