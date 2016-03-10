-- Inspired by http://okmij.org/ftp/Computation/FLOLAC/lecture.pdf
module Language.FCL.Inline (inline) where

import qualified Data.Map as Map

import Language.FCL.Syntax

type Env = Map.Map Variable (TypeScheme Type, Exp Type)

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
     then (Definition (defVar d) Nothing (defTypeScheme d) True e') : rest
     else rest
  
inlineAll :: Env -> Exp Type -> Exp Type
inlineAll _ e@(IntScalar _) = e
inlineAll _ e@(DoubleScalar _) = e
inlineAll _ e@(BoolScalar _) = e
inlineAll _ e@LocalSize = e
inlineAll env (Var v ty) =
  case Map.lookup v env of
    Just (_, e) -> inlineAll env e  -- TODO fix up types
    Nothing -> Var v ty
inlineAll env (UnOp op e0)           = UnOp op      (inlineAll env e0)
inlineAll env (BinOp op e0 e1)       = BinOp op     (inlineAll env e0) (inlineAll env e1)
inlineAll env (Vec es ety)           = Vec          (map (inlineAll env) es) ety
inlineAll env (Lamb v ty0 ebody ty1) = Lamb v ty0   (inlineAll (Map.delete v env) ebody) ty1
inlineAll env (Let v e ebody ty)     = Let v        (inlineAll env e) (inlineAll (Map.delete v env) ebody) ty
inlineAll env (App e0 e1)            = App          (inlineAll env e0) (inlineAll env e1)
inlineAll env (Cond e0 e1 e2 ty)     = Cond         (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) ty
inlineAll env (Pair e0 e1)           = Pair         (inlineAll env e0) (inlineAll env e1)
inlineAll env (Proj1E e0)            = Proj1E       (inlineAll env e0)
inlineAll env (Proj2E e0)            = Proj2E       (inlineAll env e0)
inlineAll env (Index e0 e1)          = Index        (inlineAll env e0) (inlineAll env e1)
inlineAll env (Length e0)            = Length       (inlineAll env e0)
inlineAll env (Fixpoint e0 e1 e2)    = Fixpoint     (inlineAll env e0) (inlineAll env e1) (inlineAll env e2)
inlineAll env (Generate lvl e0 e1)   = Generate lvl (inlineAll env e0) (inlineAll env e1)
inlineAll env (Map e0 e1)            = Map          (inlineAll env e0) (inlineAll env e1)
inlineAll env (ForceLocal e0)        = ForceLocal   (inlineAll env e0)
inlineAll env (Assemble e0 e1 e2)    = Assemble     (inlineAll env e0) (inlineAll env e1) (inlineAll env e2)
inlineAll env (Scanl e0 e1 e2)       = Scanl        (inlineAll env e0) (inlineAll env e1) (inlineAll env e2)

