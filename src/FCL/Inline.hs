module FCL.Inline (inline) where

import qualified Data.Map as Map

import FCL.Core.Identifier
import FCL.External.Syntax
import FCL.Type.Polymorphic

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

inlineAll env (Symbol v ty reg) =
  case Map.lookup v env of
    Just (_, e) -> inlineAll env e  -- TODO fix up types (currently we perform a re-run of the typechecker after inlining)
    Nothing -> Symbol v ty reg
inlineAll env (UnaryOp op e0 reg)        = UnaryOp op   (inlineAll env e0) reg
inlineAll env (BinaryOp op e0 e1 reg)    = BinaryOp op  (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (Vec es ety reg)           = Vec          (map (inlineAll env) es) ety reg
inlineAll env (Lamb v ty0 ebody ty1 reg) = Lamb v ty0   (inlineAll (Map.delete v env) ebody) ty1 reg
inlineAll env (LambLvl lvlvar ebody ty reg) = LambLvl lvlvar (inlineAll env ebody) ty reg
inlineAll env (Let v e ebody ty reg)     = Let v        (inlineAll env e) (inlineAll (Map.delete v env) ebody) ty reg
inlineAll env (App e0 e1 reg)            = App          (inlineAll env e0) (inlineAll env e1) reg
inlineAll env (AppLvl e lvl reg)         = AppLvl       (inlineAll env e) lvl reg
inlineAll env (Cond e0 e1 e2 ty reg)     = Cond         (inlineAll env e0) (inlineAll env e1) (inlineAll env e2) ty reg
inlineAll env (Pair e0 e1 reg)           = Pair         (inlineAll env e0) (inlineAll env e1) reg
