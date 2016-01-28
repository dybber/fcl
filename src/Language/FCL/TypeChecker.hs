module Language.FCL.TypeChecker
 (typecheck)
where

import Language.FCL.Syntax
import qualified Data.Map as Map

type TyEnv = Map.Map Variable Type

emptyTyEnv :: TyEnv
emptyTyEnv = Map.empty

emptyExpEnv :: ExpEnv
emptyExpEnv = []

typecheck :: Prog ty -> [(Variable, Exp Type)]
typecheck p = typecheckProg emptyTyEnv emptyExpEnv p

makeLetChain :: Type -> ExpEnv -> Exp Type -> Exp Type
makeLetChain _ [] ebody = ebody
makeLetChain ty ((v,e):es) ebody =
  Let v e (makeLetChain ty es ebody) ty

typecheckProg :: TyEnv -> ExpEnv -> Prog ty -> [(Variable, Exp Type)]
typecheckProg _ _ [] = []
typecheckProg tenv eenv (KernelDef v : ds) =
  case Map.lookup v tenv of
    Just ty -> (v, makeLetChain ty (reverse eenv) (Var v ty)) : typecheckProg tenv eenv ds
    Nothing -> error ("Variable " ++ v ++ " not defined in kernel declaration")
typecheckProg tenv eenv (Definition v _ e : ds) =
  let (ety, ty) = check tenv e
  in typecheckProg (Map.insert v ty tenv) ((v, ety) : eenv) ds
          
check :: TyEnv -> Exp ty -> (Exp Type, Type)
check _ (IntScalar v) = (IntScalar v, IntT)
check _ (DoubleScalar v) = (DoubleScalar v, DoubleT)
check _ (BoolScalar v) = (BoolScalar v, BoolT)
check env (UnOp op e0) =
  let (e0', ty0) = check env e0
  in (UnOp op e0',
      checkUnOp op ty0)
check env (BinOp op e0 e1) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
  in (BinOp op e0' e1',
      checkBinOp op ty0 ty1)
check env (Var x _) =
  case (Map.lookup x env) of
    Just ty_env -> (Var x ty_env, ty_env)
    Nothing     -> error ("Unbound variable " ++ x)
check env (Lamb x ty e _) =
  let (e', ty') = check (Map.insert x ty env) e
  in (Lamb x ty e' ty', ty :> ty')
check env (Let x e0 e1 _) = -- TODO: let-polymorphism yet
  let (e0', ty) = check env e0
      (e1', ty') = check (Map.insert x ty env) e1
  in (Let x e0' e1' ty', ty')
check env (App e0 e1) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
  in case ty0 of
       ty1' :> ty2 | ty1 == ty1' -> (App e0' e1', ty2)
                   | otherwise -> error $ concat ["Argument type does not match parameter type in function call.\n",
                                                  "Expected: ", show ty1', "\n",
                                                  "Got: ", show ty1, "\n"]
       _ -> error "Non-function type in function position of application"
check env (Cond e0 e1 e2 _) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
      (e2', ty2) = check env e2
  in case ty0 of
       BoolT | ty1 == ty2 -> (Cond e0' e1' e2' ty1,
                              ty1)
             | otherwise  -> error "Types of conditional branches does not match"
       _ -> error "first argument to conditional must be boolean typed"
check env (Pair e0 e1) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
  in (Pair e0' e1', ty0 :*: ty1)
check env (Proj1E e0) =
  let (e0', ty) = check env e0
  in case ty of
       ty0 :*: _ -> (Proj1E e0', ty0)
       _ -> error "Projection must be applied to expression of product type"
check env (Proj2E e0) =
  let (e0', ty) = check env e0
  in case ty of
       ty0 :*: _ -> (Proj2E e0', ty0)
       _ -> error "Projection must be applied to expression of product type"
check env (Index e0 e1) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
  in case (ty0, ty1) of
       (ArrayT _ ty_elem, IntT) -> (Index e0' e1', ty_elem)
       (_, _) -> error "Index must receive Array as first argument and integer as second argument"
check env (Length e0) =
  let (e0', ty0) = check env e0
  in case ty0 of
       ArrayT _ _ -> (Length e0', IntT)
       _ -> error "Argument to length must be an array"
check env (Fixpoint cond step initv) =
  let (cond', cond_ty)  = check env cond
      (step', step_ty)  = check env step
      (initv', init_ty) = check env initv
  in case (cond_ty, step_ty) of
       (ty0 :> ty1, ty0' :> ty1')
         | ty0 /= init_ty  -> error "fixpoint: argument to conditional does not match initial value"
         | ty1 /= BoolT    -> error "fixpoint: conditional does not return boolean"
         | ty0' /= init_ty -> error "fixpoint: stepper-function argument type does not match initial value"
         | ty1' /= init_ty -> error "fixpoint: result type of stepper-function does not match initial value"
         | otherwise       -> (Fixpoint cond' step' initv', init_ty)
       _ -> error "fixpoint: Conditional and stepper should be functions (for now)"
check env (Generate lvl e0 e1) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
  in case (ty0, ty1) of
    (IntT, IntT :> ty1') -> (Generate lvl e0' e1', ArrayT lvl ty1')
    _ -> error "do"
check env (Map e0 e1) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
  in case (ty0, ty1) of
       (ty0' :> ty1', ArrayT lvl ty_elem) | ty0' == ty_elem -> (Map e0' e1', ArrayT lvl ty1')
                                          | otherwise -> error "Map: function argument type and array element type does not match"
       (_ :> _', ty) -> error ("Map expects an array as second argument, got: " ++ show ty)
       (ty, ArrayT _ _) -> error ("Map expects a function as first argument, got: " ++ show ty)
       _ -> error ("Map expects a function and an array as arguments, got: " ++ show ty0 ++ ", " ++ show ty1)
check env (ForceLocal e0) =
  let (e0', ty0) = check env e0
  in case ty0 of
       ArrayT lvl ty0' -> (ForceLocal e0', ArrayT lvl ty0')
       _ -> error "Typechecking force: Expecting array as argument"
check env (Vec es _) =
  let (es', tys') = unzip $ map (check env) es
  in case tys' of
       [] -> error "Empty lists not supported before type inference is implemented"
       (ty:tys) -> if and (map (== ty) tys)
                   then (Vec es' (ArrayT Block ty), ArrayT Block ty)
                   else error "Differing element types in literal vector"
check env (Concat e0 e1) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
  in case (ty0, ty1) of
       (IntT, ArrayT lvlouter (ArrayT _ bty)) -> (Concat e0' e1', ArrayT lvlouter bty)
       _ -> error "Typechecking concat: Expecting array as argument"
check env (Assemble e0 e1 e2) =
  let (e0', ty0) = check env e0
      (e1', ty1) = check env e1
      (e2', ty2) = check env e2
  in case (ty0, ty1, ty2) of
       (IntT, (IntT :*: IntT) :> IntT, ArrayT lvlouter (ArrayT _ bty)) -> (Assemble e0' e1' e2',
                                                                           ArrayT lvlouter bty)
       (IntT, _, ArrayT _ (ArrayT _ _)) -> error ("Second argument to assemble not of type int*int -> int, got: " ++ show ty1)
       _ -> error "Typechecking assemble: Expecting array as argument"

checkBinOp :: BinOp -> Type -> Type -> Type
checkBinOp AddI IntT IntT = IntT
checkBinOp SubI IntT IntT = IntT
checkBinOp MulI IntT IntT = IntT
checkBinOp DivI IntT IntT = IntT
checkBinOp ModI IntT IntT = IntT
checkBinOp MinI IntT IntT = IntT
checkBinOp EqI  IntT IntT = BoolT
checkBinOp NeqI IntT IntT = BoolT
checkBinOp op   _    _    = error ("checkBinOp: Illegal arguments to built-in operator " ++ show op)

checkUnOp :: UnOp -> Type -> Type
checkUnOp AbsI IntT  = IntT
checkUnOp SignI IntT = IntT
checkUnOp op _       = error ("checkUnOp: Illegal arguments to built-in operator " ++ show op)
