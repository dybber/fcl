module Language.ObsidianLight.Eval where

import qualified Data.Map as Map
import Language.ObsidianLight.Syntax
import Language.FCL.Eval.ArrayLib
import qualified Language.FCL.Eval.ArrayLib as Arr

-- Evaluation environment
type VarEnv ty = Map.Map Variable (Value ty)

data Env ty = Env { varEnv :: VarEnv ty }
  deriving (Eq, Show)

emptyEnv :: Env ty
emptyEnv = Env { varEnv = Map.empty }

lookupVar :: Variable -> Env ty -> Maybe (Value ty)
lookupVar x env = Map.lookup x (varEnv env)

insertVar :: Variable -> Value ty -> Env ty -> Env ty
insertVar x v env = env { varEnv = Map.insert x v (varEnv env) }

-- Values
data Value ty = LamV (Env ty) Variable (Exp ty)
              | IntV Int
              | DoubleV Double
              | PairV (Value ty) (Value ty)
              | BoolV Bool
              | ArrayV (FCLArray (Value ty))
   deriving (Eq, Show)

-- Evaluation of expressions
eval :: Env ty -> Exp ty -> Value ty
eval _ (IntScalar i) = IntV i
eval _ (DoubleScalar d) = DoubleV d
eval _ (BoolScalar b) = BoolV b
eval env (Var x _) =
  case lookupVar x env of
    Just v -> v
    Nothing -> error "using undefined variable"
eval env (Pair e1 e2) = PairV (eval env e1) (eval env e2)
eval env (Proj1E e) =
  case eval env e of
    PairV v1 _ -> v1
    _ -> error "Proj1E should be applied to a pair"
eval env (Proj2E e) =
  case eval env e of
    PairV _ v2 -> v2
    _ -> error "Proj2E should be applied to a pair"
eval env (Cond ec et ef _) =
  case eval env ec of
    BoolV True -> eval env et
    BoolV False -> eval env ef
    _ -> error "condition expression in if-statement evaluating to non-bool value"
eval env (Lamb x _ e _) = LamV env x e
eval env (App e1 e2) =
  let v = eval env e2
  in case eval env e1 of
      LamV env' x e -> eval (insertVar x v env') e
      _ -> error "Using non-function value as a function"
eval env (Let x e ebody _) =
  let v = eval env e
  in eval (insertVar x v env) ebody
eval env (BinOp op e1 e2) =
  let v1 = eval env e1
      v2 = eval env e2
  in case op of
       -- Arithmetic
       AddI -> IntV $ (unInt "addi" v1) + (unInt "addi" v2)
       SubI -> IntV $ (unInt "subi" v1) - (unInt "subi" v2)
       MulI -> IntV $ (unInt "muli" v1) * (unInt "muli" v2)
       DivI -> IntV $ (unInt "divi" v1) `div` (unInt "divi" v2)
       ModI -> IntV $ (unInt "modi" v1) `mod` (unInt "modi" v2)
       MinI -> IntV $ (unInt "mini" v1) `min` (unInt "mini" v2)
       EqI  -> BoolV $ (unDouble "eqi" v1) == (unDouble "eqi" v2)
eval env (Map ef e) =
  case (eval env ef, eval env e) of
    (LamV env' x ebody, ArrayV arr) ->
      let f v = eval (insertVar x v env') ebody
      in ArrayV (Arr.mapA f arr)
    (LamV _ _ _, _) -> error "Error when evaluating second argument to map: not an array"
    _                 -> error "Error when evaluating first argument to map: not a function"
eval env (Index e0 e1) =
  case (eval env e0, eval env e1) of
    (ArrayV arr, IntV i) -> index arr i
    _ -> error "Error - expecting array and integer as argument to Index"
eval env (Generate _ e0 e1) =
  case (eval env e0, eval env e1) of
    (IntV n, LamV env' var ef) -> ArrayV (fromFunction n (\x -> eval (insertVar var (IntV x) env') ef))
    _ -> error "Generate expects integer expression as first argument and function as second argument"
eval env (Length e0) =
  case (eval env e0) of
    (ArrayV arr) -> IntV (sizeOf arr)
    _ -> error "Error - expecting array as argument to Length"
eval env (ForceLocal e0) = eval env e0
eval env (Fixpoint e0 e1 e2) =
  case (eval env e0, eval env e1) of
    (LamV env0 var0 f, LamV env1 var1 body) ->
      fix (\x -> eval (insertVar var0 x env0) f)
          (\x -> eval (insertVar var1 x env1) body)
          (eval env e2)
    _ -> error ""

fix :: (Value ty -> Value ty) -> (Value ty -> Value ty) -> Value ty -> Value ty
fix f body x =
  case f x of
    BoolV True -> fix f body (body x)
    BoolV False -> x
    _ -> error "Second argument to fixpoint should return Bool"

unDouble :: String -> Value ty -> Double
unDouble str v =
  case v of
    DoubleV v' -> v'
    _ -> error ("Expecting double in " ++ str)

unBool :: String -> Value ty -> Bool
unBool str v =
  case v of
    BoolV v' -> v'
    _ -> error ("Expecting bool in " ++ str)

unInt :: String -> Value ty -> Int
unInt str v =
  case v of
    IntV v' -> v'
    _ -> error ("Expecting int in " ++ str)

