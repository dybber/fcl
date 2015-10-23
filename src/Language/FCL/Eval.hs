-- TODO:
--   * proper error-handling
--   * implement reorderStride
module Language.FCL.Eval where

import Language.FCL.Syntax
import qualified Data.Map as Map
import Language.FCL.Eval.ArrayLib
import qualified Language.FCL.Eval.ArrayLib as Arr

-- Evaluation environment
type VarEnv ty = Map.Map VarName (Value ty)

data Env ty = Env { varEnv :: VarEnv ty }
  deriving (Eq, Show)

emptyEnv :: Env ty
emptyEnv = Env { varEnv = Map.empty-- ,
                 -- sizeEnv = Map.empty
               }

lookupVar :: VarName -> Env ty -> Maybe (Value ty)
lookupVar x env = Map.lookup x (varEnv env)

insertVar :: VarName -> Value ty -> Env ty -> Env ty
insertVar x v env = env { varEnv = Map.insert x v (varEnv env) }

-- Values
data Value ty = LamV (Env ty) VarName (Exp ty)
--              | LamS (Env ty) SizeVar (Exp ty)
              | IntV Int
              | DoubleV Double
              | BoolV Bool
              | PairV (Value ty) (Value ty)
--              | UnitV
              | ArrayV (FCLArray (Value ty))
   deriving (Eq, Show)

-- Evaluation of expressions
eval :: Env ty -> Exp ty -> Value ty
eval _ (IntE i) = IntV i
eval _ (DoubleE d) = DoubleV d
eval _ (BoolE b) = BoolV b
eval env (VarE x _) =
  case lookupVar x env of
    Just v -> v
    Nothing -> error "using undefined variable"
--eval _ UnitE = UnitV
eval env (PairE e1 e2) = PairV (eval env e1) (eval env e2)
eval env (Proj1E e) =
  case eval env e of
    PairV v1 _ -> v1
    _ -> error "Proj1E should be applied to a pair"
eval env (Proj2E e) =
  case eval env e of
    PairV _ v2 -> v2
    _ -> error "Proj2E should be applied to a pair"
eval env (IfE ec et ef _) =
  case eval env ec of
    BoolV True -> eval env et
    BoolV False -> eval env ef
    _ -> error "condition expression in if-statement evaluating to non-bool value"
eval env (LamE x _ e _) = LamV env x e
eval env (AppE e1 e2 _) =
  let v = eval env e2
  in case eval env e1 of
      LamV env' x e -> eval (insertVar x v env') e
      _ -> error "Using non-function value as a function"
eval env (LetE x _ e ebody _) =
  let v = eval env e
  in eval (insertVar x v env) ebody
eval env (VectorE es _) = ArrayV $ Arr.fromList $ map (eval env) es
eval env (UnaryOpE op e) =
  let v = eval env e
  in case op of
       Not          -> BoolV   (not          (unBool "not" v))
       NegateInt    -> IntV    (negate       (unInt "negatei" v))
       NegateDouble -> DoubleV (negate       (unDouble "negated" v))
       Exp          -> DoubleV (exp          (unDouble "exp" v))
       Ln           -> DoubleV (log          (unDouble "log" v))
       AbsI         -> IntV    (abs          (unInt "absi" v))
       AbsD         -> DoubleV (abs          (unDouble "absd" v))
       Ceil         -> IntV    (ceiling      (unDouble "ceil" v))
       Floor        -> IntV    (floor        (unDouble "floor" v))
       I2D          -> DoubleV (fromIntegral (unInt "i2d" v))
eval env (BinOpE op e1 e2) =
  let v1 = eval env e1
      v2 = eval env e2
  in case op of
       -- Arithmetic
       AddI -> IntV $ (unInt "addi" v1) + (unInt "addi" v2)
       SubI -> IntV $ (unInt "subi" v1) - (unInt "subi" v2)
       MulI -> IntV $ (unInt "muli" v1) * (unInt "muli" v2)
       DivI -> IntV $ (unInt "divi" v1) `div` (unInt "divi" v2)
       AddD -> DoubleV $ (unDouble "addd" v1) + (unDouble "addd" v2)
       SubD -> DoubleV $ (unDouble "subd" v1) - (unDouble "subd" v2)
       MulD -> DoubleV $ (unDouble "muld" v1) * (unDouble "muld" v2)
       DivD -> DoubleV $ (unDouble "divd" v1) / (unDouble "divd" v2)

       -- Comparisons
       LtI  -> BoolV $ (unDouble "lti" v1) < (unDouble "lti" v2)
       LteI -> BoolV $ (unDouble "ltei" v1) <= (unDouble "ltei" v2)
       GtI  -> BoolV $ (unDouble "gti" v1) < (unDouble "gti" v2)
       GteI -> BoolV $ (unDouble "gtei" v1) >= (unDouble "gtei" v2)
       EqI  -> BoolV $ (unDouble "eqi" v1) == (unDouble "eqi" v2)
       NeqI -> BoolV $ (unDouble "neqi" v1) /= (unDouble "neqi" v2)
       
       LtD  -> BoolV $ (unDouble "ltd" v1) < (unDouble "ltd" v2)
       LteD -> BoolV $ (unDouble "lted" v1) <= (unDouble "lted" v2)
       GtD  -> BoolV $ (unDouble "gtd" v1) < (unDouble "gtd" v2)
       GteD -> BoolV $ (unDouble "gted" v1) >= (unDouble "gted" v2)
       EqD  -> BoolV $ (unDouble "eqd" v1) == (unDouble "eqd" v2)
       NeqD -> BoolV $ (unDouble "neqd" v1) /= (unDouble "neqd" v2)
       
       -- Boolean
       And -> BoolV $ (unBool "and" v1) && (unBool "and" v2)
       Or  -> BoolV $ (unBool "or" v1) || (unBool "or" v2)
eval env (MapE _ ef e _) =
  case (eval env ef, eval env e) of
    (LamV env' x ebody, ArrayV arr) ->
      let f v = eval (insertVar x v env') ebody
      in ArrayV (Arr.mapA f arr)
    (LamV _ _ _, _) -> error "Error when evaluating second argument to map: not an array"
    _                 -> error "Error when evaluating first argument to map: not a function"
eval env (ReduceSeqE ef e1 e2 _) =
  case (eval env ef, eval env e1, eval env e2) of
    (LamV env' x ebody, v, ArrayV arr) ->
      let f (v1,v2) = eval (insertVar x (PairV v1 v2) env') ebody
      in Arr.foldlA f v arr
    (LamV _ _ _, _, _) -> error "Error when evaluating second argument to reduceSeq: not an array"
    _                  -> error "Error when evaluating first argument to reduceSeq: not a function"
eval env (ToGlobalE e) = eval env e
eval env (ToLocalE e) = eval env e
eval env (ReorderE e) = eval env e
eval env (JoinE e _) =
  case eval env e of
    ArrayV arr -> let arr' = Arr.mapA (\x -> case x of
                                              ArrayV v -> v
                                              _ -> error "Expecting array of arrays as argument to join") arr
                  in ArrayV (Arr.joinA arr')
    _ -> error "Expecting array as argument to join"
eval env (SplitE s e _) =
  case eval env e of
    ArrayV arr -> ArrayV (Arr.mapA ArrayV (Arr.splitA (unInt "first argument to split" $ eval env s) arr))
    _ -> error "Expecting array as argument to split"
eval _ (ReorderStrideE _ _) = error "Error: reorderStride not yet supported by the interpreter"

-- We could have done something smart with multi-parameter typeclasses
-- providing lifting functions, but this works for now.
--
-- We would still have had to annotate most of the operators
-- to distinguish which instance to use:
--   liftUnary (negate :: Int -> Int)
--   liftUnary (negate :: Double -> Double)
--
-- This is just going to be a dumb interpreter anyways!

unDouble :: String -> Value t -> Double
unDouble str v = case v of DoubleV v' -> v'
                           _ -> error ("Expecting double in " ++ str)

unBool :: String -> Value t -> Bool
unBool str v = case v of BoolV v' -> v'
                         _ -> error ("Expecting bool in " ++ str)

unInt :: String -> Value t -> Int
unInt str v = case v of IntV v' -> v'
                        _ -> error ("Expecting int in " ++ str)



