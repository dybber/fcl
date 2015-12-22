module Language.FCL.Eval where

import qualified Data.Map as Map
import Data.Bits ((.&.), xor, shiftL, shiftR)

import Language.FCL.Syntax
import Language.FCL.Eval.ArrayLib
import qualified Language.FCL.Eval.ArrayLib as Arr

import Data.List (sortBy)
import Data.Ord (comparing)

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
interp :: Show ty => Env ty -> Exp ty -> Value ty
interp _ (IntScalar i) = IntV i
interp _ (DoubleScalar d) = DoubleV d
interp _ (BoolScalar b) = BoolV b
interp env (Var x _) =
  case lookupVar x env of
    Just v -> v
    Nothing -> error "using undefined variable"
interp env (Pair e1 e2) = PairV (interp env e1) (interp env e2)
interp env (Proj1E e) =
  case interp env e of
    PairV v1 _ -> v1
    _ -> error "Proj1E should be applied to a pair"
interp env (Proj2E e) =
  case interp env e of
    PairV _ v2 -> v2
    _ -> error "Proj2E should be applied to a pair"
interp env (Cond ec et ef _) =
  case interp env ec of
    BoolV True -> interp env et
    BoolV False -> interp env ef
    _ -> error "condition expression in if-statement evaluating to non-bool value"
interp env (Lamb x _ e _) = LamV env x e
interp env (App e1 e2) =
  let v = interp env e2
  in case interp env e1 of
      LamV env' x e -> interp (insertVar x v env') e
      _ -> error "Using non-function value as a function"
interp env (Let x e ebody _) =
  let v = interp env e
  in interp (insertVar x v env) ebody
interp env (BinOp op e1 e2) =
  let v1 = interp env e1
      v2 = interp env e2
  in case op of
       -- Arithmetic
       AddI -> IntV $ (unInt "addi" v1) + (unInt "addi" v2)
       SubI -> IntV $ (unInt "subi" v1) - (unInt "subi" v2)
       MulI -> IntV $ (unInt "muli" v1) * (unInt "muli" v2)
       DivI -> IntV $ (unInt "divi" v1) `div` (unInt "divi" v2)
       ModI -> IntV $ (unInt "modi" v1) `mod` (unInt "modi" v2)
       MinI -> IntV $ (unInt "mini" v1) `min` (unInt "mini" v2)
       EqI  -> BoolV $ (unInt "eqi" v1) == (unInt "eqi" v2)
       NeqI  -> BoolV $ (unInt "eqi" v1) /= (unInt "eqi" v2)
       AndI -> IntV $ (unInt "andi" v1) .&. (unInt "andi" v2)
       XorI -> IntV $ (unInt "xori" v1) `xor` (unInt "xori" v2)
       ShiftLI -> IntV $ (unInt "shiftLi" v1) `shiftL` (unInt "shiftLi" v2)
       ShiftRI -> IntV $ (unInt "shiftRi" v1) `shiftR` (unInt "shiftRi" v2)
interp env (Map ef e) =
  case (interp env ef, interp env e) of
    (LamV env' x ebody, ArrayV arr) ->
      let f v = interp (insertVar x v env') ebody
      in ArrayV (Arr.mapA f arr)
    (LamV _ _ _, _) -> error "Error when evaluating second argument to map: not an array"
    _               -> error "Error when evaluating first argument to map: not a function"
interp env (Index e0 e1) =
  case (interp env e0, interp env e1) of
    (ArrayV arr, IntV i) -> index arr i
    _ -> error "Error - expecting array and integer as argument to Index"
interp env (Generate _ e0 e1) =
  case (interp env e0, interp env e1) of
    (IntV n, LamV env' var ef) -> ArrayV (fromFunction n (\x -> interp (insertVar var (IntV x) env') ef))
    _ -> error "Generate expects integer expression as first argument and function as second argument"
interp env (Length e0) =
  case (interp env e0) of
    (ArrayV arr) -> IntV (sizeOf arr)
    _ -> error "Error - expecting array as argument to Length"
interp env (ForceLocal e0) = interp env e0
interp env (Vec ls _) =
  let vs = map (interp env) ls
  in ArrayV (fromList vs)
interp env (UnOp op e0) =
  let v0 = interp env e0
  in case op of
       AbsI  -> IntV (abs (unInt "absi" v0))
       SignI -> IntV (signum (unInt "signi" v0))
interp env (Concat _ e0) =
  case interp env e0 of
    ArrayV arr ->
      let vs = toList arr
      in ArrayV (fromList (concatMap (toList . unArray "concat") vs))
    s -> error (show s ++ " not implemented")
interp env (Assemble n f e0) =
  let fx x y =
        case interp env (App f (Pair (IntScalar x) (IntScalar y))) of
          IntV z -> z
          _ -> error "Function given to assemble should return an integer"
  in case (interp env n, interp env e0) of
       (IntV _, ArrayV arr) ->
         let vs = toList arr
         in ArrayV (fromList (assemble fx (map (toList . unArray "asssemble") vs)))
       s -> error (show s ++ " not implemented")
interp env (Fixpoint e0 e1 e2) =
  case (interp env e0, interp env e1) of
    (LamV env0 var0 f, LamV env1 var1 body) ->
      fix (\x -> interp (insertVar var0 x env0) f)
          (\x -> interp (insertVar var1 x env1) body)
          (interp env e2)
    _ -> error ""


sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f ls = sortBy (comparing f) ls

assemble :: (Int -> Int -> Int) -> [[a]] -> [a]
assemble f array =
  let buildAssocList _ _ [] = []
      buildAssocList i _ ([]:xs) = buildAssocList (i+1) 0 xs
      buildAssocList i j ((y:ys):xs) =
        (f i j, y) : buildAssocList i (j+1) (ys:xs)
  in Prelude.map Prelude.snd (sortOn Prelude.fst (buildAssocList 0 0 array))

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

unArray :: String -> Value ty -> FCLArray (Value ty)
unArray str v =
  case v of
    ArrayV v' -> v'
    _ -> error ("Expecting double in " ++ str)
