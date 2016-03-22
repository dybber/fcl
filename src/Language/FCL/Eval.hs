module Language.FCL.Eval (eval) where

import qualified Data.Map as Map
import Data.Bits ((.&.), xor, shiftL, shiftR)

import Language.FCL.SourceRegion
import Language.FCL.Syntax
import Language.FCL.Eval.ArrayLib
import qualified Language.FCL.Eval.ArrayLib as Arr

-- import Data.List (sortBy)
-- import Data.Ord (comparing)

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
   deriving Eq

instance Show (Value ty) where
  show (LamV _ _ _) = "<fun>"
  show (IntV i) = show i
  show (DoubleV d) = show d
  show (PairV v1 v2) = concat ["(", show v1, ", ", show v2, ")"]
  show (BoolV b) = show b
  show (ArrayV arr) = show arr

eval :: Show ty => Program ty -> Either String (Value ty)
eval = evalProgram emptyEnv

evalProgram :: Show ty => Env ty -> Program ty -> Either String (Value ty)
evalProgram _ [] = Left "No main, exiting"
evalProgram env (def:defs) =
  if defVar def == "main"
  then Right (interp env (defBody def))
  else
    let newEnv = insertVar (defVar def) (interp env (defBody def)) env
    in evalProgram newEnv defs

-- Evaluation of expressions
interp :: Show ty => Env ty -> Exp ty -> Value ty
interp _ (IntScalar i _) = IntV i
interp _ (DoubleScalar d _) = DoubleV d
interp _ (BoolScalar b _) = BoolV b
interp env (Var x _ reg) =
  case lookupVar x env of
    Just v -> v
    Nothing -> error (show reg ++ ": using undefined variable")
interp env (Pair e1 e2 _) = PairV (interp env e1) (interp env e2)
interp env (Proj1E e reg) =
  case interp env e of
    PairV v1 _ -> v1
    _ -> error (show reg ++ ": fst should be applied to a pair")
interp env (Proj2E e reg) =
  case interp env e of
    PairV _ v2 -> v2
    _ -> error (show reg ++ ": snd should be applied to a pair")
interp env (Cond ec et ef _ reg) =
  case interp env ec of
    BoolV True -> interp env et
    BoolV False -> interp env ef
    _ -> error (show reg ++ ": condition expression in if-statement evaluating to non-bool value")
interp env (Lamb x _ e _ _) = LamV env x e
interp env (App e1 e2) =
  let v = interp env e2
  in case interp env e1 of
      LamV env' x e -> interp (insertVar x v env') e
      _ -> error "Using non-function value as a function"
interp env (Let x e ebody _ _) =
  let v = interp env e
  in interp (insertVar x v env) ebody
interp env (BinOp op e1 e2 r) =
  let v1 = interp env e1
      v2 = interp env e2
  in case op of
       -- Arithmetic
       AddI -> IntV $ (unInt "addi" v1 r) + (unInt "addi" v2 r)
       SubI -> IntV $ (unInt "subi" v1 r) - (unInt "subi" v2 r)
       MulI -> IntV $ (unInt "muli" v1 r) * (unInt "muli" v2 r)
       DivI -> IntV $ (unInt "divi" v1 r) `div` (unInt "divi" v2 r)
       ModI -> IntV $ (unInt "modi" v1 r) `mod` (unInt "modi" v2 r)
       MinI -> IntV $ (unInt "mini" v1 r) `min` (unInt "mini" v2 r)
       EqI  -> BoolV $ (unInt "eqi" v1 r) == (unInt "eqi" v2 r)
       NeqI  -> BoolV $ (unInt "eqi" v1 r) /= (unInt "eqi" v2 r)
       AndI -> IntV $ (unInt "andi" v1 r) .&. (unInt "andi" v2 r)
       XorI -> IntV $ (unInt "xori" v1 r) `xor` (unInt "xori" v2 r)
       ShiftLI -> IntV $ (unInt "shiftLi" v1 r) `shiftL` (unInt "shiftLi" v2 r)
       ShiftRI -> IntV $ (unInt "shiftRi" v1 r) `shiftR` (unInt "shiftRi" v2 r)
       PowI    -> IntV $ (unInt "powi" v1 r) ^ (unInt "powi" v2 r)
       PowR    -> DoubleV $ (unDouble "powr" v1 r) ** (unDouble "powr" v2 r)
       DivR    -> DoubleV $ (unDouble "divr" v1 r) / (unDouble "divr" v2 r)
interp env (Map ef e reg) =
  case (interp env ef, interp env e) of
    (LamV env' x ebody, ArrayV arr) ->
      let f v = interp (insertVar x v env') ebody
      in ArrayV (Arr.mapA f arr)
    (LamV _ _ _, _) -> error "Error when evaluating second argument to map: not an array"
    _               -> error "Error when evaluating first argument to map: not a function"
interp env (Index e0 e1 reg) =
  case (interp env e0, interp env e1) of
    (ArrayV arr, IntV i) -> index arr i
    _ -> error "Error - expecting array and integer as argument to Index"
interp env (Generate _ e0 e1 reg) =
  case (interp env e0, interp env e1) of
    (IntV n, LamV env' var ef) -> ArrayV (fromFunction n (\x -> interp (insertVar var (IntV x) env') ef))
    _ -> error "Generate expects integer expression as first argument and function as second argument"
interp env (Length e0 reg) =
  case (interp env e0) of
    (ArrayV arr) -> IntV (sizeOf arr)
    _ -> error "Error - expecting array as argument to Length"
interp env (ForceLocal e0 reg) = interp env e0
interp env (Vec ls _ reg) =
  let vs = map (interp env) ls
  in ArrayV (fromList vs)
interp env (UnOp op e0 reg) =
  let v0 = interp env e0
  in case op of
       AbsI  -> IntV (abs (unInt "absi" v0 reg))
       SignI -> IntV (signum (unInt "signi" v0 reg))
       Not -> BoolV (not (unBool "not" v0 reg))
       NegateI -> IntV (negate (unInt "negatei" v0 reg))
       I2D -> DoubleV (fromIntegral (unInt "i2d" v0 reg))
interp env (Concat n e0 reg) =
  case (interp env n, interp env e0) of
    (IntV _, ArrayV arr) ->
      let vs = toList arr
      in ArrayV (fromList (concat (map (toList . unArray "assemble") vs)))
    s -> error (show reg ++ ": " ++ show s ++ " not implemented")
interp env (While e0 e1 e2 reg) =
  case (interp env e0, interp env e1) of
    (LamV env0 var0 f, LamV env1 var1 body) ->
      while (\x -> interp (insertVar var0 x env0) f)
          (\x -> interp (insertVar var1 x env1) body)
          (interp env e2)
    _ -> error (show reg)
interp env (Scanl ef e bs reg) =
  case (interp env ef, interp env bs) of
    (LamV env' x ebody, ArrayV arr) ->
      let f (v1,v2) = interp (insertVar x (PairV v1 v2) env') ebody
      in ArrayV (Arr.scanlA f (interp env e) arr)
    (LamV _ _ _, _) -> error (show reg ++ ": third argument to map not an array")
    _               -> error (show reg ++ ": first argument to map: not a function")
interp _ (LocalSize reg) = error (show reg ++ ": localSize not implemented")


-- sortOn :: Ord b => (a -> b) -> [a] -> [a]
-- sortOn f ls = sortBy (comparing f) ls

-- assemble :: (Int -> Int -> Int) -> [[a]] -> [a]
-- assemble f array =
--   let buildAssocList _ _ [] = []
--       buildAssocList i _ ([]:xs) = buildAssocList (i+1) 0 xs
--       buildAssocList i j ((y:ys):xs) =
--         (f i j, y) : buildAssocList i (j+1) (ys:xs)
--   in Prelude.map Prelude.snd (sortOn Prelude.fst (buildAssocList 0 0 array))

while :: (Value ty -> Value ty) -> (Value ty -> Value ty) -> Value ty -> Value ty
while f body x =
  case f x of
    BoolV True -> while f body (body x)
    BoolV False -> x
    _ -> error "Second argument to while should return Bool"

unDouble :: String -> Value ty -> Region -> Double
unDouble str v reg =
  case v of
    DoubleV v' -> v'
    _ -> error (show reg ++ ": expecting double in " ++ str)

unBool :: String -> Value ty -> Region -> Bool
unBool str v reg =
  case v of
    BoolV v' -> v'
    _ -> error (show reg ++ ": expecting bool in " ++ str)

unInt :: String -> Value ty -> Region -> Int
unInt str v reg =
  case v of
    IntV v' -> v'
    _ -> error (show reg ++ ": expecting int in " ++ str)

unArray :: String -> Value ty -> FCLArray (Value ty)
unArray str v =
  case v of
    ArrayV v' -> v'
    _ -> error ("Expecting double in " ++ str)
