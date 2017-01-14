module Language.FCL.Eval (eval) where

import qualified Data.Map as Map
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR, testBit)
import Data.List (sortBy)
import Data.Ord (comparing)

import Control.Monad.Trans.Except
import Control.Monad (liftM, liftM2)

import Language.FCL.SourceRegion
import Language.FCL.Identifier
import Language.FCL.Syntax
import Language.FCL.Eval.ArrayLib

----------------
-- Eval Monad --
----------------
data EvalError = EvalError (Maybe SourceRegion) String
  deriving Show

type Eval a = Except EvalError a

runEval :: Eval a -> Either EvalError a
runEval = runExcept

evalError :: (Maybe SourceRegion) -> String -> Eval a
evalError reg msg = throwE (EvalError reg msg)

----------------------------
-- Evaluation environment --
----------------------------
type VarEnv ty = Map.Map Identifier (Value ty)

data Env ty = Env { varEnv :: VarEnv ty }
  deriving Show

emptyEnv :: Env ty
emptyEnv = Env { varEnv = Map.empty }

lookupVar :: Identifier -> Env ty -> Maybe (Value ty)
lookupVar x env = Map.lookup x (varEnv env)

insertVar :: Identifier -> Value ty -> Env ty -> Env ty
insertVar x v env = env { varEnv = Map.insert x v (varEnv env) }

------------
-- Values --
------------
data Value ty = LamV (Env ty) Identifier (Exp ty)
              | IntV Int
              | DoubleV Double
              | PairV (Value ty) (Value ty)
              | BoolV Bool
              | StringV String
              | UnitV
              | ArrayV (FCLArray (Value ty))

instance Show (Value ty) where
  show (LamV _ _ _) = "<function>"
  show (IntV i) = show i
  show (DoubleV d) = show d
  show (PairV v1 v2) = concat ["(", show v1, ", ", show v2, ")"]
  show (BoolV b) = show b
  show (StringV str) = show str
  show UnitV = "unit"
  show (ArrayV arr) = show arr

------------------------------
-- Interpretation functions --
------------------------------
eval :: Show ty => [Definition ty] -> Either EvalError (Value ty)
eval prog = runEval (evalProgram emptyEnv prog)

evalProgram :: Show ty => Env ty -> [Definition ty] -> Eval (Value ty)
evalProgram _ [] = evalError Nothing "No main, exiting."
evalProgram env (def:defs) =
  if identToString (defVar def) == "main"
  then evalExp env (defBody def)
  else
    do v <- evalExp env (defBody def)
       let newEnv = insertVar (defVar def) v env
       evalProgram newEnv defs

-- Evaluation of expressions
evalExp :: Show ty => Env ty -> Exp ty -> Eval (Value ty)
evalExp _ (Literal (LiteralInt i) _) = return (IntV i)
evalExp _ (Literal (LiteralDouble d) _) = return (DoubleV d)
evalExp _ (Literal (LiteralBool b) _) = return (BoolV b)
evalExp _ (Literal (LiteralString str) _) = return (StringV str)
evalExp _ (Literal Unit _) = return UnitV
evalExp env (Var x _ reg) =
  case lookupVar x env of
    Just v -> return v
    Nothing -> evalError (Just reg) "using undefined variable"
evalExp env (Pair e1 e2 _) = do
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  return (PairV v1 v2)
evalExp env (Proj1E e reg) = do
  v <- evalExp env e
  case v of
    PairV v1 _ -> return v1
    _ -> evalError (Just reg) "fst should be applied to a pair"
evalExp env (Proj2E e reg) = do
  v <- evalExp env e
  case v of
    PairV _ v2 -> return v2
    _ -> evalError (Just reg) "snd should be applied to a pair"
evalExp env (Cond ec et ef _ reg) = do
  vcond <- evalExp env ec
  case vcond of
    BoolV True -> evalExp env et
    BoolV False -> evalExp env ef
    _ -> evalError (Just reg) "condition expression in if-statement evaluating to non-bool value"
evalExp env (Lamb x _ e _ _) = return (LamV env x e)
evalExp env (App e1 e2) = do
  v2 <- evalExp env e2
  v1 <- evalExp env e1
  case v1 of
    LamV env' x e -> evalExp (insertVar x v2 env') e
    _ -> evalError Nothing "Using non-function value as a function"
evalExp env (LambLvl _ e _ _) = evalExp env e -- ignore lvl argument, just evaluate body
evalExp env (AppLvl e _) = evalExp env e      -- ignore lvl argument, just return evaluated lambda function
evalExp env (Let x e ebody _ _) = do
  v <- evalExp env e
  evalExp (insertVar x v env) ebody
evalExp env (BinaryOp op e1 e2 r) = do
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  case op of
    -- Arithmetic
    AddI    -> IntV <$> liftM2 (+) (unInt r "addi" v1) (unInt r "addi" v2)
    SubI    -> IntV <$> liftM2 (-) (unInt r "first argument of subi" v1) (unInt r "second argument of subi" v2)
    MulI    -> IntV <$> liftM2 (*) (unInt r "muli" v1) (unInt r "muli" v2)
    DivI    -> IntV <$> liftM2 div (unInt r "divi" v1) (unInt r "divi" v2)
    ModI    -> IntV <$> liftM2 mod (unInt r "modi" v1) (unInt r "modi" v2)
    MinI    -> IntV <$> liftM2 min (unInt r "mini" v1) (unInt r "mini" v2)
    MaxI    -> IntV <$> liftM2 max (unInt r "maxi" v1) (unInt r "maxi" v2)
    PowI    -> IntV <$> liftM2 (^) (unInt r "powi" v1) (unInt r "powi" v2)

    AddR    -> DoubleV <$> liftM2 (+) (unDouble r "addr" v1) (unDouble r "addr" v2)
    
    AndI    -> IntV <$> liftM2 (.&.) (unInt r "andi" v1) (unInt r "andi" v2)
    OrI     -> IntV <$> liftM2 (.|.) (unInt r "ori" v1) (unInt r "ori" v2)
    XorI    -> IntV <$> liftM2 xor (unInt r "xori" v1) (unInt r "xori" v2)
    ShiftLI -> IntV <$> liftM2 shiftL (unInt r "shiftLi" v1) (unInt r "shiftLi" v2)
    ShiftRI -> IntV <$> liftM2 shiftR (unInt r "shiftRi" v1) (unInt r "shiftRi" v2)
    
    EqI     -> BoolV <$> liftM2 (==) (unInt r "eqi" v1) (unInt r "eqi" v2)
    NeqI    -> BoolV <$> liftM2 (/=) (unInt r "neqi" v1) (unInt r "neqi" v2)
    LtI     -> BoolV <$> liftM2 (<)  (unInt r "lti" v1) (unInt r "lti" v2)
    
    PowR    -> DoubleV <$> liftM2 (**) (unDouble r "powr" v1) (unDouble r "powr" v2)
    DivR    -> DoubleV <$> liftM2 (/) (unDouble r "divr" v1) (unDouble r "divr" v2)
evalExp env (MapPull ef e reg) = do
  vf <- evalExp env ef
  v <- evalExp env e
  case (vf, v) of
    (LamV env' var ebody, ArrayV arr) -> map_ env' var ebody arr
    (LamV _ _ _, _) -> evalError (Just reg) "Error when evaluating second argument to map: not an array"
    _               -> evalError (Just reg) "Error when evaluating first argument to map: not a function"
evalExp env (MapPush ef e reg) = do
  vf <- evalExp env ef
  v <- evalExp env e
  case (vf, v) of
    (LamV env' var ebody, ArrayV arr) -> map_ env' var ebody arr
    (LamV _ _ _, _) -> evalError (Just reg) "Error when evaluating second argument to map: not an array"
    _               -> evalError (Just reg) "Error when evaluating first argument to map: not a function"
evalExp env (Index e0 e1 reg) = do
  v0 <- evalExp env e0
  v1 <- evalExp env e1
  case (v0, v1) of
    (ArrayV arr, IntV i) -> return (index arr i)
    _ -> evalError (Just reg) "expecting array and integer as argument to Index"
evalExp env (GeneratePull e0 e1 reg) = do
  v0 <- evalExp env e0
  v1 <- evalExp env e1
  case (v0, v1) of
    (IntV n, LamV env' var ef) -> generate n env' var ef
    _ -> evalError (Just reg) "Generate expects integer expression as first argument and function as second argument"
evalExp env (LengthPull e0 reg) = do
  v0 <- evalExp env e0
  case v0 of
    (ArrayV arr) -> return (IntV (sizeOf arr))
    _ -> evalError (Just reg) "expecting array as argument to Length"
evalExp env (LengthPush e0 reg) = do
  v0 <- evalExp env e0
  case v0 of
    (ArrayV arr) -> return (IntV (sizeOf arr))
    _ -> evalError (Just reg) "expecting array as argument to Length"
evalExp env (Force e0 _) = evalExp env e0
evalExp env (ForceAndPrint e0 e1 reg) =
  do v0 <- evalExp env e0
     v1 <- evalExp env e1
     return v1
evalExp env (Push _ e0 _) = evalExp env e0
evalExp env (Vec ls _ _) = do
  vs <- mapM (evalExp env) ls
  return (ArrayV (fromList vs))
evalExp env (UnaryOp op e0 reg) = do
  v0 <- evalExp env e0
  case op of
    AbsI    -> liftM (IntV . abs)    (unInt reg "absi" v0)
    SignI   -> liftM (IntV . signum) (unInt reg "signi" v0)
    Not     -> liftM (BoolV . not)   (unBool reg "not" v0)
    NegateI -> liftM (IntV . negate) (unInt reg "negatei" v0)
    I2D     -> liftM (DoubleV . fromIntegral) (unInt reg "i2d" v0)
    B2I     -> liftM (IntV . fromBool) (unBool reg "b2i" v0)
    CLZ     -> liftM (IntV . countLeadingZeros) (unInt reg "clz" v0)
evalExp env (Interleave en ixf e0 reg) = interleave_ env en ixf e0 reg
evalExp env (For e0 e1 e2 reg) = do
  v0 <- evalExp env e0
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  let step = case v2 of
               LamV env1 var1 body1 ->
                 (\i x -> do v2' <- evalExp (insertVar var1 i env1) body1
                             case v2' of
                               LamV env2 var2 body2 -> evalExp (insertVar var2 x env2) body2
                               _ -> evalError (Just reg) "Second argument while evaluating 'power'")
               -- _ -> evalError (Just reg) "First argument while evaluating 'power'"
  for reg v0 v1 step
evalExp env (Power e0 e1 e2 reg) = do
  v0 <- evalExp env e0
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  let step = case v1 of
               LamV env1 var1 body1 ->
                 (\i x -> do v1' <- evalExp (insertVar var1 i env1) body1
                             case v1' of
                               LamV env2 var2 body2 -> evalExp (insertVar var2 x env2) body2
                               _ -> evalError (Just reg) "Second argument while evaluating 'power'")
               -- _ -> evalError (Just reg) "First argument while evaluating 'power'"
  power reg 0 v0 step v2    
evalExp env (While e0 e1 e2 reg) = do
  v0 <- evalExp env e0
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  case (v0, v1) of
    (LamV env0 var0 cond, LamV env1 var1 step) ->
      while
        reg
        (\x -> evalExp (insertVar var0 x env0) cond)
        (\x -> evalExp (insertVar var1 x env1) step)
        v2
    _ -> evalError (Just reg) "Argument while evaluating 'while'"
evalExp env (WhileSeq e0 e1 e2 reg) = do
  v0 <- evalExp env e0
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  case (v0, v1) of
    (LamV env0 var0 f, LamV env1 var1 body) ->
      while
        reg
        (\x -> evalExp (insertVar var0 x env0) f)
        (\x -> evalExp (insertVar var1 x env1) body)
        v2
    _ -> evalError (Just reg) "Argument while evaluating while"
evalExp _ (BlockSize reg) = evalError (Just reg) "blockSize not implemented"
evalExp env (Bind e1 e2 r) = do
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  case v2 of
    LamV env' x e -> evalExp (insertVar x v1 env') e
    _ -> evalError (Just r) "Bind: Using non-function value as a function"
evalExp env (Return _ e0 _) = evalExp env e0

---------------------
-- Various helpers --
---------------------
fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

countLeadingZeros :: Int -> Int
countLeadingZeros x = (w-1) - go (w-1)
  where
    go i | i < 0       = i -- no bit set
         | testBit x i = i
         | otherwise   = go (i-1)

    w = 32

generate :: Show ty => Int -> Env ty -> Identifier -> Exp ty -> Eval (Value ty)
generate n env' var ebody = do
  let arr = fromFunction n (\i -> evalExp (insertVar var (IntV i) env') ebody)
  arr' <- materializeM arr
  return (ArrayV arr')

map_ :: Show ty => Env ty -> Identifier -> Exp ty -> FCLArray (Value ty) -> Eval (Value ty)
map_ env' var ebody arr = do
  let arr' = mapA (\x -> evalExp (insertVar var x env') ebody) arr
  arr'' <- materializeM arr'
  return (ArrayV arr'')

interleave_ :: Show ty => Env ty -> Exp ty -> Exp ty -> Exp ty -> SourceRegion -> Eval (Value ty)
interleave_ env e0 e1 e2 reg =
 do n <- evalExp env e0
    ixf <- evalExp env e1
    arr <- evalExp env e2
    case (n, ixf, arr) of
      (IntV _, LamV env' var ebody, ArrayV arr') ->
        do let f i j =
                 case runEval (evalExp (insertVar var (PairV (IntV i) (IntV j)) env') ebody) of
                   Right (IntV v) -> v
                   Right _ -> error ""
                   Left _ -> error ""
           let vs = toList arr'
           vs' <- mapM (unArray reg "interleave") vs
            
           return (ArrayV (fromList (interleave f (map toList vs'))))
      _ -> evalError (Just reg) "interleave eval err"

while :: SourceRegion
      -> (Value ty -> Eval (Value ty))
      -> (Value ty -> Eval (Value ty))
      -> Value ty
      -> Eval (Value ty)
while reg cond step x = do
  c <- cond x
  case c of -- stop condition
    BoolV True ->
      do x' <- step x
         while reg cond step x'
    BoolV False -> return x
    _ -> evalError (Just reg) "First argument to while should return Bool"

for :: SourceRegion
    -> Value ty
    -> Value ty
    -> (Value ty -> Value ty -> Eval (Value ty))
    -> Eval (Value ty)
for reg (IntV size) (IntV iterations) step = undefined
for reg _ _ _ = evalError (Just reg) "First argument to power should be an integer"


power :: SourceRegion
      -> Int
      -> Value ty
      -> (Value ty -> Value ty -> Eval (Value ty))
      -> Value ty
      -> Eval (Value ty)
power _ _ (IntV 0) _ x = return x
power reg i (IntV n) step x = 
  do x' <- step (IntV i) x
     power reg (i+1) (IntV (n-1)) step x'
power reg _ _ _ _ = evalError (Just reg) "First argument to power should be an integer"


unInt ::  SourceRegion -> String -> Value ty -> Eval Int
unInt reg str v =
  case v of
    IntV v' -> return v'
    u -> evalError (Just reg) ("expecting int in " ++ str ++ " got " ++ show u)

unDouble :: SourceRegion -> String -> Value ty -> Eval Double
unDouble reg str v =
  case v of
    DoubleV v' -> return v'
    _ -> evalError (Just reg) ("expecting int in " ++ str)

unBool :: SourceRegion -> String -> Value ty -> Eval Bool
unBool reg str v =
  case v of
    BoolV v' -> return v'
    _ -> evalError (Just reg) ("expecting int in " ++ str)

unArray :: SourceRegion -> String -> Value ty -> Eval (FCLArray (Value ty))
unArray reg str v =
  case v of
    ArrayV v' -> return v'
    _ -> evalError (Just reg) ("Expecting double in " ++ str)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f ls = sortBy (comparing f) ls

interleave :: (Int -> Int -> Int) -> [[a]] -> [a]
interleave f array =
  let buildAssocList _ _ [] = []
      buildAssocList i _ ([]:xs) = buildAssocList (i+1) 0 xs
      buildAssocList i j ((y:ys):xs) =
        (f i j, y) : buildAssocList i (j+1) (ys:xs)
  in Prelude.map Prelude.snd (sortOn Prelude.fst (buildAssocList 0 0 array))
