module Language.FCL.Eval (eval) where

import qualified Data.Map as Map
import Data.Bits ((.&.), xor, shiftL, shiftR)

import Language.FCL.SourceRegion
import Language.FCL.Syntax
import Language.FCL.Eval.ArrayLib

import Control.Monad.Trans.Except
import Control.Monad (liftM, liftM2)
import Control.Applicative ((<$>))

----------------
-- Eval Monad --
----------------
data EvalError = EvalError (Maybe Region) String
  deriving Show

type Eval a = Except EvalError a

runEval :: Eval a -> Either EvalError a
runEval = runExcept

evalError :: (Maybe Region) -> String -> Eval a
evalError reg msg = throwE (EvalError reg msg)

----------------------------
-- Evaluation environment --
----------------------------
type VarEnv ty = Map.Map Name (Value ty)

data Env ty = Env { varEnv :: VarEnv ty }
  deriving (Eq, Show)

emptyEnv :: Env ty
emptyEnv = Env { varEnv = Map.empty }

lookupVar :: Name -> Env ty -> Maybe (Value ty)
lookupVar x env = Map.lookup x (varEnv env)

insertVar :: Name -> Value ty -> Env ty -> Env ty
insertVar x v env = env { varEnv = Map.insert x v (varEnv env) }

------------
-- Values --
------------
data Value ty = LamV (Env ty) Name (Exp ty)
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

------------------------------
-- Interpretation functions --
------------------------------
eval :: Show ty => Program ty -> Either EvalError (Value ty)
eval prog = runEval (evalProgram emptyEnv prog)

evalProgram :: Show ty => Env ty -> Program ty -> Eval (Value ty)
evalProgram _ [] = evalError Nothing "No main, exiting."
evalProgram env (def:defs) =
  if defVar def == "main"
  then evalExp env (defBody def)
  else
    do v <- evalExp env (defBody def)
       let newEnv = insertVar (defVar def) v env
       evalProgram newEnv defs

-- Evaluation of expressions
evalExp :: Show ty => Env ty -> Exp ty -> Eval (Value ty)
evalExp _ (IntScalar i _) = return (IntV i)
evalExp _ (DoubleScalar d _) = return (DoubleV d)
evalExp _ (BoolScalar b _) = return (BoolV b)
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
evalExp env (Let x e ebody _ _) = do
  v <- evalExp env e
  evalExp (insertVar x v env) ebody
evalExp env (BinOp op e1 e2 r) = do
  v1 <- evalExp env e1
  v2 <- evalExp env e2
  case op of
    -- Arithmetic
    AddI    -> IntV <$> liftM2 (+) (unInt r "addi" v1) (unInt r "addi" v2)
    SubI    -> IntV <$> liftM2 (+) (unInt r "subi" v1) (unInt r "subi" v2)
    MulI    -> IntV <$> liftM2 (*) (unInt r "muli" v1) (unInt r "muli" v2)
    DivI    -> IntV <$> liftM2 div (unInt r "divi" v1) (unInt r "divi" v2)
    ModI    -> IntV <$> liftM2 mod (unInt r "modi" v1) (unInt r "modi" v2)
    MinI    -> IntV <$> liftM2 min (unInt r "mini" v1) (unInt r "mini" v2)
    PowI    -> IntV <$> liftM2 (^) (unInt r "powi" v1) (unInt r "powi" v2)
    AndI    -> IntV <$> liftM2 (.&.) (unInt r "andi" v1) (unInt r "andi" v2)
    XorI    -> IntV <$> liftM2 xor (unInt r "xori" v1) (unInt r "xori" v2)
    ShiftLI -> IntV <$> liftM2 shiftL (unInt r "shiftLi" v1) (unInt r "shiftLi" v2)
    ShiftRI -> IntV <$> liftM2 shiftR (unInt r "shiftRi" v1) (unInt r "shiftRi" v2)
    
    EqI     -> BoolV <$> liftM2 (==) (unInt r "eqi" v1) (unInt r "eqi" v2)
    NeqI    -> BoolV <$> liftM2 (/=) (unInt r "neqi" v1) (unInt r "neqi" v2)
    
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
evalExp env (Push e0 _ _) = evalExp env e0
evalExp env (Vec ls _ _) = do
  vs <- mapM (evalExp env) ls
  return (ArrayV (fromList vs))
evalExp env (UnOp op e0 reg) = do
  v0 <- evalExp env e0
  case op of
    AbsI    -> liftM (IntV . abs)    (unInt reg "absi" v0)
    SignI   -> liftM (IntV . signum) (unInt reg "signi" v0)
    Not     -> liftM (BoolV . not)   (unBool reg "not" v0)
    NegateI -> liftM (IntV . negate) (unInt reg "negatei" v0)
    I2D     -> liftM (DoubleV . fromIntegral) (unInt reg "i2d" v0)
evalExp env (Concat en e0 reg) = do
  n <- evalExp env en
  v0 <- evalExp env e0
  case (n, v0) of
    (IntV _, ArrayV arr) ->
      do let vs = toList arr
         vs' <- mapM (unArray reg "concat") vs
         return (ArrayV (fromList (concat (map toList vs'))))
    (_,_) -> evalError (Just reg) ("concat expects integer and array as arguments.")
evalExp env (While e0 e1 e2 reg) = do
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
evalExp env (Scanl ef e es reg) = do
  vf <- evalExp env ef
  v <- evalExp env e
  vs <- evalExp env es
  case (vf, vs) of
    (LamV env' x ebody, ArrayV arr) -> do scanl_ env' x ebody v arr
    (LamV _ _ _, _) -> evalError (Just reg) "third argument to map not an array"
    _               -> evalError (Just reg) "first argument to map: not a function"
evalExp _ (LocalSize reg) = evalError (Just reg) "localSize not implemented"

---------------------
-- Various helpers --
---------------------
generate :: Show ty => Int -> Env ty -> Name -> Exp ty -> Eval (Value ty)
generate n env' var ebody = do
  let arr = fromFunction n (\i -> evalExp (insertVar var (IntV i) env') ebody)
  arr' <- materializeM arr
  return (ArrayV arr') 

map_ :: Show ty => Env ty -> Name -> Exp ty -> FCLArray (Value ty) -> Eval (Value ty)
map_ env' var ebody arr = do
  let arr' = mapA (\x -> evalExp (insertVar var x env') ebody) arr
  arr'' <- materializeM arr'
  return (ArrayV arr'')

scanM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
scanM _ _ []     = return []
scanM f v (u:us) =
  do v' <- f v u
     rest <- scanM f v' us
     return (v : rest)

scanl_ :: Show ty => Env ty -> Name -> Exp ty -> Value ty -> FCLArray (Value ty) -> Eval (Value ty)
scanl_ env' var ebody v arr = do
  let vs = toList arr
  let f v1 v2 = evalExp (insertVar var (PairV v1 v2) env') ebody
  vs' <- scanM f v vs
  return (ArrayV (fromList vs'))

while :: Region
      -> (Value ty -> Eval (Value ty))
      -> (Value ty -> Eval (Value ty))
      -> Value ty
      -> Eval (Value ty)
while reg f body x = do
  cond <- f x
  case cond of
    BoolV True -> while reg f body =<< body x
    BoolV False -> return x
    _ -> evalError (Just reg) "Second argument to while should return Bool"

unInt ::  Region -> String -> Value ty -> Eval Int
unInt reg str v =
  case v of
    IntV v' -> return v'
    _ -> evalError (Just reg) ("expecting int in " ++ str)

unDouble :: Region -> String -> Value ty -> Eval Double
unDouble reg str v =
  case v of
    DoubleV v' -> return v'
    _ -> evalError (Just reg) ("expecting int in " ++ str)

unBool :: Region -> String -> Value ty -> Eval Bool
unBool reg str v =
  case v of
    BoolV v' -> return v'
    _ -> evalError (Just reg) ("expecting int in " ++ str)

unArray :: Region -> String -> Value ty -> Eval (FCLArray (Value ty))
unArray reg str v =
  case v of
    ArrayV v' -> return v'
    _ -> evalError (Just reg) ("Expecting double in " ++ str)

----------------------
-- Unused old stuff --
----------------------
-- import Data.List (sortBy)
-- import Data.Ord (comparing)

-- sortOn :: Ord b => (a -> b) -> [a] -> [a]
-- sortOn f ls = sortBy (comparing f) ls

-- assemble :: (Int -> Int -> Int) -> [[a]] -> [a]
-- assemble f array =
--   let buildAssocList _ _ [] = []
--       buildAssocList i _ ([]:xs) = buildAssocList (i+1) 0 xs
--       buildAssocList i j ((y:ys):xs) =
--         (f i j, y) : buildAssocList i (j+1) (ys:xs)
--   in Prelude.map Prelude.snd (sortOn Prelude.fst (buildAssocList 0 0 array))
