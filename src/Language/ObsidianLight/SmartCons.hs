{-# LANGUAGE FlexibleInstances #-}
module Language.ObsidianLight.SmartCons
(Type(..),
 Level(..),
 Obs, runObs,
 
 constant,

 addi, subi, muli, divi, modi, mini,
 eqi,

 lam, app, pair, lett, if_, proj1, proj2,

 map, generate, force, index, (!), len, fixpoint,

 compile, Kernel(..), NoType

)
where

import Control.Monad.State
import Control.Applicative
import Prelude hiding (map)

import Language.ObsidianLight.Syntax
import Language.GPUIL.Syntax (Kernel(..), NoType)
import Language.ObsidianLight.TypeChecker (typecheck)
import qualified Language.ObsidianLight.Compile as C (compile)

newtype Obs x = Obs (State Int (Exp Untyped))

runObs :: Obs a -> Exp Untyped
runObs (Obs m) = evalState m 0

compile :: String -> Obs a -> IO (Kernel NoType)
compile name e = do
  putStrLn $ "Unfolding Smart constructors: " ++ name
  let uexp = runObs e
  print uexp
  putStrLn $ "Typechecking: " ++ name
  let (texp, _) = typecheck uexp
  print texp
  putStrLn $ "Compiling: " ++ name
  return (C.compile name texp)

newVar :: State Int String
newVar = do
  c <- get
  modify (+1)
  return ("x" ++ show c)

class Scalar t where
  constant :: t -> Obs t
instance Scalar Int where
  constant = Obs . return . IntScalar
instance Scalar Bool where
  constant = Obs . return . BoolScalar
instance Scalar Double where
  constant = Obs . return . DoubleScalar

addi, subi, muli, divi, modi, mini :: Obs Int -> Obs Int -> Obs Int
addi (Obs x0) (Obs x1) = Obs (BinOp AddI <$> x0 <*> x1)
subi (Obs x0) (Obs x1) = Obs (BinOp SubI <$> x0 <*> x1)
muli (Obs x0) (Obs x1) = Obs (BinOp MulI <$> x0 <*> x1)
divi (Obs x0) (Obs x1) = Obs (BinOp DivI <$> x0 <*> x1)
modi (Obs x0) (Obs x1) = Obs (BinOp ModI <$> x0 <*> x1)
mini (Obs x0) (Obs x1) = Obs (BinOp MinI <$> x0 <*> x1)

absi, signi :: Obs Int -> Obs Int
absi (Obs x0) = Obs (UnOp AbsI <$> x0)
signi (Obs x0) = Obs (UnOp SignI <$> x0)

eqi :: Obs Int -> Obs Int -> Obs Bool
eqi (Obs x0) (Obs x1) = Obs (BinOp EqI <$> x0 <*> x1)

instance Num (Obs Int) where
  (+) = addi
  (-) = subi
  (*) = muli
  abs = absi
  signum = signi
  fromInteger = constant . fromInteger

lam :: Type -> (Obs a -> Obs b) -> Obs (a -> b)
lam ty f = Obs $ do
  x <- newVar
  let Obs body = f (Obs (return (Var x Untyped)))
  body' <- body
  return (Lamb x ty body' Untyped)

app :: Obs (a -> b) -> Obs a -> Obs b
app (Obs f) (Obs x) = Obs (App <$> f <*> x)

lett :: Obs a -> (Obs a -> Obs b) -> Obs b
lett (Obs e0) e1 = Obs $ do
  e0' <- e0
  x <- newVar
  let Obs body = e1 (Obs (return (Var x Untyped)))
  body' <- body
  return (Let x e0' body' Untyped)

if_ :: Obs Bool -> Obs t -> Obs t -> Obs t
if_ (Obs econd) (Obs etrue) (Obs efalse) =
  Obs (Cond <$> econd <*> etrue <*> efalse <*> pure Untyped)

pair :: Obs a -> Obs b -> Obs (a,b)
pair (Obs e0) (Obs e1) = Obs (Pair <$> e0 <*> e1)

proj1 :: Obs (a,b) -> Obs a
proj1 (Obs e) = Obs (Proj1E <$> e)

proj2 :: Obs (a,b) -> Obs b
proj2 (Obs e) = Obs (Proj2E <$> e)

index :: Obs [a] -> Obs Int -> Obs a
index (Obs arr) (Obs idx) = Obs (Index <$> arr <*> idx)

(!) :: Obs [a] -> Obs Int -> Obs a
(!) = index

len :: Obs [a] -> Obs Int
len (Obs arr) = Obs (Length <$> arr)

map :: Obs (a -> b) -> Obs [a] -> Obs [b]
map (Obs f) (Obs arr) = Obs (Map <$> f <*> arr)

generate :: Level -> Obs Int -> Obs (Int -> a) -> Obs [a]
generate lvl (Obs n) (Obs f) = Obs (Generate lvl <$> n <*> f)

fixpoint :: Obs (a -> Bool) -> Obs (a -> a) -> Obs a -> Obs a
fixpoint (Obs cond) (Obs body) (Obs init') = Obs (Fixpoint <$> cond <*> body <*> init')

force :: Obs a -> Obs a
force (Obs e) = Obs (ForceLocal <$> e)
