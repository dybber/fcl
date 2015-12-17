{-# LANGUAGE FlexibleInstances #-}
module Language.ObsidianLight.SmartCons
(Type(..),
 Level(..),
 Untyped,
 Obs, runObs,
 
 constant, fromList,

 addi, subi, muli, divi, modi, mini,
 andi, xori, shiftLi, shiftRi,
 eqi, neqi,

 lam, app, pair, lett, if_, fst, snd,

 map, generate, force, index, (!), len, fixpoint, concat, assemble
)
where

import Control.Monad.State
import Control.Applicative
import Prelude hiding (map, concat, fst, snd)

import Language.ObsidianLight.Syntax

newtype Obs x = Obs (State Int (Exp Untyped))

runObs :: Obs a -> Exp Untyped
runObs (Obs m) = evalState m 0

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

fromList :: [Obs a] -> Obs [a]
fromList arr =
  Obs $ do
    xs <- mapM unObs arr
    return (Vec xs Untyped)
 where unObs (Obs a) = a

addi, subi, muli, divi, modi, mini :: Obs Int -> Obs Int -> Obs Int
addi (Obs x0) (Obs x1) = Obs (BinOp AddI <$> x0 <*> x1)
subi (Obs x0) (Obs x1) = Obs (BinOp SubI <$> x0 <*> x1)
muli (Obs x0) (Obs x1) = Obs (BinOp MulI <$> x0 <*> x1)
divi (Obs x0) (Obs x1) = Obs (BinOp DivI <$> x0 <*> x1)
modi (Obs x0) (Obs x1) = Obs (BinOp ModI <$> x0 <*> x1)
mini (Obs x0) (Obs x1) = Obs (BinOp MinI <$> x0 <*> x1)

andi, xori, shiftLi, shiftRi :: Obs Int -> Obs Int -> Obs Int
andi (Obs x0) (Obs x1) = Obs (BinOp AndI <$> x0 <*> x1)
xori (Obs x0) (Obs x1) = Obs (BinOp XorI <$> x0 <*> x1)
shiftLi (Obs x0) (Obs x1) = Obs (BinOp ShiftLI <$> x0 <*> x1)
shiftRi (Obs x0) (Obs x1) = Obs (BinOp ShiftRI <$> x0 <*> x1)


absi, signi :: Obs Int -> Obs Int
absi (Obs x0) = Obs (UnOp AbsI <$> x0)
signi (Obs x0) = Obs (UnOp SignI <$> x0)

neqi, eqi :: Obs Int -> Obs Int -> Obs Bool
eqi (Obs x0) (Obs x1) = Obs (BinOp EqI <$> x0 <*> x1)
neqi (Obs x0) (Obs x1) = Obs (BinOp NeqI <$> x0 <*> x1)

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

fst :: Obs (a,b) -> Obs a
fst (Obs e) = Obs (Proj1E <$> e)

snd :: Obs (a,b) -> Obs b
snd (Obs e) = Obs (Proj2E <$> e)

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

concat :: Obs Int -> Obs [[a]] -> Obs [a]
concat (Obs i) (Obs arr) = Obs (Concat <$> i <*> arr)

assemble :: Obs Int -> Obs ((Int, Int) -> Int) -> Obs [[a]] -> Obs [a]
assemble (Obs i) (Obs f) (Obs arr) = Obs (Assemble <$> i <*> f <*> arr)

force :: Obs a -> Obs a
force (Obs e) = Obs (ForceLocal <$> e)
