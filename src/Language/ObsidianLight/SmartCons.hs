module Language.ObsidianLight.SmartCons where

import Control.Monad.State

import Language.ObsidianLight.Syntax

newtype Exp t = E OExp

type M x = State Int x

newVar :: M VarName
newVar = do
  c <- get
  modify (+1)
  return ("x" ++ show c)

class Scalar t where
  constant :: t -> Exp t

instance Scalar Int where
  constant = E . IntScalar

instance Scalar Bool where
  constant = E . BoolScalar

addi, subi, muli, divi, modi, mini :: Exp Int -> Exp Int -> Exp Int
(E e0) `addi` (E e1) = E (BinOp AddI e0 e1)
(E e0) `subi` (E e1) = E (BinOp SubI e0 e1)
(E e0) `muli` (E e1) = E (BinOp MulI e0 e1)
(E e0) `divi` (E e1) = E (BinOp DivI e0 e1)
(E e0) `modi` (E e1) = E (BinOp ModI e0 e1)
(E e0) `mini` (E e1) = E (BinOp MinI e0 e1)

eqi :: Exp Int -> Exp Int -> Exp Bool
(E e0) `eqi` (E e1) = E (BinOp EqI e0 e1)


lam :: (Exp a -> Exp b) -> M (Exp (a -> b))
lam f = do
  x <- newVar
  let E body = f (E (Var x))
  return (E (Lamb x body))

lett :: Exp a -> (Exp a -> Exp b) -> M (Exp b)
lett (E e0) e1 = do
  x <- newVar
  let E body = e1 (E (Var x))
  return (E (Let x e0 body))

app :: Exp (a -> b) -> Exp a -> Exp b
app (E f) (E x) = E (App f x)

if_ :: Exp Bool -> Exp t -> Exp t -> Exp t
if_ (E econd) (E etrue) (E efalse) =
  E (Cond econd etrue efalse)

pair :: Exp a -> Exp b -> Exp (a,b)
pair (E e0) (E e1) = E (Pair e0 e1)

proj1 :: Exp (a,b) -> Exp a
proj1 (E e) = E (Proj1E e)

proj2 :: Exp (a,b) -> Exp b
proj2 (E e) = E (Proj2E e)

index :: Exp [a] -> Exp Int -> Exp a
index (E arr) (E idx) = E (Index arr idx)

length :: Exp [a] -> Exp Int
length (E arr) = E (Length arr)

map :: Exp (a -> b) -> Exp [a] -> Exp [b]
map (E f) (E arr) = E (Map f arr)

generate :: Level -> Exp (Int -> a) -> Exp Int -> Exp [a]
generate lvl (E f) (E n) = E (Generate lvl f n)

fix :: Exp (a -> Bool) -> Exp (a -> a) -> Exp a -> Exp a
fix (E cond) (E body) (E init') = E (Fixpoint cond body init')

force :: Exp a -> Exp a
force (E e) = E (ForceLocal e)
