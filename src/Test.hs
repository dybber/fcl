module TestEval where

import Language.ObsidianLight
import Prelude hiding (map, splitAt, zipWith, concat)
import qualified Prelude

toArray :: [Int] -> Obs [Int]
toArray = fromList . (Prelude.map constant)

vec0 :: Obs [Int]
vec0 = toArray [0..1000]

v :: Value Untyped
v = eval (splitUp (constant 512) vec0)

test_map :: (Int -> Int) -> Obs (Int -> Int) -> Bool
test_map f g =
  let ls0 = [0..9]
      expected = toArray (Prelude.map f ls0)
      result   = map g (toArray ls0)
  in eval result == eval expected

test_addi :: Bool
test_addi = test_map (+ 10) (lam IntT (\x -> x + constant 10))

test_subi :: Bool
test_subi = test_map ((-) 10) (lam IntT (\x -> x - constant 10))

test_zipWith :: (Int -> Int -> Int) -> Obs (Int -> Int -> Int) -> Bool
test_zipWith f g =
  let ls0 = [0..99]
      ls1 = [100..199]
      expected = toArray (Prelude.zipWith f ls0 ls1)
      result   = zipWith Block g (toArray ls0) (toArray ls1)
  in eval expected == eval result

test_reduce :: (Int -> Int -> Int) -> Obs (Int -> Int -> Int) -> Bool
test_reduce f g =
  let ls0 = [0..99]
      expected = toArray [foldl1 f ls0]
      result   = reduce g (toArray ls0)
  in eval expected == eval result

add :: Obs (Int -> Int -> Int)
add = lam IntT (\x -> lam IntT (\y -> addi x y))

t = eval (reduce add (toArray [0..511]))
