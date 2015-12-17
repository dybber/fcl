module Sobol where

import Language.ObsidianLight
import Prelude hiding (map, splitAt, zipWith, concat, fst, snd)
import qualified Prelude


sobol_num_bits :: Obs Int
sobol_num_bits = constant 30

sobol_divisor :: Obs Int
sobol_divisor = constant (2^(30 :: Int))

fromBool :: Obs (Bool -> Int)
fromBool = lam BoolT (\b -> if_ b (constant 1) (constant 0))

bit :: Obs (Int -> Int)
bit = lam IntT (\i -> 1 `shiftLi` i)

testBit :: Obs (Int -> Int -> Bool)
testBit = lam IntT (\x -> lam IntT (\i ->
            (x `andi` (bit `app` i)) `neqi` constant 0))

grayCode :: Obs (Int -> Int)
grayCode = lam IntT (\x -> x `xori` (x `shiftRi` 1))

mul :: Obs (Int -> Int -> Int)
mul = lam IntT (\x -> lam IntT (\y -> muli x y))

xor :: Obs (Int -> Int -> Int)
xor = lam IntT (\x -> lam IntT (\y -> xori x y))

-- Two direction vectors
sobol_dirVs :: Obs [[Int]]
sobol_dirVs =
  fromList $ Prelude.map (fromList . Prelude.map constant)
    [[536870912,268435456,134217728,67108864,33554432,
      16777216,8388608,4194304,2097152,1048576,524288,
      262144,131072,65536,32768,16384,8192,4096,2048,
      1024,512,256,128,64,32,16,8,4,2,1],
     [536870912,805306368,671088640,1006632960,570425344,
      855638016,713031680,1069547520,538968064,808452096,
      673710080,1010565120,572653568,858980352,715816960,
      1073725440,536879104,805318656,671098880,1006648320,
      570434048,855651072,713042560,1069563840,538976288,
      808464432,673720360,1010580540,572662306,858993459]]

-- Two direction vectors
sobol_dirVs1 :: Obs [Int]
sobol_dirVs1 =
  fromList . Prelude.map constant $
    [536870912,268435456,134217728,67108864,33554432,
     16777216,8388608,4194304,2097152,1048576,524288,
     262144,131072,65536,32768,16384,8192,4096,2048,
     1024,512,256,128,64,32,16,8,4,2,1]

sobolIndependent :: Obs (Int -> [Int] -> Int)
sobolIndependent =
  lam IntT (\ix ->
    lam (ArrayT Block IntT) (\dirVec ->
      let
        bitVec :: Obs [Int]
        bitVec = generate
                     Block
                     sobol_num_bits
                     (lam IntT (\i -> fromBool `app` (testBit `app` (grayCode `app` ix) `app` i)))
      in seqReduce xor 0 (zipWith Block mul dirVec bitVec)))

-- Find first set
ffs :: Obs (Int -> Int)
ffs =
  lam IntT (\ix ->
    let cond = lam (IntT :*: IntT) (\v -> (snd v `andi` 1) `neqi` 0)
        step = lam (IntT :*: IntT) (\v -> pair (fst v + 1) (snd v `shiftRi` 1))
    in fst (fixpoint cond step (pair 0 ix)))

sobolRec :: Obs ([Int] -> Int -> Int -> Int)
sobolRec =
  lam (ArrayT Block IntT) (\dirVs ->
     lam IntT (\prev ->
        lam IntT (\n ->
          let bit_ = ffs `app` n
          in xor `app` prev `app` (dirVs ! bit_))))

-- Sobol recursive (Integer): get sobol N+1 from N for each of the
-- direction vectors
sobolRecI :: Obs ([[Int]] -> [Int] -> Int -> [Int])
sobolRecI =
  lam (ArrayT Block (ArrayT Block IntT)) (\dirVs ->
     lam (ArrayT Block IntT) (\prev ->
        lam (IntT) (\n ->
          let bit = ffs `app` n
              dir_vs = map (lam (IntT) (\vs -> vs ! bit)) dirVs
          in zipWith Block xor prev dir_vs)))

-- -- Use inductive sobol to initialize, then use recursion
-- -- real -> int -> word list list -> int * int -> real list list
-- sobolRecMap :: Obs ([[Int]] -> (Int,Int) -> Int -> [Int])
-- sobolRecMap dir_vs (l : int, u : int) =
--   lam (ArrayT Block (ArrayT Block IntT)) (\dirVs ->
--      lam (IntT :*: IntT) (\lu ->
--        let l = fst lu
--            u = snd lu
--            first = sobolIndI `app` l `app` dir_vs
--            range = List.tabulate (u-l, fn i => Word.fromInt (i+l))
--        in  scanl (sobolRecI bits_num dir_vs) first range



-- sobolIndReal :: Obs Int -> Obs [Int] -> Obs [Float]
-- sobolIndReal i dirVs = fmap normalise $ sobolIndependent i dirVs
--   where
--    normalise x = (i2d x) `divi` sobol_divisor

test :: Obs Int -> Value Untyped
test i = (eval $ sobolIndependent `app` i `app` sobol_dirVs1)
