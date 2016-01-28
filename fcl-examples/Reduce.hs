module Reduce where


import Language.FCL

import Prelude hiding (map, splitAt, zipWith, concat, fst, snd)

add :: Obs (Int -> Int -> Int)
add = lam IntT (\x -> lam IntT (\y -> addi x y))


-- Block-level reduction
red1 :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
red1 f =
  let cond = lam (ArrayT Block IntT) (\arr -> 1 `eqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (evenOdds Block arr) (\y -> zipWith Block f (fst y) (snd y))
  in concat 1
       . map (lam (ArrayT Block IntT) (\arr -> fixpoint cond step arr))
       . splitUp 512

-- The array will be materialized before looping
red2 :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
red2 f array =
  let cond = lam (ArrayT Block IntT) (\arr -> constant 1 `eqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (halve Block arr) (\y -> zipWith Block f (fst y) (snd y))
  in fixpoint cond step array

-- Here we take one step before starting the loop, to do some actual
-- work before materialization.
red2_1 :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
red2_1 f =
  let cond = lam (ArrayT Block IntT) (\arr -> 1 `eqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (halve Block arr) (\y -> zipWith Block f (fst y) (snd y))
  in concat 1
     . map (lam (ArrayT Block IntT) (\arr ->
              fixpoint cond step (step `app` arr)))
     . splitUp 512

testred1 :: IO ()
testred1 = compileAndPrint "red1" (lam (ArrayT Block IntT) (red1 add))
testred2 :: IO ()
testred2 = compileAndPrint "red2" (lam (ArrayT Block IntT) (red2 add))
testred2_1 :: IO ()
testred2_1 = compileAndPrint "red2_1" (lam (ArrayT Block IntT) (red2_1 add))
