module Language.ObsidianLight.Library where

import Language.ObsidianLight.SmartCons
import Prelude hiding (map, splitAt, zipWith, concat, fst, snd)

-------------
-- Prelude --
-------------
iota :: Obs Int -> Obs [Int]
iota n = generate Block n (lam IntT (\x -> x))

zipWith :: Level -> Obs (a -> b -> c) -> Obs [a] -> Obs [b] -> Obs [c]
zipWith lvl f a1 a2 =
  generate lvl (mini (len a1) (len a2)) $
    lam IntT (\ix -> (f `app` (a1 ! ix)) `app` (a2 ! ix))

-- Split an array at a given point
splitAt :: Level -> Obs Int -> Obs [a] -> Obs ([a], [a])
splitAt lvl n arr =
  lett n $ \n' ->
    pair
      (generate lvl n' (lam IntT (arr !)))
      (generate lvl (len arr - n') (lam IntT (\x -> arr ! (x + n'))))

halve :: Level -> Obs [a] -> Obs ([a], [a])
halve lvl arr = splitAt lvl (len arr `divi` constant 2) arr

evenOdds :: Level -> Obs [a] -> Obs ([a], [a])
evenOdds lvl arr =
  let n :: Obs Int
      n  = len arr
  in
    lett (n `divi` 2) $ \n2 ->
      pair
        (generate lvl (n-n2) (lam IntT (\ix -> arr ! (constant 2*ix))))
        (generate lvl n2     (lam IntT (\ix -> arr ! (constant 2*ix + constant 1))))

splitUp :: Obs Int -> Obs [a] -> Obs [[a]]
splitUp n arr {-(Pull m ixf)-} =
  generate Block ((len arr + n - 1) `divi` n) $
    lam IntT (\i ->
      generate Block n
        (lam IntT (\j -> arr ! ((i * n) + j))))

coalesce :: Obs Int -> Obs [a] -> Obs [[a]]
coalesce n arr =
  generate Block s $ lam IntT $ \i ->
    generate Block n $ lam IntT $ \j -> arr ! (i + (s * j))
  where s = len arr `divi` n

seqReduce :: Obs (a -> a -> a) -> Obs a -> Obs [a] -> Obs a
seqReduce f b array =
  lett (len array) $ \n ->
  let cond = lam (IntT :*: IntT) (\v -> fst v `neqi` n)
      step = lam (IntT :*: IntT) $
               \v -> pair (fst v + 1) (f `app` (array ! fst v) `app` (snd v))
  in snd (fixpoint cond step (pair (constant 0) b))

reduce :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
reduce f =
  let cond = lam (ArrayT Block IntT) (\arr -> 1 `neqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (halve Block arr)
                            (\y -> zipWith Block f (fst y) (snd y))
  in concat 1
       . map (lam (ArrayT Block IntT)
                  (\arr -> fixpoint cond step (step `app` arr)))
       . splitUp 512

concat2 :: Obs Int -> Obs [[a]] -> Obs [a]
concat2 n = assemble n (lam (IntT :*: IntT) (\x -> (fst x)*n + (snd x)))
