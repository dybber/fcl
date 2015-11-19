module Language.ObsidianLight.Examples where

import Language.GPUIL (renderKernel)

import Language.ObsidianLight.SmartCons
import Prelude hiding (map, splitAt, zipWith, concat)

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
  generate Block (len arr `divi` n) $
    lam IntT (\i ->
      generate Block n
        (lam IntT (\j -> arr ! ((i * n) + j))))

coalesce :: Obs Int -> Obs [a] -> Obs [[a]]
coalesce n arr =
  generate Block s $ lam IntT $ \i ->
    generate Block n $ lam IntT $ \j -> arr ! (i + (s * j))
  where s = len arr `divi` n

add :: Obs (Int -> Int -> Int)
add = lam IntT (\x -> lam IntT (\y -> addi x y))

--------------------
-- Basic examples --
--------------------
mapIota1000 :: Obs (Int -> [Int])
mapIota1000 =
  lam IntT $ \n ->
    let g = lam IntT (\x -> constant 20000 + x)
    in map g (iota n)

mapForceMap :: Obs [Int]
mapForceMap =
  let f = lam IntT (\x -> constant 10 * x)
      g = lam IntT (\x -> constant 20000 + x)
  in map f (force (map g (iota (constant 1000))))

mapIndex :: Level -> Obs [Int] -> Obs [Int]
mapIndex lvl a = generate lvl (len a) $
                    lam IntT (\ix -> (a ! ix) `divi` constant 17)


-- Distribute examples
double :: Obs ([Int] -> [Int])
double =
  lam (ArrayT Block IntT) $ \arr ->
    let g = lam (ArrayT Block IntT)
              (\part -> (map (lam IntT (\x -> 2 * x)) part))
    in concat (map g (splitUp (constant 512) arr))

-- Reduction examples from Obsidian

-- Block-level reduction
red1 :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
red1 f array =
  let cond = lam (ArrayT Block IntT) (\arr -> constant 1 `eqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (evenOdds Block arr) (\y -> zipWith Block f (proj1 y) (proj2 y))
  in fixpoint cond step array

-- The array will first materialized before looping
red2 :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
red2 f array =
  let cond = lam (ArrayT Block IntT) (\arr -> constant 1 `eqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (halve Block arr) (\y -> zipWith Block f (proj1 y) (proj2 y))
  in fixpoint cond step array

-- Here we take one step before starting the loop, to do some actual
-- work before materialization.
red2_1 :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
red2_1 f array =
  let cond = lam (ArrayT Block IntT) (\arr -> constant 1 `eqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (halve Block arr) (\y -> zipWith Block f (proj1 y) (proj2 y))
  in fixpoint cond step (step `app` array)

compileAndPrint :: String -> Obs a -> IO ()
compileAndPrint name e = do
  kernel <- compile name e
  putStrLn "Output:"
  print kernel
  putStrLn ""
  putStrLn (renderKernel kernel)

test_mapIota :: IO ()
test_mapIota = compileAndPrint "mapIota" mapIota1000
test_mapForceMap :: IO ()
test_mapForceMap = compileAndPrint "mapForceMap" mapForceMap
testdouble :: IO ()
testdouble = compileAndPrint "double0" double --
testred1 :: IO ()
testred1 = compileAndPrint "red1" (lam (ArrayT Block IntT) (red1 add))
testred2 :: IO ()
testred2 = compileAndPrint "red2" (lam (ArrayT Block IntT) (red2 add))
testred2_1 :: IO ()
testred2_1 = compileAndPrint "red2_1" (lam (ArrayT Block IntT) (red2_1 add))
