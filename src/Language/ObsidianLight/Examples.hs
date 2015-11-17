module Language.ObsidianLight.Examples where

-- import Language.GPUIL (generateKernel, Level(..))
import Language.GPUIL.PrettyLib (render)
import Language.GPUIL.PrettyOpenCL (ppKernel)
-- import Language.ObsidianLight.Compile
import Language.ObsidianLight.SmartCons
import Prelude hiding (map, splitAt, zipWith)

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
  let lhs = generate lvl n (lam IntT (arr !))
      rhs = generate lvl (len arr - n) (lam IntT (\x -> arr ! (x + n)))
  in pair lhs rhs

halve :: Level -> Obs [a] -> Obs ([a], [a])
halve lvl arr = splitAt lvl (len arr `divi` constant 2) arr

add :: Obs (Int -> Int -> Int)
add = lam IntT (\x -> lam IntT (\y -> addi x y))

--------------------
-- Basic examples --
--------------------
mapIota1000 :: Obs [Int]
mapIota1000 =
  let g = lam IntT (\x -> constant 20000 + x)
  in map g (iota (constant 1000))

mapForceMap :: Obs [Int]
mapForceMap =
  let f = lam IntT (\x -> constant 10 * x)
      g = lam IntT (\x -> constant 20000 + x)
  in map f (force (map g (iota (constant 1000))))

mapIndex :: Level -> Obs [Int] -> Obs [Int]
mapIndex lvl a = generate lvl (len a) $
                    lam IntT (\ix -> (a ! ix) `divi` constant 17)

-- Reduction examples from Obsidian

-- Block-level reduction
red1 :: Obs (a -> a -> a) -> Obs [a] -> Obs [a]
red1 f array =
  let cond = lam (ArrayT Block IntT) (\arr -> constant 1 `eqi` len arr)
      step = lam (ArrayT Block IntT) $
               \arr -> lett (halve Block arr) (\y -> zipWith Block f (proj1 y) (proj2 y))
  in fixpoint cond step array

compileAndPrint :: String -> Obs a -> IO ()
compileAndPrint name e = do
  kernel <- compile name e
  putStrLn "Output:"
  print kernel
  putStrLn ""
  putStrLn (render 4 2 (ppKernel kernel))

test_mapIota :: IO ()
test_mapIota = compileAndPrint "mapIota" (force mapIota1000)
test_mapForceMap :: IO ()
test_mapForceMap = compileAndPrint "mapForceMap" (force mapForceMap)
testred1 :: IO ()
testred1 = compileAndPrint "red1" (lam (ArrayT Block IntT) (red1 add))
