module Maps where

import Language.FCL
import Prelude hiding (map, splitAt, zipWith, concat)

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
    let mapmult2 :: Obs ([Int] -> [Int])
        mapmult2 = lam (ArrayT Block IntT)
                    (\part -> (map (lam IntT (\x -> 2 * x)) part))
    in concat (map mapmult2 (splitUp (constant 512) arr))

doubleForceDouble :: Obs ([Int] -> [Int])
doubleForceDouble =
  lam (ArrayT Block IntT) $ \arr ->
    let mapmult2 :: Obs ([Int] -> [Int])
        mapmult2 = lam (ArrayT Block IntT)
                       (\part -> (map (lam IntT (\x -> 2 * x)) part))
        mapmult2_twice = lam (ArrayT Block IntT)
                             (\array -> mapmult2 `app` (force (mapmult2 `app` array)))
    in concat (map mapmult2_twice (splitUp (constant 512) arr))


-- Reduction examples from Obsidian
test_mapIota :: IO ()
test_mapIota = compileAndPrint "mapIota" mapIota1000
test_mapForceMap :: IO ()
test_mapForceMap = compileAndPrint "mapForceMap" mapForceMap
testdouble :: IO ()
testdouble = compileAndPrint "double0" double
testdouble2 :: IO ()
testdouble2 = compileAndPrint "doubleForceDouble0" doubleForceDouble
