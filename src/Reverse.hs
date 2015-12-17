module Reverse where

import Language.ObsidianLight
import Prelude hiding (map, splitAt, zipWith, concat, fst, snd, reverse)
import qualified Prelude

-- These two are the same except for the types
-- we do not have polymorphism yet
reverse :: Obs ([Int] -> [Int])
reverse =
  lam (ArrayT Block IntT)
    (\arr ->
        lett (len arr) $ \n ->
          generate Block n (lam IntT $ \i -> arr ! (n-i-1)))

reverseBlocks :: Obs ([[Int]] -> [[Int]])
reverseBlocks =
  lam (ArrayT Block (ArrayT Block IntT))
    (\arr ->
        lett (len arr) $ \n ->
          generate Block n (lam IntT $ \i -> arr ! (n-1-i)))


reverseGrid :: Obs Int -> Obs ([Int] -> [Int])
reverseGrid splitSize =
  lam (ArrayT Block IntT)
    (\arr -> concat2 splitSize $ reverseBlocks `app` 
               map (lam (ArrayT Block IntT) $
                      \arr' -> force (reverse `app` arr'))
               (splitUp splitSize arr))

test_reverse = compileAndPrint "reverse" (reverseGrid 512)

toArray :: [Int] -> Obs [Int]
toArray = fromList . (Prelude.map constant)
t = eval (reverseGrid 10 `app` (toArray [1..100]))


-- compileKernel (reverseGrid 512) (Config {blockSize = 512, warpSize = 32})
