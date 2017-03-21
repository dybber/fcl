module Sklansky1 where

import Data.Bits

--------------------
-- From Obsidian library and Obsidian JFP paper:
-- https://github.com/svenssonjoel/Obsidian/blob/master/Obsidian/Library.hs#L349
-- https://github.com/svenssonjoel/Obsidian/blob/master/Benchmarks/ScanBench/Scan.hs#L47
--------------------
unsafeBinSplit :: Int -> ([a] -> [b]) -> [a] -> [b]
unsafeBinSplit = twoK

twoK :: Int -> ([a] -> [b]) -> [a] -> [b]
twoK 0 f = f
twoK n f =
  \arr -> 
     let c    = 2^n -- size of blocks
         m    = (length arr `div` c) -- number of blocks
         nl2   = length (f (map (\j -> arr !! undefined) [0..m-1])) -- number of blocks after
         lt    = nl2 * c -- total size after
         g i j = i .&. (fromIntegral (complement (m-1))) .|. j -- offset to block + offset inside block
         h i   = i .&. (fromIntegral (nl2-1)) -- index of block
         
     in map (\i -> (f (map (\j -> (arr !! (g i j))) [0..m-1])) !! (h i)) [0..lt-1]

halve :: [a] -> ([a], [a])
halve arr = splitAt (length arr `div` 2) arr

fan :: (a -> a -> a) -> [a] -> [a]
fan op arr =  a1 ++ map (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 !! fromIntegral (length a1 - 1)

sklansky :: Int -> (a -> a -> a) -> [a] -> [a]
sklansky 0 op arr = arr
sklansky n op arr =
  let arr1 = unsafeBinSplit (n-1) (fan op) arr
  in sklansky (n-1) op arr1


test0 = sklansky 3 (+) [1..8] == scanl1 (+) [1..8]
test1 = sklansky 4 (+) [1..16] == scanl1 (+) [1..16]
test2 = sklansky 5 (+) [1..32] == scanl1 (+) [1..32]
test3 = sklansky 6 (+) [1..64] == scanl1 (+) [1..64]
test4 = sklansky 7 (+) [1..128] == scanl1 (+) [1..128]

tests = [test0, test1, test2, test3, test4]

-- Note: scanl is exclusive
--       scanl1 is inclusive
--       sklansky is inclusive

------------

binsplit :: Int -> ([Int] -> [Int]) -> [Int] -> [Int]
binsplit 0 f = f
binsplit n f =
  \arr -> 
     let c    = 2^n -- number of blocks
         m    = (length arr `div` c) -- size of each block
         lt    = c * m -- total size after
         h i   = i `mod` m -- global index -> index inside block
         g i j = (i `div` m) * m -- block-offset from global index
                 + j             -- index inside block
     in
       map (\i -> (f (map (\j -> (arr !! (g i j))) [0..m-1])) !! (h i)) [0..lt-1]

binsplitSimple :: Int -> ([Int] -> [Int]) -> [Int] -> [Int]
binsplitSimple 0 f = f
binsplitSimple n f =
  \arr -> 
     let c    = 2^n -- number of blocks
         m    = (length arr `div` c) -- size of each block
     in
       concat [f (map (\j -> arr !! (blockID*m + j)) [0..m-1])
              | blockID <- [0..c-1]]

sklanskySimple :: Int -> (Int -> Int -> Int) -> [Int] -> [Int]
sklanskySimple 0 op arr = arr
sklanskySimple n op arr =
  let arr1 = binsplitSimple (n-1) (fan op) arr
  in sklansky (n-1) op arr1

