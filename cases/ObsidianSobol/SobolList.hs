module Sobol where

import Data.Word
import Data.Bits

fromBool True = 1
fromBool False = 0

sobol_num_bits :: Int
sobol_num_bits = 30
sobol_dim :: Word32
sobol_dim = 2
sobol_divisor :: Double
sobol_divisor = fromIntegral (2 ^ sobol_num_bits)

-- Two direction vectors
sobol_dirVs :: [[Word32]]
sobol_dirVs = [[536870912,268435456,134217728,67108864,33554432,
                16777216,8388608,4194304,2097152,1048576,524288,
                262144,131072,65536,32768,16384,8192,4096,2048,
                1024,512,256,128,64,32,16,8,4,2,1],
               [536870912,805306368,671088640,1006632960,570425344,
                855638016,713031680,1069547520,538968064,808452096,
                673710080,1010565120,572653568,858980352,715816960,
                1073725440,536879104,805318656,671098880,1006648320,
                570434048,855651072,713042560,1069563840,538976288,
                808464432,673720360,1010580540,572662306,858993459]]

grayCode :: Int -> Word32
grayCode ix = fromIntegral (ix `xor` (ix `shiftR` 1))

-------------------------------
-- Sobol independent formula --
-------------------------------
-- Takes a direction vector and an index, computes the sobol number at
-- that index.
sobolInd :: Int -> [Word32] -> Word32
sobolInd ix dirVec = 
  let bitVec :: [Word32]
      bitVec = map (\i -> fromBool $ testBit (grayCode ix) i)
                   [0..sobol_num_bits-1]
  in foldr xor 0 $ zipWith (*) dirVec bitVec

-- Creates a length n sobol sequences using the given direction vector
sobol1D :: Int -> [Word32] -> [Word32]
sobol1D n dirVec = map (\ix -> sobolInd ix dirVec) [0..n-1]

-- Creates multi-dimensional sobol-sequences of length n
-- with as many dimensions as there are direction vectors
sobolND :: Int -> [[Word32]] -> [[Word32]]
sobolND n dirVs = map (\ix -> map (sobolInd ix) dirVs) [1..n]


---------------------
-- Sobol recursive --
---------------------
-- index of the rightmost zero bit (find first zero)
ffz :: Int -> Int
ffz n = loop 0 n
 where
   loop count c = if c .&. 1 == 0
                  then count
                  else loop (count+1) (c `shiftR` 1)

sobolRec :: Int -> [Word32] -> Word32 -> Word32
sobolRec ix dirVec e = e `xor` (dirVec !! (ffz ix))

-- Creates part of a sobol sequences using the given direction vector
-- creates `n` elements starting at index `startIx`
sobolRec1D :: Int -> Int -> [Word32] -> [Word32]
sobolRec1D startIx n dirVec = 
 let init = sobolInd startIx dirVec
     recurse _ _ 0 = [] -- This can be written as a sequential scan
     recurse i e n = e : recurse (i+1) (sobolRec i dirVec e) (n-1)
 in recurse startIx init n

-- Creates a length n sobol sequences using the given direction vector
sobolRec1DChunked :: Int -> Int -> [Word32] -> [Word32]
sobolRec1DChunked chunkSize n dirVec = 
  concatMap (\i -> sobolRec1D i chunkSize dirVec) [0,chunkSize..n-1]

-- Creates multi-dimensional sobol-sequences of length n
-- with as many dimensions as there are direction vectors
sobolRecND :: Int -> [[Word32]] -> [[Word32]]
sobolRecND n dirVs = 
 let init = map (sobolInd 0) dirVs
     recurse i e | i == n    = []
                 | otherwise = e : recurse (i+1) (zipWith (sobolRec i) dirVs e)
 in recurse 0 init

---------------------------------------------------------
-- Skipping algorithm for GPUs (Bradley, Giles et al.) --
---------------------------------------------------------
--
-- TODO

sobolSkipping = error "Not implemented yet"

--------------------
-- Monte carlo Pi --
--------------------
-- Converts a sobol number to floating point in the interval [0,1]
normalise :: Word32 -> Double
normalise x = fromIntegral x / sobol_divisor

normaliseND :: [[Word32]] -> [[Double]]
normaliseND = map (map normalise)

mcpi :: (Int -> [[Word32]] -> [[Word32]]) -> Int -> Double
mcpi sobol n = 
  let -- Make two dimensional Sobol-sequence
      xs = normaliseND $ sobol n sobol_dirVs

      -- Compute distance to centrum
      dist a b = sqrt (a*a + b*b)
      distances = map (\ [a,b] -> fromIntegral $ truncate (dist a b)) xs

      m = fromIntegral n
  in 4 * ((m - sum distances) / m)

mcpi_independent = mcpi sobolND
mcpi_recursive = mcpi sobolRecND
