The most approachable description of Sobol generation algorithms is given by Bradley et al.
in their paper "Parallelisation Techniques for Random Number Generators"
https://people.maths.ox.ac.uk/gilesm/files/gems_rng.pdf

Lookout, they use both 0- and 1-indexing, which also have confused
themselves, leading to an indexing error in equation (4), it should
say $m_{q_n + 1}$.

Let's build a simple (non-efficient) module for generating
Sobol-numbers, just to illustrate the algorithms.

> module Sobol where

> import Data.Word
> import Data.Bits
> import Data.List (transpose)

To test our generators, we will generate two-dimensional sequences and
use them to calculate an estimate for $\pi$.

The basis for each sequence is a so-called direction vector. We are
going to use these two:

> sobol_dirVs :: [[Word32]]
> sobol_dirVs = [[536870912,268435456,134217728,67108864,33554432,
>                 16777216,8388608,4194304,2097152,1048576,524288,
>                 262144,131072,65536,32768,16384,8192,4096,2048,
>                 1024,512,256,128,64,32,16,8,4,2,1],
>                [536870912,805306368,671088640,1006632960,570425344,
>                 855638016,713031680,1069547520,538968064,808452096,
>                 673710080,1010565120,572653568,858980352,715816960,
>                 1073725440,536879104,805318656,671098880,1006648320,
>                 570434048,855651072,713042560,1069563840,538976288,
>                 808464432,673720360,1010580540,572662306,858993459]]

Each containing information to generate 30-bit quasi-random integers

> sobol_num_bits :: Int
> sobol_num_bits = 30

The efficient algorithms depends on encoding integers as their gray
code:

> grayCode :: Int -> Word32
> grayCode ix = fromIntegral (ix `xor` (ix `shiftR` 1))

And to avoid some conditionals, we can replace booleans by
multiplication with their integer representation:

> fromBool :: Num a => Bool -> a
> fromBool True = 1
> fromBool False = 0



Sobol independent formula
=========================

The independent formula for computing a sobol number. Computes the
Sobol number given the direction vector and an index of that number.

> sobolInd :: Int -> [Word32] -> Word32
> sobolInd ix dirVec = 
>   let bitVec :: [Word32]
>       bitVec = map (fromBool . testBit (grayCode ix))
>                    [0..sobol_num_bits-1]
>   in foldr xor 0 (zipWith (*) dirVec bitVec)

To create a complete 1-dimensional sequence, we just need to map over
the indices:

> sobolInd1D :: Int -> [Word32] -> [Word32]
> sobolInd1D n dirVec = map (\ix -> sobolInd ix dirVec) [0..n-1]

To create a multi-dimensional sequence, we also need to map over each
direction vector:

> sobolIndND :: Int -> [[Word32]] -> [[Word32]]
> sobolIndND n dirVs = map (\ix -> map (sobolInd ix) dirVs) [0..n-1]

Sobol recursive
===============

```ffz``` finds the index of the rightmost zero bit (find first zero)
> ffz :: Int -> Int
> ffz n = loop 0 n
>  where
>    loop count c
>      | c .&. 1 == 0 = count
>      | otherwise = loop (count+1) (c `shiftR` 1)

This is necessary to for the recursive Sobol-formula:

> sobolRec :: [Word32] -> Word32 -> Int -> Word32
> sobolRec dirVec previous ix = previous `xor` (dirVec !! (ffz ix))

To create 1-dimensional sequences, we now use the independent
formulate to initialize at a start index, and recursively computes the
rest of sequence.

> sobolRec1D :: Int -> Int -> [Word32] -> [Word32]
> sobolRec1D startIx n dirVs = 
>  let initial = sobolInd startIx dirVs
>  in scanl (sobolRec dirVs) initial [startIx..startIx+n-2]

Which can be used to chunk up the sequence and compute many seperate
parts in parallel (not really a parallel map here, but it could be!)

> sobolRec1DChunked :: Int -> Int -> [Word32] -> [Word32]
> sobolRec1DChunked chunkSize n dirVec = 
>   concat (map (\i -> sobolRec1D i chunkSize dirVec) [0,chunkSize..n-1])

And this can of course be mapped (again) over multiple dimensions of
the Sobol-sequence. The transpose here, is just to obtain the same
layout of the numbers as what we did in the independent formulation.

> sobolRecND :: Int -> [[Word32]] -> [[Word32]]
> sobolRecND n dirVs = transpose (map (sobolRec1D 0 n) dirVs)

Skipping algorithm for GPUs (Bradley, Giles et al.)
===================================================

> sobolSkip :: [Word32] -> Int -> Word32 -> Int -> Word32
> sobolSkip dirVec p prev n =
>   let q_n = ffz (n .|. (2^p - 1))
>   in prev
>       `xor` (dirVec !! (p - 1))
>       `xor` (dirVec !! q_n)

> sobolSkip1D :: Int -> Int -> [Word32] -> [Word32]
> sobolSkip1D logskip steps dirVec =
>   let stepSize = (2^logskip)
>       initial = sobolInd1D stepSize dirVec
>       skip prev start = zipWith (sobolSkip dirVec logskip) prev [start..start+stepSize-1]
>   in concat (scanl skip initial (map (*stepSize) [0..steps-2]))

> sobolSkipND :: Int -> Int -> [[Word32]] -> [[Word32]]
> sobolSkipND logskip steps dirVs =
>   transpose (map (sobolSkip1D logskip steps) dirVs)

> sobolSkipNDWrap :: Int -> Int -> [[Word32]] -> [[Word32]]
> sobolSkipNDWrap logskip n dirVs =
>   let stepSize = (2^logskip)
>       steps = (n + stepSize - 1) `div` stepSize
>   in take n (sobolSkipND logskip steps dirVs)

> sobolSkipping :: Int -> [[Word32]] -> [[Word32]]
> sobolSkipping = error "Not implemented yet"

Normalise to floating point [0;1]
=================================
> sobol_divisor :: Double
> sobol_divisor = fromIntegral ((2 :: Int) ^ sobol_num_bits)

Converts a sobol number to floating point in the interval [0,1]

> normalise :: Word32 -> Double
> normalise x = fromIntegral x / sobol_divisor

> normaliseND :: [[Word32]] -> [[Double]]
> normaliseND = map (map normalise)

Tests
=====
> iterations :: Int
> iterations = 50

> expectedOut :: [[Double]]
> expectedOut = [[0.0, 0.0], [0.5, 0.5], [0.75, 0.25], [0.25, 0.75], [0.375, 0.375],
>      [0.875, 0.875], [0.625, 0.125], [0.125, 0.625], [0.1875, 0.3125],
>      [0.6875, 0.8125], [0.9375, 0.0625], [0.4375, 0.5625], [0.3125, 0.1875],
>      [0.8125, 0.6875], [0.5625, 0.4375], [0.0625, 0.9375], [0.09375, 0.46875],
>      [0.59375, 0.96875], [0.84375, 0.21875], [0.34375, 0.71875],
>      [0.46875, 0.09375], [0.96875, 0.59375], [0.71875, 0.34375],
>      [0.21875, 0.84375], [0.15625, 0.15625], [0.65625, 0.65625],
>      [0.90625, 0.40625], [0.40625, 0.90625], [0.28125, 0.28125],
>      [0.78125, 0.78125], [0.53125, 0.03125], [0.03125, 0.53125],
>      [0.046875, 0.265625], [0.546875, 0.765625], [0.796875, 0.015625],
>      [0.296875, 0.515625], [0.421875, 0.140625], [0.921875, 0.640625],
>      [0.671875, 0.390625], [0.171875, 0.890625], [0.234375, 0.078125],
>      [0.734375, 0.578125], [0.984375, 0.328125], [0.484375, 0.828125],
>      [0.359375, 0.453125], [0.859375, 0.953125], [0.609375, 0.203125],
>      [0.109375, 0.703125], [0.078125, 0.234375], [0.578125, 0.734375]]

> testSobolIndND :: [[Double]]
> testSobolIndND = normaliseND (sobolIndND iterations sobol_dirVs)
> 
> testSobolRecND :: [[Double]]
> testSobolRecND = normaliseND (sobolRecND iterations sobol_dirVs)
> 
> testSobolSkipND :: [[Double]]
> testSobolSkipND = normaliseND (sobolSkipNDWrap 3 iterations sobol_dirVs)


Calculate pi w. Monte carlo integration
=======================================

To illustrate, we can use the numbers to do a small Monte Carlo
integration, computing $\pi$.

> mcpi :: (Int -> [[Word32]] -> [[Word32]]) -> Int -> Double
> mcpi sobol n = 
>   let -- Make two dimensional Sobol-sequence
>       xs = normaliseND (sobol n sobol_dirVs)
> 
>       -- Compute distance to centrum
>       dist a b = a*a + b*b
>       distances = map (\ [a,b] -> fromIntegral (truncate (dist a b) :: Int)) xs
> 
>       m = fromIntegral n
>   in 4 * ((m - sum distances) / m)

And we can now test it with each of our generators. (All of them are
pretty slow, when written in this style, using plain Haskell lists.)

> mcpi_independent :: Int -> Double
> mcpi_independent = mcpi sobolIndND

> mcpi_recursive :: Int -> Double
> mcpi_recursive = mcpi sobolRecND

> mcpi_skip :: Int -> Double
> mcpi_skip = mcpi (sobolSkipNDWrap 5)
