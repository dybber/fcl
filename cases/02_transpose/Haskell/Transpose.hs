module Transpose where

import Data.List (sortBy)
import Data.Ord (comparing)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f ls = sortBy (comparing f) ls

generate :: Int -> (Int -> a) -> [a]
generate n f = map f [0..n-1]

transpose :: Int -> Int -> [a] -> [a]
transpose rows cols elems =
  generate (rows * cols)
           (\n ->
              let i = n `div` rows
                  j = n `mod` rows
              in elems !! (j * rows + i))

-- Split a 2D array (represented as a flat vector) into a 2D array of 2D arrays
-- First argument @splitSize is width and height of tiles
-- Second and third argument is the width and height of the original array
splitGrid :: Int -> Int -> Int -> [a] -> [[a]]
splitGrid splitSize rows cols elems =
  let tileSize = splitSize * splitSize
      n = rows `div` splitSize -- height of outer array
      m = cols `div` splitSize -- width of outer array
      mkTile p q =
        generate tileSize
                      (\k ->
                         let i = k `div` splitSize -- row in inner array
                             j = k `mod` splitSize -- column in inner array
                         in elems !! (p * m * tileSize
                                      + q * splitSize
                                      + m * splitSize * i
                                      + j))
  in generate (n * m)
       (\i ->
          let p = i `div` n -- row in outer
              q = i `mod` n -- column in outer
          in mkTile p q)


-----------------------------
-- Concat grid w. assemble --
-----------------------------
assemble :: ((Int, Int) -> Int) -> [[a]] -> [a]
assemble f array =
  let buildAssocList _ _ [] = []
      buildAssocList i _ ([]:xs) = buildAssocList (i+1) 0 xs
      buildAssocList i j ((y:ys):xs) =
        (f (i,j), y) : buildAssocList i (j+1) (ys:xs)
  in map snd (sortOn fst (buildAssocList 0 0 array))

-- Concatenate a 2D array of 2D arrays
-- First argument @splitSize is the width and height of inner arrays
-- Second argument @cols is the width and height of the outer array
-- Total number of elements is thus splitSize*splitSize*cols*cols
concatGrid :: Int -> Int -> [[a]] -> [a]
concatGrid splitSize  -- width&height of inner array (2)
               cols   -- width&height of outer array (2)
               arr =
    let tileSize = splitSize * splitSize -- 4
        mkGrid (i,j) =
             let outerRow = i `div` cols
                 outerCol = i `mod` cols
                 innerRow = j `div` splitSize
                 innerCol = j `mod` splitSize
             in outerRow * cols * tileSize  -- skip complete tiles (outerRow * 8)
                + innerRow * cols * splitSize -- skip complete rows (innerRow * 4)
                + outerCol * splitSize        -- skip to the tile   (
                + innerCol                    -- skip to row in tile
    in assemble mkGrid arr

-- Assemble is bad, as order of writes are dictated by the argument
-- function, and not assemble itself.
-- 

--------------------------------
-- Concat grid w. backpermute --
--------------------------------

backpermute :: (Int -> (Int, Int)) -> [[a]] -> [a]
backpermute f array =
  let n = sum (map length array)
  in generate n (\ix -> let (i,j) = f ix
                        in (array !! i) !! j)

concatGridBackpermute :: Int -> Int -> [[a]] -> [a]
concatGridBackpermute splitSize  -- width&height of inner array (2)
               cols   -- width&height of outer array (2)
               arr =
    let tileSize = splitSize * splitSize -- 4
        rowSize = tileSize * cols
        mkGrid i =
          let tileRow = i `div` rowSize
              tileCol = (i `div` splitSize) `mod` cols
              tileID = tileRow * cols + tileCol
              x = (i - (tileRow * rowSize))
              innerRow = x `div` (splitSize*cols)
              y = x - (innerRow * splitSize * cols)
              innerCol = y `mod` splitSize
          in (tileID, innerRow*splitSize + innerCol)
    in backpermute mkGrid arr

--------------------------------------------
-- Concat grid w. backpermute2 and concat --
--------------------------------------------

backpermute2 :: ((Int, Int) -> (Int, Int)) -> [[a]] -> [[a]]
backpermute2 f array =
  let n = length array
      m = length (head array)
  in generate m (\i ->
       generate n (\j -> let (i',j') = f (i,j)
                         in (array !! i') !! j'))

-- Transpose:
--  - split up in tiles
--  - transpose each tile
--  - force each tile to shared memory
--  - transpose outer array
--  - write back to global memory with concatGrid
transposeChunked :: Int -> Int -> Int -> [Int] -> [Int]
transposeChunked splitSize rows cols elems =
      (concatGridBackpermute
        splitSize
        (cols `div` splitSize))
        (transpose
           (rows `div` splitSize)
           (cols `div` splitSize)
           (map (transpose splitSize splitSize)
                (splitGrid splitSize rows cols elems)))

--------------------
-- Test transpose --
--------------------
inMatrix_4x4 :: [Int]
inMatrix_4x4 = [0..15]
--  0,  1,  2,  3
--  4,  5,  6,  7
--  8,  9, 10, 11
-- 12, 13, 14, 15

test4x4_basic, test4x4_chunked :: [Int]
test4x4_basic   = transpose 4 4 inMatrix_4x4
test4x4_chunked = transposeChunked 2 4 4 inMatrix_4x4
-- 0, 4,  8, 12
-- 1, 5,  9, 13
-- 2, 6, 10, 14
-- 3, 7, 11, 15

inMatrix_12x12 :: [Int]
inMatrix_12x12 = [0..143]
--   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11,
--  12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
--  24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
--  36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
--  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
--  60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71,
--  72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
--  84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
--  96, 97, 98, 99,100,101,102,103,104,105,106,107,
-- 108,109,110,111,112,113,114,115,116,117,118,119,
-- 120,121,122,123,124,125,126,127,128,129,130,131,
-- 132,133,134,135,136,137,138,139,140,141,142,143

test12x12_basic, test12x12_chunked :: [Int]
test12x12_basic = transpose 12 12 inMatrix_12x12
test12x12_chunked = transposeChunked 4 12 12 inMatrix_12x12
--  0, 12, 24, 36, 48, 60, 72, 84, 96,108,120,132,
--  1, 13, 25, 37, 49, 61, 73, 85, 97,109,121,133,
--  2, 14, 26, 38, 50, 62, 74, 86, 98,110,122,134,
--  3, 15, 27, 39, 51, 63, 75, 87, 99,111,123,135,
--  4, 16, 28, 40, 52, 64, 76, 88,100,112,124,136,
--  5, 17, 29, 41, 53, 65, 77, 89,101,113,125,137,
--  6, 18, 30, 42, 54, 66, 78, 90,102,114,126,138,
--  7, 19, 31, 43, 55, 67, 79, 91,103,115,127,139,
--  8, 20, 32, 44, 56, 68, 80, 92,104,116,128,140,
--  9, 21, 33, 45, 57, 69, 81, 93,105,117,129,141,
-- 10, 22, 34, 46, 58, 70, 82, 94,106,118,130,142,
-- 11, 23, 35, 47, 59, 71, 83, 95,107,119,131,143

test_all :: Bool
test_all =
  and [test4x4_basic == test4x4_chunked,
       test12x12_basic == test12x12_chunked]
