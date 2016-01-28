module Transpose where

import Language.FCL
import Prelude hiding (map, splitAt, zipWith, concat, fst, snd, reverse)

transpose :: Obs (Int -> Int -> [Int] -> [Int])
transpose =
  lam IntT (\rows ->
    lam IntT (\cols ->
      lam (ArrayT Block IntT) (\elems ->
         generate Block (rows*cols)
           (lam IntT (\n -> let i = n `divi` rows
                                j = n `modi` rows
                            in elems ! (j*rows + i))))))

-- Polymorphism would be nice
-- This is the exact same as above, except for its type
transpose2 :: Obs (Int -> Int -> [[Int]] -> [[Int]])
transpose2 =
  lam IntT (\rows ->
    lam IntT (\cols ->
      lam (ArrayT Block (ArrayT Block IntT)) (\elems ->
         generate Block (rows*cols)
           (lam IntT (\n -> let i = n `divi` rows
                                j = n `modi` rows
                            in elems ! (j*rows + i))))))

-- Split a 2D array (represented as a flat vector) into a 2D array of 2D arrays
-- First argument @splitSize is width and height of tiles
-- Second and third argument is the width and height of the original array
splitGrid :: Obs Int -> Obs Int -> Obs Int -> Obs [Int] -> Obs [[Int]]
splitGrid splitSize rows cols elems  =
  let tileSize, n, m :: Obs Int
      tileSize = splitSize*splitSize
      n = rows `divi` splitSize -- height of outer array
      m = cols `divi` splitSize -- width of outer array

      mkTile :: Obs Int -> Obs Int -> Obs [Int]
      mkTile p q =
        generate Block tileSize
           (lam IntT (\k -> let i = k `divi` splitSize -- row in inner array
                                j = k `modi` splitSize -- column in inner array
                            in elems ! (p*m*tileSize + q*splitSize + m*splitSize*i + j)))
  in generate Block (n*m)
       (lam IntT (\i ->
          let p = i `divi` n -- row in outer
              q = i `modi` n -- column in outer
          in mkTile p q))

-- Concatenate a 2D array of 2D arrays
-- First argument @splitSize is the width and height of inner arrays
-- Second argument @cols is the width and height of the outer array
-- Total number of elements is thus splitSize*splitSize*cols*cols
concatGrid :: Obs (Int -> Int -> [[Int]] -> [Int])
concatGrid =
  (lam IntT (\splitSize -> -- width&height of inner array (2)
     (lam IntT (\cols ->   -- width&height of outer array (2)
       (lam (ArrayT Block (ArrayT Block IntT))
         (\arr ->
           lett (splitSize*splitSize) $ \tileSize ->
            assemble tileSize
             (lam (IntT :*: IntT) (\x ->
                   let p = fst x -- index into outer (0..3)
                       q = snd x -- index into inner (0..3)
                       outerRow = p `divi` cols
                       outerCol = p `modi` cols
                       innerRow = q `divi` splitSize 
                       innerCol = q `modi` splitSize
                   in outerRow * cols * tileSize    -- skip complete tiles
                      + innerRow * cols * splitSize -- skip complete rows
                      + outerCol * splitSize        -- skip to the tile
                      + innerCol                    -- skip to row in tile
                   )) arr))))))

-- Transpose:
--  - split up in tiles
--  - transpose each tile
--  - force each tile to shared memory
--  - transpose outer array
--  - write back to global memory with concatGrid
transposeChunked :: Obs (Int -> Int -> Int -> [Int] -> [Int])
transposeChunked =
  lam IntT (\splitSize ->
    lam IntT (\rows ->
      lam IntT (\cols ->
        lam (ArrayT Block IntT) (\elems ->
         (concatGrid `app` splitSize `app` (cols `divi` splitSize)) `app`
           ((transpose2 `app` (rows `divi` splitSize) `app` (cols `divi` splitSize))
            `app`
            ((map (lam (ArrayT Block IntT) (\arr -> force arr)))  -- force into shared memory
            (map (transpose `app` splitSize `app` splitSize)
             (splitGrid splitSize rows cols elems))))))))
