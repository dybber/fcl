
import Language.ObsidianLight
import Prelude hiding (map, splitAt, zipWith, concat, fst, snd, reverse)
import qualified Prelude
import Transpose

test_splitGrid :: Obs (Int -> Int -> Int -> [Int] -> [Int])
test_splitGrid =
  lam IntT (\splitSize ->
    lam IntT (\rows ->
      lam IntT (\cols ->
        lam (ArrayT Block IntT) (\elems ->
         concat (splitSize*splitSize) (splitGrid splitSize rows cols elems)))))

mkGrid :: Obs Int -> Obs Int -> Obs Int -> Obs [[Int]]
mkGrid splitSize rows cols =
  let tileSize, n, m :: Obs Int
      tileSize = splitSize*splitSize
      n = rows `divi` splitSize -- height of outer array
      m = cols `divi` splitSize -- width of outer array

      mkTile :: Obs Int -> Obs [Int]
      mkTile i =
        generate Block (splitSize*splitSize)
           (lam IntT (\_ -> i))
  in generate Block (n*m)
       (lam IntT (\i -> mkTile i))

test_mkGrid :: Obs (Int -> Int -> Int -> [Int])
test_mkGrid =
  lam IntT (\splitSize ->
    lam IntT (\rows ->
      lam IntT (\cols -> concat (splitSize*splitSize) (mkGrid splitSize rows cols))))

test_concatGrid :: Obs (Int -> Int -> Int -> [Int])
test_concatGrid =
  lam IntT (\splitSize ->
    lam IntT (\rows ->
      lam IntT (\cols ->
         concatGrid `app` splitSize
                    `app` splitSize
                    `app` (mkGrid splitSize rows cols))))

test_transposeWithOrdinaryConcat :: Obs (Int -> Int -> Int -> [Int] -> [Int])
test_transposeWithOrdinaryConcat =
  lam IntT (\splitSize ->
    lam IntT (\rows ->
      lam IntT (\cols ->
        lam (ArrayT Block IntT) (\elems ->
         concat (splitSize*splitSize)
           ((transpose2 `app` (rows `divi` splitSize) `app` (cols `divi` splitSize))
            `app`
            (map (transpose `app` splitSize `app` splitSize)
             (splitUp (splitSize*splitSize) elems)))))))

test_transpose = compileAndPrint "transpose" transposeChunked
test_concatGrid1 = compileAndPrint "concatGrid" concatGrid
test_concatGrid2 = compileAndPrint "concatGrid2" test_concatGrid

toArray :: [Int] -> Obs [Int]
toArray = fromList . (Prelude.map constant)
t0 = eval (transpose `app` 8 `app` 8 `app` (toArray [1..64]))
t1 = eval (transposeChunked `app` 4 `app` 8 `app` 8 `app` (toArray [1..64]))
t2 = eval (test_splitGrid `app` 4 `app` 8 `app` 8 `app` (toArray [1..64]))
t30 = eval (test_mkGrid `app` 4 `app` 8 `app` 8)
t3 = eval (test_concatGrid `app` 4 `app` 8 `app` 8)
t4 = eval (test_transposeWithOrdinaryConcat `app` 4 `app` 8 `app` 8 `app` (toArray [1..64]))


s0 = eval (transpose `app` 4 `app` 4 `app` (toArray [1..16]))
s1 = eval (transposeChunked `app` 2 `app` 4 `app` 4 `app` (toArray [1..16]))
s2 = eval (test_splitGrid `app` 2 `app` 4 `app` 4 `app` (toArray [1..16]))
s30 = eval (test_mkGrid `app` 2 `app` 4 `app` 4)
s3 = eval (test_concatGrid `app` 2 `app` 4 `app` 4)
s4 = eval (test_transposeWithOrdinaryConcat `app` 2 `app` 4 `app` 4 `app` (toArray [1..16]))


-- Expected
-- [1,9,17,25,33,41,49,57,
--  2,10,18,26,34,42,50,58,
--  3,11,19,27,35,43,51,59,
--  4,12,20,28,36,44,52,60,
--  5,13,21,29,37,45,53,61,
--  6,14,22,30,38,46,54,62,
--  7,15,23,31,39,47,55,63,
--  8,16,24,32,40,48,56,64]

-- Actual
-- [1,9,17,25,33,41,49,57,
--  5,13,21,29,37,45,53,61,
--  2,10,18,26,34,42,50,58,
--  6,14,22,30,38,46,54,62,
--  3,11,19,27,35,43,51,59,
--  7,15,23,31,39,47,55,63,
--  4,12,20,28,36,44,52,60,
--  8,16,24,32,40,48,56,64]

-- In
-- [0,0,0,0,0,0,0,0,
--  0,0,0,0,0,0,0,0,
--  1,1,1,1,1,1,1,1,
--  1,1,1,1,1,1,1,1,
--  2,2,2,2,2,2,2,2,
--  2,2,2,2,2,2,2,2,
--  3,3,3,3,3,3,3,3,
--  3,3,3,3,3,3,3,3]

-- Out
-- [0,0,0,0,1,1,1,1,
--  2,2,2,2,3,3,3,3,
--  0,0,0,0,1,1,1,1,
--  2,2,2,2,3,3,3,3,
--  0,0,0,0,1,1,1,1,
--  2,2,2,2,3,3,3,3,
--  0,0,0,0,1,1,1,1,
--  2,2,2,2,3,3,3,3]
