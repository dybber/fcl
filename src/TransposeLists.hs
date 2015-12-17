module TransposeLists where

import Data.List (sortBy)
import Data.Ord (comparing)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f ls = sortBy (comparing f) ls

a |> f = f a

data Matrix a = Matrix (Int,Int) [a]
 deriving Show
toList :: Matrix a -> [a]
toList (Matrix _ elems) = elems

ppList :: Show a => [a] -> String
ppList [] = ""
ppList (x:xs) = show x ++ " " ++ ppList xs

-- instance Show a => Show (Matrix a) where
--   show (Matrix (0,cols) elems) = ""
--   show (Matrix (rows,cols) elems) =
--     ppList (take cols elems)
--     ++ "\n"
--     ++ show (Matrix (rows-1, cols) (drop cols elems))

mat33 = Matrix (3,3) [0..8]
mat44 = Matrix (4,4) [1..16]
mat99 = Matrix (9,9) [0..9*9]

splitGrid :: Matrix a -> Int -> Matrix (Matrix a)
splitGrid (Matrix (rows,cols) elems) splitSize =
  let tileSize = splitSize*splitSize
      n = rows `div` splitSize
      m = cols `div` splitSize
      mkTile p q = [ elems !! (p*m*tileSize + q*splitSize + m*splitSize*i + j)
                   | i <- [0..splitSize-1]
                   , j <- [0..splitSize-1]]
  in Matrix (n,m)
       [ Matrix (splitSize, splitSize) (mkTile p q)
         | p <- [0 .. n-1]
         , q <- [0 .. m-1]]

concatGrid :: Int -> Matrix (Matrix a) -> Matrix a
concatGrid splitSize (Matrix (rows,cols) elems) =
  let n = rows * splitSize
      m = cols * splitSize
  in Matrix (n,m)
       [ vs !! (i * splitSize + j)
         | p <- [0 .. n-1] -- row in result
         , q <- [0 .. m-1] -- col in result
         , let r = p `div` splitSize -- row in outer
               c = q `div` splitSize -- col in outer
         , let Matrix _ vs = elems !! (r * splitSize + c)
         , let i = p `mod` splitSize   -- row in inner
               j = q `mod` splitSize ] -- col in inner

-- reorder :: Int -> Matrix (Matrix a) -> Matrix (Matrix a)
-- reorder splitSize (Matrix (rows,cols) elems) =
--   let n = rows * splitSize
--       m = cols * splitSize
--   in Matrix (n,m)
--        [ vs !! (i * splitSize + j)
--          | p <- [0 .. n-1]
--          , q <- [0 .. m-1]
--          , let r = p `div` splitSize
--                c = q `div` splitSize
--          , let Matrix _ vs = elems !! (r * splitSize + c)
--          , let i = p `mod` splitSize
--                j = q `mod` splitSize ]


transposeMat :: Matrix a -> Matrix a
transposeMat (Matrix (rows, cols) elems) =
  Matrix (cols,rows)
    [elems !! (j*rows + i)
      | i <- [0..rows-1]
      , j <- [0..cols-1]]

transposeChunked mat splitSize =
  let Matrix (n,m) mats = splitGrid mat splitSize
  in Matrix (n,m) (Prelude.map transposeMat mats)
       |> transposeMat
       |> concatGrid splitSize

assemble :: (Int -> Int -> Int) -> [[Int]] -> [Int]
assemble f array =
  let buildAssocList _ _ [] = []
      buildAssocList i _ ([]:xs) = buildAssocList (i+1) 0 xs
      buildAssocList i j ((y:ys):xs) =
        (f i j, y) : buildAssocList i (j+1) (ys:xs)
  in Prelude.map Prelude.snd (sortOn Prelude.fst (buildAssocList 0 0 array))

reassembleChunks :: Int -> Int -> [[Int]] -> [Int]
reassembleChunks n splitSize array =
  let tileSize = splitSize*splitSize
      reorder i j =   (i `div` n)*n*tileSize
                    + (j `div` splitSize)*n*splitSize
                    + (i `mod` n)*splitSize
                    + j `mod` splitSize
  in assemble reorder array


-- transposeChunked2 mat =
--   let Matrix (n,m) mats = splitGrid mat 3
--   in Matrix (n,m) (Prelude.map transposeMat mats)
--        |> transposeMat
--        |> reassembleChunks n 3
