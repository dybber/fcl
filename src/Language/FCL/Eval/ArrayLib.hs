-- Simple array library using delay-arrays for FCL-interpreter
module Language.FCL.Eval.ArrayLib
 (FCLArray,
  sizeOf,
  indices,
  fromList, toList,
  fromFunction,
  index, mapA, foldlA,
  reduceA, joinA, splitA
 )
where

data FCLArray a = FCLArray { len :: Int
                           , idx :: Int -> a
                           }

instance Eq a => Eq (FCLArray a) where
  arr1 == arr2 =
    len arr1 == len arr2 &&
    and (zipWith (==) (map (idx arr1) [0..len arr1-1])
                      (map (idx arr2) [0..len arr2-1]))

instance Show a => Show (FCLArray a) where
  show = show . toList

sizeOf :: FCLArray a -> Int
sizeOf = len

indices :: FCLArray a -> [Int]
indices arr = [0..len arr-1]

fromList :: [a] -> FCLArray a
fromList ls =
  let n = length ls
  in FCLArray { len = n
              , idx = \i -> if i < n
                              then ls !! i
                              else error ("Array index out of bounds. N=" ++ show n ++ " got index " ++ show i)
              }

fromFunction :: Int -> (Int -> a) -> FCLArray a
fromFunction = FCLArray

index :: FCLArray a -> Int -> a
index (FCLArray {idx=f}) = f

toList :: FCLArray a -> [a]
toList (FCLArray {len=n,idx=f}) = map f [0..n-1]

mapA :: (a -> b) -> FCLArray a -> FCLArray b
mapA g (FCLArray {len=n, idx=f}) = FCLArray { len=n
                                            , idx = \i -> g (f i)
                                            }

foldlA :: ((a, b) -> a) -> a -> FCLArray b -> a
foldlA f b arr = foldl (curry f) b (toList arr)

reduceA :: ((a, a) -> a) -> a -> FCLArray a -> a
reduceA = foldlA

joinA :: FCLArray (FCLArray a) -> FCLArray a
joinA arr =
  if sizeOf arr == 0
  then FCLArray {len = 0, idx = undefined}
  else let w = sizeOf (index arr 0)
       in FCLArray { len = reduceA (uncurry (+)) 0 (mapA sizeOf arr)
                   , idx = \i -> if i == 0
                                 then idx (idx arr 0) 0
                                 else idx (idx arr (i `div` w)) (i `mod` w)
                   }

splitA :: Int -> FCLArray a -> FCLArray (FCLArray a)
splitA n arr =
  let total = len arr
      narrays = total `div` n
  in if total `mod` n /= 0
     then error "Cannot split evenly, does not divide"
     else FCLArray { len = narrays
                   , idx = \i -> FCLArray { len = n
                                          , idx = \j -> idx arr (i * n + j)
                                          }
                   }
