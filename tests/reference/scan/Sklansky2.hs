-- Improved sklansky scan from Obsidian
-- https://github.com/svenssonjoel/Obsidian/blob/master/Benchmarks/ScanBench/Scan.hs#L79
---- without conditional resulting from "append" of pull arrays in fan
---- with 2 elements per thread

import Data.Bits
import Data.Word

insertZero :: Int -> Word32 -> Word32
insertZero 0 a = a `shiftL` 1
insertZero i a = a + zeroOutBits i a

zeroOutBits :: Int -> Word32 -> Word32
zeroOutBits i a = a .&. fromIntegral (complement (oneBits i :: Word32))

flipBit :: (Num a, Bits a) => Int -> a -> a
flipBit i = (`xor` (1 `shiftL` i))

oneBits :: (Num a, Bits a) => Int -> a
oneBits i = (2^i) - 1



-- -- i determines iteration number
-- -- tid determines the thread ID 
-- -- ix1 is the index of
-- phase :: Int -> (a -> a -> a) -> Pull Word32 a -> Push Block Word32 a
-- phase i f arr =
--   mkPush l (\wf -> forAll sl2 (\tid -> -- for loop over half as many threads as array elements
--   do
--     let ix1 = insertZero i tid -- 
--         ix2 = flipBit i ix1
--         ix3 = zeroOutBits i ix2 - 1
--     wf (arr ! ix1) ix1                 -- pass first value through
--     wf (f (arr ! ix3) (arr ! ix2) ) ix2)) -- combine two values
--   where
--     l = length arr
--     l2 = l `div` 2
--     sl2 = fromIntegral l2



-- --------

-- sklansky2 :: Data a => Int -> (a -> a -> a) -> Pull Word32 a -> Program Block (Push Block Word32 a)
-- sklansky2 l f = compose [phase i f | i <- [0..(l-1)]]
  
-- compose :: Data a
--            => [Pull Word32 a -> Push Block Word32 a] 
--            -> Pull Word32 a
--            -> Program Block (Push Block Word32 a)
-- compose [f] arr = return $ f arr
-- compose (f:fs) arr = compose fs =<< compute (f arr)
