module Main where

import Language.FCL.Host.Program
import Language.FCL.Host.Syntax
import Language.FCL.ILKernel
import CGen

-- Input size: n
-- blockSize: variable
-- Num. blocks: (n+blockSize*2-1)/(blockSize*2)
-- Output size = num blocks
-- Shared memory need: blockSize
kernelBody :: VarName -> VarName -> VarName -> ILKernel ()
kernelBody inData n outData =
 let int :: Int -> CExp
     int = constant
     sdata = ("sdata", pointer_t [attrLocal] int32_t)
 in
    do tid <- let_ "tid" int32_t localID
       i <- let_ "i" int32_t ((workgroupID `muli` (localSize `muli` (int 2))) `addi` localID)
       assignArray sdata (if_ (i `lti` var n) (inData ! i) (int 0)) tid
       iff ((i `addi` localSize) `lti` var n)
         (assignArray sdata ((sdata ! tid) `addi` (inData ! (i `addi` localSize))) tid,
          return ())
       syncLocal
       s <- letVar "s" int32_t (localSize `divi` int 2)
       whileLoop (var s `gti` (int 0)) $
         do iff (tid `lti` var s)
              (assignArray sdata ((sdata ! tid) `addi` (sdata ! (tid `addi` var s))) tid,
               return ())
            syncLocal
            s <== (var s `srl` int 1)
       iff (tid `eqi` int 0)
         (assignArray outData (sdata ! int 0) workgroupID,
          return ())

reduceBody =
  let out = ("out", pointer_t [attrGlobal] int32_t)
      in_ = ("in", pointer_t [attrGlobal] int32_t)
      size = ("size", int32_t)
  in ([in_, size, out],
      kernelBody in_ size out)

blockSize :: Int
blockSize = 256

hostCode :: HostProgram
hostCode =
 let n = ("n", int32_t)
     blockSize_ = ("blockSize", int32_t)
     num_blocks = ("num_blocks", int32_t)
     inputArray = ("input", pointer_t [attrGlobal] int32_t)
     outputArray = ("output", pointer_t [attrGlobal] int32_t)
 in [Declare inputArray (EReadIntCSV (EString "input.csv")),
     Declare n (ELength (EVar inputArray)),
     Declare blockSize_ (EInt 256),
     Declare num_blocks (EBinOp DivI (EBinOp AddI (EVar n) (EBinOp SubI (EBinOp MulI (EVar blockSize_) (EInt 2)) (EInt 1))) (EBinOp MulI (EVar blockSize_) (EInt 2))),
     Declare outputArray (EAlloc int32_t (EVar num_blocks)),
     DefKernel "reduce"  reduceBody,
     Call "reduce" (EBinOp MulI (EVar num_blocks) (EVar blockSize_)) [inputArray, n, outputArray],
     PrintArray (EVar num_blocks) (EVar outputArray),
     Call "reduce" (EVar blockSize_) [outputArray, num_blocks, outputArray],
     PrintArray (EInt 1) (EVar outputArray)
    ]

test :: IO ()
test =
  let (hostcode, kernelcode) = compile hostCode
  in do putStrLn kernelcode
        putStrLn hostcode

main :: IO ()
main = compileAndOutput hostCode "."
