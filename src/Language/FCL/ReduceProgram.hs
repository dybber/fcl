module Language.FCL.ReduceProgram where

import Language.FCL.Program
import Language.FCL.ILKernel
import CGen

kernelBody :: VarName -> VarName -> VarName -> ILKernel ()
kernelBody inData n outData =
 let int :: Int -> CExp
     int = constant
     sdata = ("sdata", pointer_t [] int32_t)
 in
    do tid <- let_ "tid" int32_t localID
       i <- let_ "i" int32_t ((workgroupID `muli` (localSize `muli` (int 2))) `addi` localID)
       assignArray sdata (if_ (i `lti` var n) (inData ! i) (int 0)) tid
       iff ((i `addi` localSize) `lti` var n)
         (assignArray sdata ((sdata ! tid) `addd` (inData ! (i `addi` localSize))) tid,
          return ())
       syncLocal
       s <- letVar "s" int32_t (localSize `divi` int 2)
       whileUnroll 5 (var s `gti` (int 32)) $
         do iff (tid `lti` var s)
              (assignArray sdata (sdata ! (tid `addi` var s)) tid,
               return ())
            syncLocal
            s <== (var s `srl` int 1)
       iff (tid `eqi` int 0)
         (assignArray outData (sdata ! int 0) workgroupID,
          return ())

hostCode :: HostProgram ([VarName], ILKernel ())
hostCode =
 let n = "n"
     inputArray = "input"
     outputArray = "output"
 in [-- Assign n (IntScalar 1000),
     -- Assign inputArray (IntVector (map IntScalar [0..999])),
     Declare  n (EInt 1000),
     Declare  inputArray (EAlloc int32_t (EInt 1000)),
     Declare  outputArray (EAlloc int32_t (EInt 1000)),
     Declare "reduce"  (EKernel ([("out", pointer_t [] int32_t)],
                                 kernelBody (inputArray, pointer_t [] int32_t) (n, int32_t) ("out", pointer_t [] int32_t))),
     Call "reduce" (EInt 1000) [(outputArray, pointer_t [] int32_t)]
    ]

test :: IO () --(String, String)
test =
  let (hostcode, kernelcode) = compile initializeState hostCode
  in do putStrLn kernelcode
        putStrLn hostcode


-- TODO
---- generate and print code for Reduce example, too see what's missing
---- shared memory allocation

-- kernel :: ILKernel ()
-- kernel =
--   do sdata <- allocate double_t [] localSize
--      inData <- addParam "inData" (pointer_t [attrGlobal] double_t)
--      outData <- addParam "outData" (pointer_t [attrGlobal] double_t)
--      n <- addParam "n" uint32_t
--      kernelBody sdata inData outData n

-- kernel_gen :: TopLevel
-- kernel_gen = fst (generateKernel () 20 "reduce4" kernel)
