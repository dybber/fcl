module Main where

import Language.FCL.Host.Program
import Language.FCL.Host.Syntax
import Language.FCL.ILKernel
import CGen

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

hostCode :: HostProgram
hostCode =
 let n = ("n", int32_t)
     inputArray = ("input", pointer_t [attrGlobal] int32_t)
     outputArray = ("output", pointer_t [attrGlobal] int32_t)
 in [Declare inputArray (EReadIntCSV (EString "input.csv")),
     Declare n (ELength (EVar inputArray)),
     Declare outputArray (EAlloc int32_t (EVar n)),
     DefKernel "reduce"  ([("out", pointer_t [attrGlobal] int32_t)],
                                 kernelBody inputArray n ("out", pointer_t [attrGlobal] int32_t)),
     Call "reduce" (EInt 1024) [outputArray],
     PrintArray (EVar n) (EVar outputArray)
    ]

test :: IO ()
test =
  let (hostcode, kernelcode) = compile hostCode
  in do putStrLn kernelcode
        putStrLn hostcode

main :: IO ()
main = compileAndOutput hostCode "."
