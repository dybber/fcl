module Language.FCL.IL.Example where

import Language.FCL.IL.Syntax
import Language.FCL.IL.Compile

blockSize_ :: Name
blockSize_ = ("blockSize", IntT)

loop :: ILProgram
loop =
  let tid = ("tid", IntT)
      inputArray = ("input", ArrayT IntT)
      outputArray = ("output", ArrayT IntT)
  in 
    [ParFor Block tid (EVar blockSize_)
       [AssignSub ],
     Synchronize
    ]

reduce :: ILProgram
reduce =
 let n = ("n", IntT)
     bid = ("bid", IntT)
     num_blocks = ("num_blocks", IntT)
     inputArray = ("input", ArrayT IntT)
     outputArray = ("output", ArrayT IntT)
 in [ReadIntCSV inputArray (EString "input.csv"),
     Declare n (EInt 1000),
     Declare blockSize_ (EInt 256),
     Declare num_blocks (EBinOp DivI (EBinOp AddI (EVar n) (EBinOp SubI (EBinOp MulI (EVar blockSize_) (EInt 2)) (EInt 1)))
                         (EBinOp MulI (EVar blockSize_) (EInt 2))),
     Alloc outputArray IntT (EVar num_blocks),
     Distribute Block bid (EVar num_blocks) loop,
     PrintIntArray (EVar num_blocks) (EVar outputArray),
     Distribute Block bid (EInt 1) loop,
     PrintIntArray (EInt 1) (EVar outputArray)
    ]



test :: IO ()
test =
  let (hostcode, kernelcode) = compile reduce
  in do putStrLn kernelcode
        putStrLn hostcode

main :: IO ()
main = compileAndOutput reduce "."
