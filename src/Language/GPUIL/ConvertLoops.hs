module Language.GPUIL.ConvertLoops where

import Language.GPUIL.Syntax

workgroupSize :: Int
workgroupSize = 512

warpSize :: Int
warpSize = 32

-- It would be nice if my Smart-constructors would be of any use here.
-- But it I must keep the same loop variable, the smart constructors
-- would create a new one.
--
-- Or maybe I could do an assignment inside the loop..? Later!
compileDistrPar :: Stmt ty -> [Stmt ty]
compileDistrPar (DistrPar Block i ub body) =
  let numWorkgroups = (UnaryOpE NumGroups (IntE 0))
      gid = (UnaryOpE GroupID (IntE 0))
      blocksQ = BinOpE DivI ub numWorkgroups
      blocksR = BinOpE ModI ub numWorkgroups
      bodyQ = body ++ [SyncLocalMem]
  in 
     [For i blocksQ bodyQ,
      If (BinOpE LtI gid blocksR) body [],
      SyncLocalMem
     ]
compileDistrPar (DistrPar Warp i (IntE n) body) =
  let numWarps = workgroupSize `div` warpSize
      tid = (UnaryOpE LocalID (IntE 0))
      warpsQ = IntE (n `div` numWarps)
      warpsR = IntE (n `mod` numWarps)
      bodyQ = body
  in
     (For i warpsQ bodyQ)
       : if n `mod` numWarps == 0
           then []
           else [If (BinOpE LtI (BinOpE DivI tid (IntE warpSize)) warpsR) body [],
                 SyncLocalMem]
compileDistrPar _ = error "DistrPar currently only implemented at warp and block level."
