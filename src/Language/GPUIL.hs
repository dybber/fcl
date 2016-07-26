module Language.GPUIL
(
  module Language.GPUIL.Cons,
  CType(..), generateKernel, renderKernel
)
where

import Language.GPUIL.Cons
import Language.GPUIL.Syntax

import Language.GPUIL.PrettyLib (render)
import Language.GPUIL.PrettyOpenCL (ppKernel)
import Language.GPUIL.Optimise (optimise, optimiseExp)
import Language.GPUIL.SimpleAllocator (memoryMap, Bytes)

--import Language.GPUIL.Analysis.TypeChecker (typeCheck, Status(..))


-- replace static dyn block-size with static blockSize
staticBlockSize :: Maybe Int -> Int -> [Statement a] -> [Statement a]
staticBlockSize blockSize warpSize_ stmts = map replaceSS stmts
  where
    replace :: IExp -> IExp
    replace LocalSize         = case blockSize of
                                  Nothing -> LocalSize
                                  Just n -> IntE n
    replace WarpSize          = IntE warpSize_
    replace (UnaryOpE op e0)  = UnaryOpE op (replace e0)
    replace (BinOpE op e0 e1) = BinOpE op (replace e0) (replace e1)
    replace (IfE e0 e1 e2)    = IfE (replace e0) (replace e1) (replace e2)
    replace (IndexE v e)      = IndexE v (replace e)
    replace (CastE v e)       = CastE v (replace e)
    replace e                   = e

    replaceSS (Assign v e lbl)          = Assign v (replace e) lbl
    replaceSS (Decl v e lbl)            = Decl v (replace e) lbl
    replaceSS (AssignSub v e0 e1 lbl)   = AssignSub v (replace e0) (replace e1) lbl
    replaceSS (Allocate v e lbl)        = Allocate v (replace e) lbl
    replaceSS (For v e ss lbl)          = For v (replace e) (map replaceSS ss) lbl
    replaceSS (If e0 ss0 ss1 lbl)       = If (replace e0) (map replaceSS ss0) (map replaceSS ss1) lbl
    replaceSS (SeqWhile unroll e ss lbl)       = SeqWhile unroll e (map replaceSS ss) lbl
    replaceSS stmt                      = stmt



generateKernel :: Int -> String -> IL () -> Maybe Int -> Int -> Kernel
generateKernel optIterations name m blockSize warpSize_ =
  let (stmts, params, _) = runIL m
--  tc params stmts
      (stmts', used) = memoryMap (staticBlockSize blockSize warpSize_ stmts)
      params' = (addSharedMem used) ++ params
--  tc params' stmts'
      stmts'' = optimise optIterations stmts'
--  tc params' stmts'''
  in Kernel { kernelName = name
            , kernelParams = params'
            , kernelBody = removeLabels stmts''
            , kernelSharedMem = fmap optimiseExp used
            , kernelBlockSize = blockSize
            , kernelWarpSize = warpSize_
            }

-- tc :: [VarName] -> [Statement a] -> ()
-- tc params stmts =
--   case typeCheck params stmts of
--     Success   -> ()
--     Error msg -> error msg

addSharedMem :: Maybe Bytes -> [VarName]
addSharedMem Nothing = []
addSharedMem _ = [("sbase", CPtr [attrLocal] CWord8)]

-- Defaulting to 4 spaces of indentation
renderKernel :: Kernel -> String
renderKernel kernel = (render 0 4 (ppKernel kernel))
