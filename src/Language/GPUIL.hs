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

generateKernel :: Int -> String -> IL () -> Kernel
generateKernel optIterations name m =
  let (stmts, params, varCount) = runIL m
--  tc params stmts
      (stmts', used) = memoryMap stmts
      params' = (addSharedMem used) ++ params
--  tc params' stmts'
      stmts'' = optimise optIterations stmts'
--  tc params' stmts'''
  in Kernel { kernelName = name
            , kernelParams = params'
            , kernelBody = removeLabels stmts''
            , kernelSharedMem = fmap optimiseExp used
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
