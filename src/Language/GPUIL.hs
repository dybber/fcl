module Language.GPUIL
(
  module Language.GPUIL.Cons,
  Level(..), CType(..), generateKernel, renderKernel
)
where

import Language.GPUIL.Cons
import Language.GPUIL.Syntax

import Language.GPUIL.ConvertLoops (convert)

import Language.GPUIL.PrettyLib (render)
import Language.GPUIL.PrettyOpenCL (ppKernel)
import Language.GPUIL.Optimise (optimise, optimiseExp)
import Language.GPUIL.SimpleAllocator (memoryMap, Bytes)

generateKernel :: String -> Program () -> Kernel NoType
generateKernel name m =
  let (stmts, params, varCount) = runProgram m
      (stmts', used) = memoryMap stmts
      (stmts'', _) = convert varCount stmts'
--      stmts'' = optimise stmts''
      smem_param = addSharedMem used
  in Kernel { kernelName = name
            , kernelParams = smem_param ++ params
            , kernelBody = stmts''
            , kernelSharedMem = fmap optimiseExp used
            }

addSharedMem :: Maybe Bytes -> ([VarName])
addSharedMem Nothing = []
addSharedMem _ = [("sbase", CPtr [attrLocal] CWord8)]

-- Defaulting to 4 spaces of indentation
renderKernel :: Kernel ty -> String
renderKernel kernel = (render 0 4 (ppKernel kernel))
