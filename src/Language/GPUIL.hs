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
import Language.GPUIL.SimpleAllocator (memoryMap, MemMap, Bytes)

import Data.Map (assocs)

generateKernel :: String -> Program () -> Kernel NoType
generateKernel name m =
  let (stmts, params, varCount) = runProgram m
      (stmts', _) = convert varCount stmts
      stmts'' = optimise stmts
      (memmap, used) = memoryMap stmts''
      (smem_param, smem_decl) = addSharedMem memmap used
  in Kernel { kernelName = name
            , kernelParams = smem_param ++ params
            , kernelBody = optimise (smem_decl ++ stmts'')
            , kernelSharedMem = fmap optimiseExp used
            }

addSharedMem :: MemMap -> Maybe Bytes -> ([VarName], Statements () NoType)
addSharedMem _ Nothing = ([], [])
addSharedMem memmap _ =
  let
    declarations = zip (map declareArray (reverse $ assocs memmap)) (repeat ())
    declareArray :: (VarName, (Bytes, CType)) -> Statement () NoType
    declareArray (name, (offset, ty)) =
      Decl name
           (Just (CastE ty (BinOpE AddI (VarE ("sbase", CWord8) NoType) offset)))
  in ([("sbase", CPtr [attrLocal] CWord8)]
     , declarations)

-- Defaulting to 4 spaces of indentation
renderKernel :: Kernel ty -> String
renderKernel kernel = (render 0 4 (ppKernel kernel))
