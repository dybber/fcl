module Language.GPUIL
(
  module Language.GPUIL.Cons,
  Level(..), CType(..), generateKernel, renderKernel
)
where

import Language.GPUIL.Cons
import Language.GPUIL.Syntax (Level (..), CType(..))

import Language.GPUIL.ConvertLoops (convert)

import Language.GPUIL.PrettyLib (render)
import Language.GPUIL.PrettyOpenCL (ppKernel)

generateKernel :: String -> Program () -> Kernel NoType
generateKernel name m =
  let (stmts, params, varCount) = runProgram m
      (stmts', _) = convert varCount stmts
  in Kernel { kernelName = name
            , kernelParams = params
            , kernelBody = stmts'
            }

-- Defaulting to 4 spaces of indentation
renderKernel :: Kernel ty -> String
renderKernel kernel = (render 0 4 (ppKernel kernel))
