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

import Language.GPUIL.Analysis.TypeChecker (typeCheck, Status(..))

generateKernel :: String -> Program () -> IO Kernel
generateKernel name m = do
  let (stmts, params, varCount) = runProgram m
  tc params stmts
  let (stmts', used) = memoryMap stmts
      params' = (addSharedMem used) ++ params
  tc params' stmts'
  let (stmts'', _) = convert varCount stmts'
  tc params' stmts''
  let stmts''' = optimise 10 stmts''
  tc params' stmts'''
  return (Kernel { kernelName = name
                 , kernelParams = params'
                 , kernelBody = removeLabels stmts'''
                 , kernelSharedMem = fmap optimiseExp used
                 })

tc :: [VarName] -> [Statement a] -> IO ()
tc params stmts =
  case typeCheck params stmts of
    Success   -> return ()
    Error msg -> putStrLn msg

addSharedMem :: Maybe Bytes -> ([VarName])
addSharedMem Nothing = []
addSharedMem _ = [("sbase", CPtr [attrLocal] CWord8)]

-- Defaulting to 4 spaces of indentation
renderKernel :: Kernel -> String
renderKernel kernel = (render 0 4 (ppKernel kernel))
