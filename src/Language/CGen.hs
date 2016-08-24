module Language.CGen
(
  module Language.CGen.Cons,
  module Language.CGen.ConsGPU,
  CType(..), generateKernel, generateFunction,
  RenderMode(..), renderProgram
)
where

import Language.CGen.Cons hiding (addStmt)
import Language.CGen.ConsGPU
import Language.CGen.Syntax

import Language.CGen.PrettyLib (render)
import Language.CGen.PrettyC as PPC (ppProgram)
import qualified Language.CGen.PrettyCUDA as PPCUDA (ppProgram)
import qualified Language.CGen.PrettyOpenCL as PPOpenCL (ppProgram)
import Language.CGen.Optimise (optimise)
import Language.CGen.SimpleAllocator (memoryMap, Bytes)

--import Language.CGen.Analysis.TypeChecker (typeCheck, Status(..))

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
    replaceSS (While unroll e ss lbl)   = While unroll e (map replaceSS ss) lbl
    replaceSS stmt                      = stmt

generateKernel :: Int -> String -> IL () -> Maybe Int -> Int -> Function
generateKernel optIterations name m blockSize warpSize_ =
  let (stmts, params, _) = runIL m
--  tc params stmts
      (stmts', used) = memoryMap (staticBlockSize blockSize warpSize_ stmts)
      params' = (addSharedMem used) ++ params
--  tc params' stmts'
      stmts'' = optimise optIterations stmts'
--  tc params' stmts'''
  in Function { funName = name
              , funParams = params'
              , funAttr = [IsKernel]
              , funBody = removeLabels stmts''
              , funReturnType = Nothing
            }

generateFunction :: Int -> String -> IL () -> Function
generateFunction optIterations name m =
  let (stmts, params, _) = runIL m
--  tc params stmts
      stmts' = optimise optIterations stmts
--  tc params' stmts'
  in Function { funName = name
              , funParams = params
              , funAttr = []
              , funBody = removeLabels stmts'
              , funReturnType = Nothing
            }

-- tc :: [VarName] -> [Statement a] -> ()
-- tc params stmts =
--   case typeCheck params stmts of
--     Success   -> ()
--     Error msg -> error msg

addSharedMem :: Maybe Bytes -> [VarName]
addSharedMem Nothing = []
addSharedMem _ = [("sbase", CPtr [attrLocal] CWord8)]

data RenderMode = C | OpenCL | CUDA

-- Defaulting to 4 spaces of indentation
renderProgram :: RenderMode -> [Function] -> String
renderProgram renderMode f =
  render 0 4 $
    case renderMode of
      C -> PPC.ppProgram f
      OpenCL -> PPOpenCL.ppProgram f
      CUDA -> PPCUDA.ppProgram f
