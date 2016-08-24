module Language.GPUIL.Optimise (optimise, optimiseExp) where

import Language.GPUIL.Optimise.ConstantFold (constantFold, foldExp)
import Language.GPUIL.Optimise.ConstantPropagation (constantProp)
import Language.GPUIL.Optimise.CopyPropagation (copyProp)
import Language.GPUIL.Optimise.DeadCodeElimination (deadCodeElimination)
import Language.GPUIL.Optimise.LoopUnroll (unroll)

import Language.GPUIL.Analysis
import Language.GPUIL.Syntax (Statement, IExp)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

optimise :: Int -> [Statement a] -> [Statement Label]
optimise n stmts =
  let stmts_labelled = addLabels stmts -- TODO this is unnecessary work, remove double computation
  in iterateN n optimise1 stmts_labelled

optimise1 :: [Statement Label] -> [Statement Label]
optimise1 stmts =
  let
      ss0 = addLabels stmts
      graph = makeFlowGraph ss0
      (defs, reachInMap, _) = reach ss0 graph
      ss1 = constantProp ss0 reachInMap defs
      ss2 = copyProp ss1 reachInMap defs
      ss3 = constantFold ss2
      ss4 = unroll ss3

      (_, liveOutMap) = liveness ss4 graph
      ss5 = deadCodeElimination ss4 liveOutMap
  in ss5

optimiseExp :: IExp -> IExp
optimiseExp = foldExp

