module Language.GPUIL.Optimise (optimise, optimiseExp) where

import Language.GPUIL.Optimise.ConstantFold (constantFold, foldExp)
import Language.GPUIL.Optimise.ConstantPropagation (constantProp)
import Language.GPUIL.Optimise.DeadCodeElimination (deadCodeElimination)

import Language.GPUIL.Analysis
import Language.GPUIL.Analysis.Graph (Graph)
import Language.GPUIL.Syntax (Statement, IExp)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

optimise :: Int -> [Statement a] -> [Statement Label]
optimise n stmts =
  let stmts_labelled = addLabels stmts
      graph = makeFlowGraph stmts_labelled
  in iterateN n (optimise1 graph) stmts_labelled

optimise1 :: Graph Label -> [Statement Label] -> [Statement Label]
optimise1 graph ss =
  let (defs, reachInMap, _) = reach ss graph
      ss' = constantProp ss reachInMap defs
      ss'' = constantFold ss'

      (_, liveOutMap) = liveness ss'' graph
      ss''' = deadCodeElimination ss'' liveOutMap
  in ss'''

optimiseExp :: IExp -> IExp
optimiseExp = foldExp

