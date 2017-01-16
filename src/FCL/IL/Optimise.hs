module FCL.IL.Optimise (optimise, optimiseExp) where

import FCL.IL.Optimise.ConstantFold (constantFold, foldExp)
import FCL.IL.Optimise.ConstantPropagation (constantProp)
import FCL.IL.Optimise.CopyPropagation (copyProp)
import FCL.IL.Optimise.DeadCodeElimination (deadCodeElimination)
import FCL.IL.Optimise.LoopUnroll (unroll)

import FCL.IL.Analysis
import FCL.IL.Syntax (Stmt, ILExp)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x = iterateN (n-1) f (f x)

optimise :: Int -> [Stmt a] -> [Stmt Label]
optimise n stmts =
  let stmts_labelled = addLabels stmts -- TODO this is unnecessary work, remove double computation
  in iterateN n optimise1 stmts_labelled

optimise1 :: [Stmt Label] -> [Stmt Label]
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

optimiseExp :: ILExp -> ILExp
optimiseExp = foldExp

