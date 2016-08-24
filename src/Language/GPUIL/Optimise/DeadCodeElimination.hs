module Language.GPUIL.Optimise.DeadCodeElimination
 (deadCodeElimination)
where

import Language.GPUIL.Analysis (Label)
import Language.GPUIL.Syntax

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- TODO: Remember to check that writes to pointers are not removed

deadCodeElimination :: [Statement Label]
                    -> Map Label (Set VarName)
                    -> [Statement Label]
deadCodeElimination stmts liveOutMap =
  let
    canElim :: Label -> VarName -> Bool
    canElim lbl var =
      case Map.lookup lbl liveOutMap of
        Just liveVars -> not (Set.member var liveVars)
        Nothing -> error "No liveness information for this statement."

    elimStmt (Assign (_, CPtr _ _) _ _) = False
    elimStmt (Decl (_, CPtr _ _) _ _) = False
    elimStmt (Assign v _ lbl) = canElim lbl v
    elimStmt (Decl v _ lbl) = canElim lbl v
    elimStmt _ = False

  in filterStmt elimStmt stmts

filterStmt :: (Statement a -> Bool) -> [Statement a] -> [Statement a]
filterStmt _ [] = []
filterStmt p (stmt:rest)
  | p stmt = filterStmt p rest
  | otherwise =
      let stmt' = case stmt of
                    For v e ss lbl -> For v e (filterStmt p ss) lbl
                    While unroll e ss lbl -> While unroll e (filterStmt p ss) lbl
                    If e ss0 ss1 lbl -> If e (filterStmt p ss0) (filterStmt p ss1) lbl
                    ss' -> ss'
      in stmt' : filterStmt p rest
