module FCL.IL.Optimise.DeadCodeElimination
 (deadCodeElimination)
where

import FCL.IL.Analysis (Label)
import FCL.IL.Syntax

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- TODO: Remember to check that writes to pointers are not removed

deadCodeElimination :: [Stmt Label]
                    -> Map Label (Set ILName)
                    -> [Stmt Label]
deadCodeElimination stmts liveOutMap =
  let
    canElim :: Label -> ILName -> Bool
    canElim lbl var =
      case Map.lookup lbl liveOutMap of
        Just liveVars -> not (Set.member var liveVars)
        Nothing -> error "No liveness information for this statement."

    -- elimStmt (Assign (_, ILArray _) _ _) = False
    -- elimStmt (Declare (_, ILArray _) _ _) = False
    elimStmt (Assign v _ lbl) = canElim lbl v
--    elimStmt (Declare v _ _ lbl) = canElim lbl v
    elimStmt _ = False

  in filterStmt elimStmt stmts

filterStmt :: (Stmt a -> Bool) -> [Stmt a] -> [Stmt a]
filterStmt _ [] = []
filterStmt p (stmt:rest)
  | p stmt = filterStmt p rest
  | otherwise =
      let stmt' = case stmt of
                    SeqFor v e ss lbl -> SeqFor v e (filterStmt p ss) lbl
                    While e ss lbl -> While e (filterStmt p ss) lbl
                    If e ss0 ss1 lbl -> If e (filterStmt p ss0) (filterStmt p ss1) lbl
                    ss' -> ss'
      in stmt' : filterStmt p rest
