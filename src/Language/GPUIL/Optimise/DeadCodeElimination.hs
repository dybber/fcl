module Language.GPUIL.Optimise.DeadCodeElimination
 (deadCodeElimination)
where

import Language.GPUIL.Analysis (Label)
import Language.GPUIL.Syntax

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- Remember to check that writes to pointers are not removed

deadCodeElimination :: [Statement Label]
                    -> Map Label (Set VarName)
                    -> [Statement Label]
deadCodeElimination stmts liveOutMap =
  let
    canElim :: Label -> VarName -> Bool
    canElim lbl var =
      case Map.lookup lbl liveOutMap of
        Just liveVars -> Set.member var liveVars
        Nothing -> error ("No liveness information for this statement: " ++ show lbl)

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
                    ForAll lvl v e ss lbl -> ForAll lvl v e (filterStmt p ss) lbl
                    DistrPar lvl v e ss lbl -> DistrPar lvl v e (filterStmt p ss) lbl
                    SeqWhile e ss lbl -> SeqWhile e (filterStmt p ss) lbl
                    If e ss0 ss1 lbl -> If e (filterStmt p ss0) (filterStmt p ss1) lbl
                    ss' -> ss'
      in stmt' : filterStmt p rest
                    

    -- -- replace variables if they are constant
    -- rep :: Label -> IExp -> IExp
    -- rep lbl e@(VarE v) =
    --   case isConstant v lbl of
    --     Nothing -> e
    --     Just e' -> e'
    -- rep lbl (UnaryOpE op e0) = UnaryOpE op (rep lbl e0)
    -- rep lbl (BinOpE op e0 e1) = BinOpE op (rep lbl e0) (rep lbl e1)
    -- rep lbl (IfE e0 e1 e2) = IfE (rep lbl e0) (rep lbl e1) (rep lbl e2)
    -- rep lbl (IndexE v e) = IndexE v (rep lbl e)
    -- rep lbl (CastE v e) = CastE v (rep lbl e)
    -- rep _ e = e
