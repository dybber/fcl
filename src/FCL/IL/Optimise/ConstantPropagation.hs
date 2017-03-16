module FCL.IL.Optimise.ConstantPropagation
  (constantProp)
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import FCL.IL.Analysis (Label)
import FCL.IL.Syntax

constantProp :: [Stmt Label]
             -> Map Label (Set Label)
             -> (ILName -> Set (Label, Maybe ILExp))
             -> [Stmt Label]
constantProp stmts inSet defs =
  let
    traceVar :: ILName -> Label -> Maybe ILExp
    traceVar v lbl =
      let definitions :: Set (Label, Maybe ILExp)
          definitions = defs v
          reaches :: [Label]
          reaches = case Map.lookup lbl inSet of
                      Just t -> Set.toList (Set.intersection (Set.map fst definitions) t)
                      Nothing -> []
      in case reaches of
           [] -> Nothing
           [x] -> case lookup x (Set.toList definitions) of
                    Nothing -> Nothing
                    Just e -> e
           _ -> Nothing

    -- replace variables if they are constant
    rep :: Label -> ILExp -> ILExp
    rep lbl e@(EVar v) =
      case traceVar v lbl of
        Nothing -> e
        Just e' | isLiteral e' -> e'
        _ -> e
    rep lbl (EUnaryOp op e0)  = EUnaryOp op (rep lbl e0)
    rep lbl (EBinOp op e0 e1) = EBinOp op (rep lbl e0) (rep lbl e1)
    rep lbl (EIf e0 e1 e2)    = EIf (rep lbl e0) (rep lbl e1) (rep lbl e2)
    rep lbl (EIndex v e)      = EIndex v (rep lbl e)
    rep _ e                   = e

    
    prop (Assign v e lbl)          = Assign v (rep lbl e) lbl
    prop (Declare v ty e lbl)      = Declare v ty (rep lbl e) lbl
    prop (AssignSub v e0 e1 lbl)   = AssignSub v (rep lbl e0) (rep lbl e1) lbl
    prop (SeqFor v e ss lbl)       = SeqFor v (rep lbl e) (map prop ss) lbl
    prop (ParFor lvl v e ss lbl)   = ParFor lvl v (rep lbl e) (map prop ss) lbl
    prop (Distribute lvl v e ss lbl)   = Distribute lvl v (rep lbl e) (map prop ss) lbl
    prop (If e0 ss0 ss1 lbl)       = If (rep lbl e0) (map prop ss0) (map prop ss1) lbl
    prop (While e ss lbl)   = While e (map prop ss) lbl
    prop (Benchmark e ss lbl)   = Benchmark e (map prop ss) lbl
    prop (Alloc v ty e lbl) = Alloc v ty (rep lbl e) lbl
    prop (Synchronize lbl) = Synchronize lbl
    prop (ReadIntCSV v1 v2 e lbl) = ReadIntCSV v1 v2 (rep lbl e) lbl
    prop (PrintIntArray e1 e2 lbl) = PrintIntArray (rep lbl e1) (rep lbl e2) lbl
    
    
  in map prop stmts
