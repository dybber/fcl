module Language.CGen.Optimise.CopyPropagation
  (copyProp)
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.CGen.Analysis (Label)
import Language.CGen.Syntax

copyProp :: [Statement Label]
         -> Map Label (Set Label)
         -> (VarName -> Set (Label, Maybe IExp))
         -> [Statement Label]
copyProp stmts inSet defs =
  let
    onlyDefinedOnce :: VarName -> Label -> Maybe IExp
    onlyDefinedOnce t lbl =
      let definitions :: Set (Label, Maybe IExp)
          definitions = defs t
          reaches :: [Label]
          reaches = case Map.lookup lbl inSet of
                      Just in_ -> Set.toList (Set.intersection (Set.map fst definitions) in_)
                      Nothing -> []
      in case reaches of
           [] -> Nothing
           [x] -> case lookup x (Set.toList definitions) of
                    Nothing -> Nothing
                    Just e -> e
           _ -> Nothing

    -- replace variables if they are only defined once before
    rep :: Label -> IExp -> IExp
    rep lbl e@(VarE t) =
      case onlyDefinedOnce t lbl of
        Nothing -> e
        Just e' | isVar e' -> e'
        _ -> e
    rep lbl (UnaryOpE op e0)  = UnaryOpE op (rep lbl e0)
    rep lbl (BinOpE op e0 e1) = BinOpE op (rep lbl e0) (rep lbl e1)
    rep lbl (IfE e0 e1 e2)    = IfE (rep lbl e0) (rep lbl e1) (rep lbl e2)
    rep lbl (IndexE v e)      = IndexE v (rep lbl e)
    rep lbl (CastE v e)       = CastE v (rep lbl e)
    rep _ e                   = e
    
    prop (Assign v e lbl)          = Assign v (rep lbl e) lbl
    prop (Decl v e lbl)            = Decl v (rep lbl e) lbl
    prop (AssignSub v e0 e1 lbl)   = AssignSub v (rep lbl e0) (rep lbl e1) lbl
    prop (Allocate v e lbl)        = Allocate v (rep lbl e) lbl
    prop (For v e ss lbl)          = For v (rep lbl e) (map prop ss) lbl
    prop (If e0 ss0 ss1 lbl)       = If (rep lbl e0) (map prop ss0) (map prop ss1) lbl
    prop (While unroll e ss lbl)   = While unroll e (map prop ss) lbl
    prop stmt                      = stmt
    
  in map prop stmts
