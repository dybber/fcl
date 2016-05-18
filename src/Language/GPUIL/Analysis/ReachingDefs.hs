module Language.GPUIL.Analysis.ReachingDefs
 (reach)
where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Language.GPUIL.Analysis.Dataflow
import Language.GPUIL.Analysis.Graph

import Language.GPUIL.Syntax

--------------------------
-- Reaching definitions --
--------------------------
unionMaps :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
unionMaps = Map.unionWith (Set.union)

singleton :: k -> a -> Map k (Set a)
singleton v lbl = Map.singleton v (Set.singleton lbl)

type DefMap = VarName -> Set (Label, Maybe IExp)

lkup :: Ord a => a -> Map a (Set b) -> Set b
lkup x m =
  case Map.lookup x m of
    Just x'  -> x'
    Nothing -> Set.empty

buildDefsMap :: [Statement Label] -> DefMap
buildDefsMap stmt x =
  let
    go :: Statement Label -> Map.Map VarName (Set (Label, Maybe IExp))
    go (For v _ ss lbl)        = foldl unionMaps (singleton v (lbl, Nothing)) (map go ss)
    go (If _ ss0 ss1 _)        = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (SeqWhile _ ss _)       = foldl unionMaps Map.empty (map go ss)
    go (Assign v e lbl)        = singleton v (lbl, Just e)
    go (Decl v e lbl)          = singleton v (lbl, Just e)
    go _                       = Map.empty

    -- build the map
    defMap = foldl unionMaps Map.empty (map go stmt)
  in lkup x defMap

type FlowMap = Map Label (Set Label)

-- gens and kills for reaching definitions
gensReachDef :: [Statement Label] -> FlowMap
gensReachDef stmts = foldl unionMaps Map.empty (map go stmts)
  where
    go (For _ _ ss lbl)        = foldl unionMaps (singleton lbl lbl) (map go ss)
    go (If _ ss0 ss1 _)        = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (SeqWhile _ ss _)       = foldl unionMaps Map.empty (map go ss)
    go (Assign _ _ lbl)        = singleton lbl lbl
    go (Decl _ _ lbl)          = singleton lbl lbl
    go _                       = Map.empty

killsReachDef :: DefMap -> [Statement Label] -> FlowMap
killsReachDef defs stmts = foldl unionMaps Map.empty (map go stmts)
  where
    killSet v lbl = (Map.singleton lbl (Set.delete lbl (Set.map fst $ defs v)))
    go (For v _ ss lbl)        = foldl unionMaps (killSet v lbl) (map go ss)
    go (If _ ss0 ss1 _)        = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (SeqWhile _ ss _)       = foldl unionMaps Map.empty (map go ss)
    go (Assign v _ lbl)        = killSet v lbl
    go (Decl v _ lbl)          = killSet v lbl
    go _                       = Map.empty

reach :: [Statement Label]
      -> Graph Label
      -> (VarName -> Set (Label, Maybe IExp),
          Map Label (Set Label),
          Map Label (Set Label))
reach stmts graph =
  let
    defs = buildDefsMap stmts
    gen = gensReachDef stmts
    kill = killsReachDef defs stmts

    -- Update in set to be union of all out-sets of predecessors
    updateIn outMap n =
      Set.fold (\p s -> Set.union s (lkup p outMap)) Set.empty (predecessors n graph)

    -- Update outset by removing killset and adding gen
    updateOut inMap n =
      (lkup n gen) `Set.union` (lkup n inMap `Set.difference` lkup n kill)

    (in', out') = forwardAnalysis updateIn updateOut graph
  in
    (defs, in', out')

-- λ> inMap
-- fromList [(0,fromList []),(1,fromList []),(2,fromList [1]),(3,fromList [2]),(4,fromList [3])]
-- λ> outMap
-- fromList [(0,fromList []),(1,fromList [1]),(2,fromList [2]),(3,fromList [3]),(4,fromList [4])]

-- ss0 = addLabels  [Decl ("a",CInt32) (IntE 0) (),
--                   Decl ("b",CInt32) (IntE 512) (),
--                   Decl ("c",CInt32) (VarE ("a",CInt32)) (),
--                   Decl ("a",CInt32) (IntE 17) ()]
-- defs = buildDefsMap ss0

-- gen = gensReachDef ss0
-- kill = killsReachDef defs ss0
