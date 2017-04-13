module FCL.IL.Analysis.ReachingDefs
 (reach)
where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import FCL.IL.Analysis.Dataflow
import FCL.IL.Analysis.Graph

import FCL.IL.Syntax

--------------------------
-- Reaching definitions --
--------------------------
unionMaps :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
unionMaps = Map.unionWith (Set.union)

singleton :: k -> a -> Map k (Set a)
singleton v lbl = Map.singleton v (Set.singleton lbl)

type DefMap = ILName -> Set (Label, Maybe ILExp)

lkup :: Ord a => a -> Map a (Set b) -> Set b
lkup x m =
  case Map.lookup x m of
    Just x'  -> x'
    Nothing -> Set.empty

buildDefsMap :: [Stmt Label] -> DefMap
buildDefsMap stmt x =
  let
    go :: Stmt Label -> Map.Map ILName (Set (Label, Maybe ILExp))
    go (SeqFor v _ ss lbl)       = foldl unionMaps (singleton v (lbl, Nothing)) (map go ss)
    go (ParFor _ v _ ss lbl)     = foldl unionMaps (singleton v (lbl, Nothing)) (map go ss)
    go (Distribute _ v _ ss lbl) = foldl unionMaps (singleton v (lbl, Nothing)) (map go ss)
    go (If _ ss0 ss1 _)          = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (While _ ss _)            = foldl unionMaps Map.empty (map go ss)
    go (Assign v e lbl)          = singleton v (lbl, Just e)
    go (Declare v _ e lbl)       = singleton v (lbl, Just e)

    -- new
    go (Alloc v _ _ lbl)         = singleton v (lbl, Nothing)
    go (ReadIntCSV v1 v2 _ lbl)  = unionMaps (singleton v1 (lbl, Nothing))
                                             (singleton v2 (lbl, Nothing))
    go (Benchmark _ ss _)        = foldl unionMaps Map.empty (map go ss)
    go (Synchronize _)           = Map.empty
    go (PrintIntArray _ _ _)     = Map.empty
    go (PrintDoubleArray _ _ _)     = Map.empty
    go (AssignSub _ _ _ _)       = Map.empty

    -- build the map
    defMap = foldl unionMaps Map.empty (map go stmt)
  in lkup x defMap

type FlowMap = Map Label (Set Label)

-- gens and kills for reaching definitions
gensReachDef :: [Stmt Label] -> FlowMap
gensReachDef stmts = foldl unionMaps Map.empty (map go stmts)
  where
    go (SeqFor _ _ ss lbl)       = foldl unionMaps (singleton lbl lbl) (map go ss)
    go (ParFor _ _ _ ss lbl)     = foldl unionMaps (singleton lbl lbl) (map go ss)
    go (Distribute _ _ _ ss lbl) = foldl unionMaps (singleton lbl lbl) (map go ss)
    go (If _ ss0 ss1 _)          = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (While _ ss _)            = foldl unionMaps Map.empty (map go ss)
    go (Assign _ _ lbl)          = singleton lbl lbl
    go (Declare _ _ _ lbl)       = singleton lbl lbl
--    go _                         = Map.empty

    go (Alloc _ _ _ lbl)         = singleton lbl lbl
    go (ReadIntCSV _ _ _ lbl)    = singleton lbl lbl
    go (Benchmark _ ss _)        = foldl unionMaps Map.empty (map go ss)
    go (Synchronize _)           = Map.empty
    go (PrintIntArray _ _ _)     = Map.empty
    go (PrintDoubleArray _ _ _)     = Map.empty
    go (AssignSub _ _ _ _)       = Map.empty


killsReachDef :: DefMap -> [Stmt Label] -> FlowMap
killsReachDef defs stmts = foldl unionMaps Map.empty (map go stmts)
  where
    killSet v lbl = (Map.singleton lbl (Set.delete lbl (Set.map fst $ defs v)))
    
    go (SeqFor v _ ss lbl) = foldl unionMaps (killSet v lbl) (map go ss)
    go (ParFor _ v _ ss lbl) = foldl unionMaps (killSet v lbl) (map go ss)
    go (Distribute _ v _ ss lbl) = foldl unionMaps (killSet v lbl) (map go ss)
    go (If _ ss0 ss1 _)    = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (While _ ss _)      = foldl unionMaps Map.empty (map go ss)
    go (Assign v _ lbl)    = killSet v lbl
    go (Declare v _ _ lbl) = killSet v lbl
--    go _                   = Map.empty

    go (Alloc v _ _ lbl)         = killSet v lbl
    go (ReadIntCSV v1 v2 _ lbl)  = unionMaps (killSet v1 lbl) (killSet v2 lbl)
    go (Benchmark _ ss _)        = foldl unionMaps Map.empty (map go ss)
    go (Synchronize _)           = Map.empty
    go (PrintIntArray _ _ _)     = Map.empty
    go (PrintDoubleArray _ _ _)     = Map.empty
    go (AssignSub _ _ _ _)       = Map.empty

reach :: [Stmt Label]
      -> Graph Label
      -> (ILName -> Set (Label, Maybe ILExp),
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
