module FCL.IL.Analysis.Liveness (liveness, liveInExp) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import FCL.IL.Analysis.Dataflow
import FCL.IL.Analysis.Graph

import FCL.IL.Syntax

-- Collection of live variables
type LiveInfo = Set ILName

-- Arrays accessed in the given expression
liveInExp :: ILExp -> LiveInfo
liveInExp e =
  case e of
    EIndex name e0      -> Set.insert name (liveInExp e0)
    EVar name           -> Set.singleton name
    -- Recursive
    EUnaryOp _ e0       -> liveInExp e0
    EBinOp _ e0 e1      -> liveInExp e0 `Set.union` liveInExp e1
    EIf e0 e1 e2        -> liveInExp e0 `Set.union` liveInExp e1 `Set.union` liveInExp e2
    -- Scalars and constants
    EInt _              -> Set.empty
    EDouble _           -> Set.empty
    EBool _             -> Set.empty
    EString _           -> Set.empty

type LiveMap = Map Label (Set ILName)

expLiveMap :: Label -> [ILExp] -> LiveMap
expLiveMap lbl es = Map.singleton lbl (foldl Set.union Set.empty (map liveInExp es))

unionMaps :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b)
unionMaps = Map.unionWith (Set.union)

singleton :: k -> a -> Map k (Set a)
singleton lbl v = Map.singleton lbl (Set.singleton v)

-- gens and kills for reaching definitions
gensLiveness :: [Stmt Label] -> LiveMap
gensLiveness stmts = liveMany stmts
  where
    liveMany :: [Stmt Label] -> LiveMap
    liveMany ss = foldl unionMaps Map.empty (map go ss)
    
    go (SeqFor _ e ss lbl)       = unionMaps (expLiveMap lbl [e]) (liveMany ss)
    go (ParFor _ _ e ss lbl)     = unionMaps (expLiveMap lbl [e]) (liveMany ss)
    go (Distribute _ _ e ss lbl) = unionMaps (expLiveMap lbl [e]) (liveMany ss)
    go (If e ss0 ss1 lbl)        = unionMaps (expLiveMap lbl [e]) (liveMany (ss0 ++ ss1))
    go (While e ss lbl)          = unionMaps (expLiveMap lbl [e]) (liveMany ss)
    go (Assign _ e lbl)          = expLiveMap lbl [e]
    go (AssignSub _ e0 e1 lbl)   = expLiveMap lbl [e0,e1]
    go (Declare _ _ e lbl)       = expLiveMap lbl [e]
    go (Alloc _ _ e lbl)         = expLiveMap lbl [e]
    go (Synchronize _)           = Map.empty
    go (ReadIntCSV _ _ e lbl)    = expLiveMap lbl [e]
    go (PrintIntArray e0 e1 lbl) = expLiveMap lbl [e0,e1]
    go (PrintDoubleArray e0 e1 lbl) = expLiveMap lbl [e0,e1]
    go (Benchmark e ss lbl)      = unionMaps (expLiveMap lbl [e]) (liveMany ss)

killsLiveness :: [Stmt Label] -> LiveMap
killsLiveness stmts = foldl unionMaps Map.empty (map go stmts)
  where
    go (SeqFor v _ ss lbl)       = foldl unionMaps (singleton lbl v) (map go ss)
    go (ParFor _ v _ ss lbl)     = foldl unionMaps (singleton lbl v) (map go ss)
    go (Distribute _ v _ ss lbl) = foldl unionMaps (singleton lbl v) (map go ss)
    go (If _ ss0 ss1 _)          = foldl unionMaps Map.empty (map go (ss0 ++ ss1))
    go (While _ ss _)            = foldl unionMaps Map.empty (map go ss)
    go (Assign v _ lbl)          = singleton lbl v
    go (AssignSub v _ _ lbl)     = singleton lbl v
    go (Declare v _ _ lbl)       = singleton lbl v
    go (Alloc v _ _ lbl)         = singleton lbl v
    go (Synchronize _)           = Map.empty
    go (ReadIntCSV v1 v2 _ lbl)  = Map.singleton lbl (Set.fromList [v1,v2])
    go (PrintIntArray _ _ _)     = Map.empty
    go (PrintDoubleArray _ _ _)  = Map.empty
    go (Benchmark _ ss _)        = foldl unionMaps Map.empty (map go ss)

lkup :: Ord a => a -> Map a (Set b) -> Set b
lkup x m =
  case Map.lookup x m of
    Just x'  -> x'
    Nothing -> Set.empty

liveness :: [Stmt Label]
         -> Graph Label
         -> (Map Label (Set ILName),
             Map Label (Set ILName))
liveness stmts graph =
  let
    gen = gensLiveness stmts
    kill = killsLiveness stmts

    -- Update in set by removing killset and adding gen
    updateIn outMap n =
      lkup n gen `Set.union` (lkup n outMap `Set.difference` lkup n kill)

    -- Update out set to be union of all in-sets of successors
    updateOut inMap n =
      Set.fold (\p s -> Set.union s (lkup p inMap)) Set.empty (successors n graph)

  in backwardAnalysis updateIn updateOut graph
