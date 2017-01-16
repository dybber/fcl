module FCL.IL.Analysis.Dataflow
 (Label(..), makeFlowGraph, addLabels, forwardAnalysis, backwardAnalysis)
where

import Control.Monad.Trans.State (evalState, get, modify, execState, State)

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List

import FCL.IL.Syntax
import FCL.IL.Analysis.Graph

--------------------------
-- Labelling statements --
--------------------------
newtype Label = Label Int
 deriving (Eq, Ord)

instance Show Label where
  show (Label i) = show i

addLabels :: [Stmt a] -> [Stmt Label]
addLabels stmts = evalState (addMany stmts) (Label 1)
  where
    next :: State Label Label
    next = do
      s <- get
      modify (\(Label c) -> Label (c+1))
      return s

    addMany :: [Stmt a] -> State Label ([Stmt Label])
    addMany = mapM addLabel

    addLabel :: Stmt a -> State Label (Stmt Label)
    addLabel (SeqFor v e ss _)       = SeqFor v e        <$> addMany ss <*> next
    addLabel (ParFor lvl v e ss _)   = ParFor lvl v e    <$> addMany ss <*> next
    addLabel (Distribute lvl v e ss _) = Distribute lvl v e <$> addMany ss <*> next
    addLabel (While v ss _)          = While v           <$> addMany ss <*> next
    addLabel (If e ss0 ss1 _)        = If e              <$> addMany ss0 <*> addMany ss1 <*> next
    addLabel (Assign v e _)          = Assign v e        <$> next
    addLabel (AssignSub v e0 e1 _)   = AssignSub v e0 e1 <$> next
    addLabel (Declare v e _)         = Declare v e       <$> next
    addLabel (Alloc v t e _)         = Alloc v t e       <$> next
    addLabel (Benchmark e ss _)      = Benchmark e       <$> (addMany ss) <*> next
    addLabel (Synchronize _)         = Synchronize       <$> next
    addLabel (ReadIntCSV v1 v2 e _)  = ReadIntCSV v1 v2 e <$> next
    addLabel (PrintIntArray e1 e2 _) = PrintIntArray e1 e2 <$> next

---------------------------
-- Create dataflow graph --
---------------------------
addEdgesM :: (Ord lbl) => [(lbl, lbl)] -> State (Graph lbl) ()
addEdgesM list = modify (addEdges list)

makeFlowGraph :: [Stmt Label] -> Graph Label
makeFlowGraph stmts =
  let root = Label 0
      empty = emptyGraph root root -- start with root and exit on the same node
  in execState (buildGraphLs stmts [root]) empty

buildGraphLs :: [Stmt Label] -> [Label] -> State (Graph Label) [Label]
buildGraphLs [] preds = return preds
buildGraphLs (s:ss) preds =
  let setExitM :: lbl -> State (Graph lbl) ()
      setExitM lbl = modify (setExitNode lbl)
  in do preds' <- buildGraph s preds
        setExitM (labelOf s) -- keep updating the exitNode until we reach the very end
        buildGraphLs ss preds'

mkLoop :: [Stmt Label] -> Label -> [Label] -> State (Graph Label) [Label]
mkLoop ss lbl preds = 
  do end0 <- buildGraphLs ss [lbl]
     addEdgesM [(p, lbl) | p <- preds ++ end0]
     return [lbl]

buildGraph :: Stmt Label -> [Label] -> State (Graph Label) [Label]
buildGraph (Distribute _ _ _ ss0 lbl) preds = mkLoop ss0 lbl preds
buildGraph (ParFor _ _ _ ss0 lbl) preds     = mkLoop ss0 lbl preds
buildGraph (SeqFor _ _ ss0 lbl) preds       = mkLoop ss0 lbl preds
buildGraph (While _ ss0 lbl) preds          = mkLoop ss0 lbl preds
buildGraph (Benchmark _ ss0 lbl) preds      = mkLoop ss0 lbl preds
buildGraph (If _ ss0 ss1 lbl) preds  =
  do addEdgesM [(p, lbl) | p <- preds]
     end0 <- buildGraphLs ss0 [lbl]
     end1 <- buildGraphLs ss1 [lbl]
     return (end0 ++ end1)
buildGraph stmt preds =
  do let lbl = labelOf stmt
     addEdgesM [(p, lbl) | p <- preds]
     return [lbl]

-- TODO: eliminate code duplication below

-- Using work-list algorithm
forwardAnalysis :: Eq a =>
                   (Map Label (Set a) -> Label -> Set a)
                -> (Map Label (Set a) -> Label -> Set a)
                -> Graph Label
                -> (Map Label (Set a),
                    Map Label (Set a))
forwardAnalysis updateIn updateOut graph =
  let
    loop inMap outMap [] = (inMap, outMap)
    loop inMap outMap (w:newWL) =
          let out =
                case Map.lookup w outMap of
                  Just x'  -> x'
                  Nothing -> Set.empty

              in'  = updateIn outMap w
              inMap' = Map.insert w in' inMap

              out' = updateOut inMap' w
              outMap' = Map.insert w out' outMap
          in if out /= out'
               then loop inMap' outMap' (newWL `Data.List.union` (Set.toList (successors w graph)))
               else loop inMap' outMap' newWL

    initIn = Map.empty
    initOut = Map.empty
    initWorklist = nodes graph -- dfs graph (startNode graph)
  in loop initIn initOut initWorklist

-- This does not currently work. Stops too early
-- TODO fix!
-- -- Using work-list algorithm
-- backwardAnalysis :: Eq a =>
--                    (Map Label (Set a) -> Label -> Set a)
--                 -> (Map Label (Set a) -> Label -> Set a)
--                 -> Graph Label
--                 -> (Map Label (Set a),
--                     Map Label (Set a))
-- backwardAnalysis updateIn updateOut graph =
--   let
--     loop inMap outMap [] = (inMap, outMap)
--     loop inMap outMap (w:newWL) =
--           let out =
--                 case Map.lookup w outMap of
--                   Just x'  -> x'
--                   Nothing -> Set.empty

--               in'  = updateIn outMap w
--               inMap' = Map.insert w in' inMap

--               out' = updateOut inMap' w
--               outMap' = Map.insert w out' outMap
--           in if out /= out'
--                then loop inMap' outMap' (newWL `Data.List.union` (Set.toList (predecessors w graph)))
--                else loop inMap' outMap' newWL

--     initIn = Map.empty
--     initOut = Map.empty
--     initWorklist = reverse $ nodes graph -- dfsBackwards graph (exitNode graph)
--   in loop initIn initOut initWorklist

-- Slower but works
backwardAnalysis :: Eq a =>
                   (Map Label (Set a) -> Label -> Set a)
                -> (Map Label (Set a) -> Label -> Set a)
                -> Graph Label
                -> (Map Label (Set a),
                    Map Label (Set a))
backwardAnalysis updateIn updateOut graph =
  let
    loop inMap outMap [] = (inMap, outMap)
    loop inMap outMap (w:newWL) =
          let in'  = updateIn outMap w
              inMap' = Map.insert w in' inMap

              out' = updateOut inMap' w
              outMap' = Map.insert w out' outMap
          in loop inMap' outMap' newWL

    initIn = Map.empty
    initOut = Map.empty
    initWorklist = reverse $ nodes graph -- dfsBackwards graph (exitNode graph)

    fixp in_ out_ =
      let (in', out') = loop in_ out_ initWorklist
      in if in' /= in_ || out_ /= out'
           then fixp in' out'
           else (in_, out_)
    
  in fixp initIn initOut
