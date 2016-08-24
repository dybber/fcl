module Language.CGen.Analysis.Graph
 (Graph, Node, nodes, emptyGraph,
  getNode, addNode, addEdge, addEdges,
  startNode, exitNode, setStartNode, setExitNode,
  predecessors, successors, -- dfs, dfsBackwards
 )
where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

data Node lbl = Node { preds :: Set lbl
                     , succs :: Set lbl
                     }
  deriving (Show)

-- Predecessor graph
data Graph lbl =
  Graph {
    assocs :: Map lbl (Node lbl)
    , startNode :: lbl
    , exitNode :: lbl
    }
 deriving (Show)

type Edge lbl = (lbl, lbl)

emptyGraph :: lbl -> lbl -> Graph lbl
emptyGraph start exit = Graph Map.empty start exit

setStartNode :: lbl -> Graph lbl -> Graph lbl
setStartNode lbl g = g { startNode = lbl }

setExitNode :: lbl -> Graph lbl -> Graph lbl
setExitNode lbl g = g { exitNode = lbl }

newNode :: Node lbl
newNode = Node { preds = Set.empty
               , succs = Set.empty }

getNode :: Ord lbl => lbl -> Graph lbl -> Maybe (Node lbl)
getNode x graph = Map.lookup x (assocs graph)

nodes :: Graph lbl -> [lbl]
nodes (Graph{assocs = ass}) = Map.keys ass

-- TODO! Use ST monad/STArray to keep an updatable Array of visited nodes.
-- See implementation in Data.Graph?
-- dfs = undefined
-- dfsBackwards = undefined

-- dfs :: (Show lbl, Ord lbl) => Graph lbl -> lbl -> [lbl]
-- dfs g start =
--   case Set.toList (successors start g) of
--     [] -> [start]
--     xs -> start : concatMap (dfs g) xs

-- dfsBackwards :: Ord lbl => Graph lbl -> lbl -> [lbl]
-- dfsBackwards g exit =
--   let next = Set.toList (predecessors exit g)
--   in exit : concatMap (dfsBackwards g) next

predecessors :: Ord lbl => lbl -> Graph lbl -> Set lbl
predecessors x graph =
  case getNode x graph of
    Just n -> preds n
    Nothing -> error "Node not in graph"

successors :: Ord lbl => lbl -> Graph lbl -> Set lbl
successors x graph =
  case getNode x graph of
    Just n -> succs n
    Nothing -> error "Node not in graph"


-- Add node if it does not already exist
addNode :: Ord lbl => lbl -> Graph lbl -> Graph lbl
addNode x graph =
  case getNode x graph of
    Nothing -> graph { assocs = Map.insert x newNode (assocs graph) }
    Just _  -> graph

addEdge :: Ord lbl => Graph lbl -> Edge lbl -> Graph lbl
addEdge graph (from, to) =
  let
    addToPreds :: Ord lbl => lbl -> Node lbl -> Node lbl
    addToPreds x s = s { preds = Set.insert x (preds s) }

    addToSuccs :: Ord lbl => lbl -> Node lbl -> Node lbl
    addToSuccs x s = s { succs = Set.insert x (succs s) }

    -- Add nodes if they don't exist yet
    g' = addNode from (addNode to graph)
    -- Add edge (from -> to)
    assocs' = Map.adjust (addToSuccs to) from (assocs g')
    -- Add back-edge (to -> from)
    assocs'' = Map.adjust (addToPreds from) to assocs'
  in g' { assocs = assocs'' }

addEdges :: Ord lbl => [(lbl, lbl)] -> Graph lbl -> Graph lbl
addEdges list graph = foldl addEdge graph list
