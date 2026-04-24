-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.IntAdjacencyMap
-- Copyright  : (c) Jack Liell-Cock 2025-2026
-- License    : MIT (see the file LICENSE)
-- Maintainer : jackliellcock@gmail.com
-- Stability  : experimental
--
-- This module defines the t'IntAdjacencyMap' data type for edge-indexed graphs
-- specialised to @Int@ edges. t'IntAdjacencyMap' is an instance of the
-- 'C.EdgeGraph' type class, which can be used for polymorphic graph construction
-- and manipulation. See "EdgeGraph.AdjacencyMap" for graphs with
-- non-@Int@ edges.
-----------------------------------------------------------------------------
module EdgeGraph.IntAdjacencyMap (
  -- * Data structure
  IntAdjacencyMap, Adjacency (..),

  -- * Conversion
  toIncidence, fromIncidence,

  -- * Basic graph construction primitives
  empty, edge, overlay, into, pits, tips, edges, fromNodeList, fromIncidenceList,
  overlays, intos,

  -- * Comparisons
  isSubgraphOf,

  -- * Graph properties
  isEmpty, hasEdge, edgeCount, nodeCount,
  edgeList, adjacencyList, nodeList, edgeIntSet, nodeSet,

  -- * Graph queries
  postset, preset,

  -- * Standard families of graphs
  path, circuit, clique, biclique, flower, node, tree, forest,

  -- * Graph transformation
  replaceEdge, mergeEdges, detachPit, detachTip, gmap, induce,

  -- * Graph algorithms
  dfsForest, topSort, isTopSort
) where

import Data.Set (Set)
import Data.Tree

import EdgeGraph.IntAdjacencyMap.Internal

import qualified Data.Graph      as KL
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified EdgeGraph.Class as C

-- | Overlay a given list of graphs.
overlays :: [IntAdjacencyMap] -> IntAdjacencyMap
overlays = C.overlays

-- | Connect (into) a given list of graphs.
intos :: [IntAdjacencyMap] -> IntAdjacencyMap
intos = C.intos

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second, i.e. @overlay x y == y@.
isSubgraphOf :: IntAdjacencyMap -> IntAdjacencyMap -> Bool
isSubgraphOf x y = overlay x y == y

-- | The /path/ on a list of edges, using 'into' as the connect operator.
path :: [Int] -> IntAdjacencyMap
path = C.path

-- | The /circuit/ on a list of edges.
circuit :: [Int] -> IntAdjacencyMap
circuit = C.circuit

-- | The /clique/ on a list of edges (fully connected via 'into').
clique :: [Int] -> IntAdjacencyMap
clique = C.clique

-- | The /biclique/ on two lists of edges.
biclique :: [Int] -> [Int] -> IntAdjacencyMap
biclique = C.biclique

-- | The /flower graph/ on a list of edges.
flower :: [Int] -> IntAdjacencyMap
flower = C.flower

-- | Construct a /node/ from a list of incoming edges and a list of outgoing
-- edges.
node :: [Int] -> [Int] -> IntAdjacencyMap
node = C.node

-- | The /tree graph/ constructed from a given 'Tree' data structure.
tree :: Tree Int -> IntAdjacencyMap
tree = C.tree

-- | The /forest graph/ constructed from a given 'Forest' data structure.
forest :: Forest Int -> IntAdjacencyMap
forest = C.forest

-- | The function @replaceEdge u v@ replaces edge @u@ with edge
-- label @v@ in a given t'IntAdjacencyMap'. If @v@ already exists, @u@ and @v@
-- will be merged.
replaceEdge :: Int -> Int -> IntAdjacencyMap -> IntAdjacencyMap
replaceEdge u v = gmap $ \w -> if w == u then v else w

-- | Merge edges satisfying a given predicate into a given edge.
mergeEdges :: (Int -> Bool) -> Int -> IntAdjacencyMap -> IntAdjacencyMap
mergeEdges p v = gmap $ \u -> if p u then v else u

-- | The /postset/ (set of successors) of an edge in the graph.
-- These are the edges departing from the sink node of the given edge,
-- i.e. the edges that the given edge flows into.
--
-- @
-- postset x 'EdgeGraph.IntAdjacencyMap.empty'                                                  == 'Data.Set.empty'
-- postset x ('EdgeGraph.IntAdjacencyMap.edge' x)                                               == 'Data.Set.empty'
-- postset 1 ('into' ('EdgeGraph.IntAdjacencyMap.edge' 1) ('EdgeGraph.IntAdjacencyMap.edge' 2)) == 'Data.Set.singleton' 2
-- @
postset :: Int -> IntAdjacencyMap -> Set Int
postset a (IntAdjacencyMap m) = maybe Set.empty succs (Map.lookup a m)

-- | The /preset/ (set of predecessors) of an edge in the graph.
-- These are the edges arriving at the source node of the given edge,
-- i.e. the edges that flow into the given edge.
--
-- @
-- preset x 'EdgeGraph.IntAdjacencyMap.empty'                                                  == 'Data.Set.empty'
-- preset x ('EdgeGraph.IntAdjacencyMap.edge' x)                                               == 'Data.Set.empty'
-- preset 2 ('into' ('EdgeGraph.IntAdjacencyMap.edge' 1) ('EdgeGraph.IntAdjacencyMap.edge' 2)) == 'Data.Set.singleton' 1
-- @
preset :: Int -> IntAdjacencyMap -> Set Int
preset a (IntAdjacencyMap m) = maybe Set.empty preds (Map.lookup a m)

-- Internal: convert to King-Launchbury graph using succs as adjacency.
graphKL :: IntAdjacencyMap -> (KL.Graph, KL.Vertex -> Int)
graphKL (IntAdjacencyMap m) = (g, \u -> case r u of (_, v, _) -> v)
  where
    (g, r) = KL.graphFromEdges'
      [ ((), a, Set.toList (succs adj)) | (a, adj) <- Map.toList m ]

-- | Compute the /depth-first search/ forest of a graph, following the
-- directed flow from each edge to its successors.
--
-- @
-- 'dfsForest' 'EdgeGraph.IntAdjacencyMap.empty'    == []
-- 'dfsForest' ('EdgeGraph.IntAdjacencyMap.edge' x) == [Node x []]
-- 'isSubgraphOf' ('forest' $ 'dfsForest' x) x      == True
-- 'dfsForest' . 'forest' . 'dfsForest'             == 'dfsForest'
-- @
dfsForest :: IntAdjacencyMap -> Forest Int
dfsForest m = let (g, r) = graphKL m in fmap (fmap r) (KL.dff g)

-- | Compute the /topological sort/ of a graph. Returns @Nothing@ if the
-- graph contains a cycle (i.e. there is no valid topological ordering).
-- The ordering respects the flow direction: if edge @a@ flows into edge @b@
-- (via 'into'), then @a@ appears before @b@ in the result.
--
-- @
-- 'topSort' ('path' [1, 2, 3])                   == Just [1, 2, 3]
-- 'topSort' ('circuit' [1, 2])                   == Nothing
-- 'topSort' ('EdgeGraph.IntAdjacencyMap.edge' x) == Just [x]
-- @
topSort :: IntAdjacencyMap -> Maybe [Int]
topSort m = if isTopSort result m then Just result else Nothing
  where
    (g, r) = graphKL m
    result  = map r (KL.topSort g)

-- | Check if a given list of edges is a valid /topological sort/ of
-- a graph. A valid topological sort lists all edges exactly once,
-- and no edge appears after one of its successors.
--
-- @
-- 'isTopSort' [] 'EdgeGraph.IntAdjacencyMap.empty'     == True
-- 'isTopSort' [x] ('EdgeGraph.IntAdjacencyMap.edge' x) == True
-- 'isTopSort' [] ('EdgeGraph.IntAdjacencyMap.edge' x)  == False
-- @
isTopSort :: [Int] -> IntAdjacencyMap -> Bool
isTopSort xs m = go Set.empty xs
  where
    go seen []     = seen == Set.fromList (edgeList m)
    go seen (v:vs) =
      let newSeen = seen `seq` Set.insert v seen
      in postset v m `Set.intersection` newSeen == Set.empty && go newSeen vs
