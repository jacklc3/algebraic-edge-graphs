-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the 'AdjacencyMap' data type as an edge-indexed
-- adjacency map for algebraic edge graphs. Each edge maps to an
-- 'Adjacency' record describing its neighbourhood (forks, joins,
-- predecessors, successors). 'AdjacencyMap' is an instance of the
-- 'C.EdgeGraph' type class, which can be used for polymorphic graph
-- construction and manipulation.
--
-----------------------------------------------------------------------------
module EdgeGraph.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, Adjacency (..),

    -- * Conversion
    toIncidence, fromIncidence,

    -- * Basic graph construction primitives
    empty, edge, overlay, into, pits, tips, edges, fromNodeList, fromIncidenceList,
    overlays, intos,

    -- * Comparisons
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasEdge, edgeCount, nodeCount,
    edgeList, adjacencyList, nodeList, edgeSet, nodeSet, edgeIntSet,

    -- * Graph queries
    postset, preset,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest,

    -- * Graph transformation
    replaceEdge, mergeEdges, detachPit, detachTip, gmap, induce,

    -- * Graph algorithms
    dfsForest, topSort, isTopSort, scc
  ) where

import Data.Foldable (toList)
import Data.Set (Set)
import Data.Tree

import EdgeGraph.AdjacencyMap.Internal

import qualified Data.Graph          as KL
import qualified Data.IntSet         as IntSet
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified EdgeGraph.Class     as C

-- | Overlay a given list of graphs.
overlays :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
overlays = C.overlays

-- | Connect (into) a given list of graphs.
intos :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
intos = C.intos

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second, i.e. @overlay x y == y@.
isSubgraphOf :: Ord a => AdjacencyMap a -> AdjacencyMap a -> Bool
isSubgraphOf x y = overlay x y == y

-- | The /path/ on a list of edges, using 'into' as the connect operator.
path :: Ord a => [a] -> AdjacencyMap a
path = C.path

-- | The /circuit/ on a list of edges.
circuit :: Ord a => [a] -> AdjacencyMap a
circuit = C.circuit

-- | The /clique/ on a list of edges (fully connected via 'into').
clique :: Ord a => [a] -> AdjacencyMap a
clique = C.clique

-- | The /biclique/ on two lists of edges.
biclique :: Ord a => [a] -> [a] -> AdjacencyMap a
biclique = C.biclique

-- | The /star/ formed by a centre edge and a list of leaf edges.
star :: Ord a => a -> [a] -> AdjacencyMap a
star = C.star

-- | The /tree graph/ constructed from a given 'Tree' data structure.
tree :: Ord a => Tree a -> AdjacencyMap a
tree = C.tree

-- | The /forest graph/ constructed from a given 'Forest' data structure.
forest :: Ord a => Forest a -> AdjacencyMap a
forest = C.forest

-- | The function @replaceEdge u v@ replaces edge @u@ with edge
-- label @v@ in a given 'AdjacencyMap'. If @v@ already exists, @u@ and @v@
-- will be merged.
replaceEdge :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
replaceEdge u v = gmap $ \w -> if w == u then v else w

-- | Merge edges satisfying a given predicate into a given edge.
mergeEdges :: Ord a => (a -> Bool) -> a -> AdjacencyMap a -> AdjacencyMap a
mergeEdges p v = gmap $ \u -> if p u then v else u

-- | The set of edges of type 'Int', as an 'IntSet.IntSet'.
edgeIntSet :: AdjacencyMap Int -> IntSet.IntSet
edgeIntSet = IntSet.fromDistinctAscList . Map.keys . adjacencyMap

-- | The /postset/ (set of successors) of an edge in the graph.
-- These are the edges departing from the sink node of the given edge,
-- i.e. the edges that the given edge flows into.
--
-- @
-- postset x 'empty'                      == Set.'Set.empty'
-- postset x ('edge' x)                   == Set.'Set.empty'
-- postset 1 ('into' ('edge' 1) ('edge' 2)) == Set.'Set.singleton' 2
-- @
postset :: Ord a => a -> AdjacencyMap a -> Set a
postset a (AdjacencyMap m) = maybe Set.empty succs (Map.lookup a m)

-- | The /preset/ (set of predecessors) of an edge in the graph.
-- These are the edges arriving at the source node of the given edge,
-- i.e. the edges that flow into the given edge.
--
-- @
-- preset x 'empty'                      == Set.'Set.empty'
-- preset x ('edge' x)                   == Set.'Set.empty'
-- preset 2 ('into' ('edge' 1) ('edge' 2)) == Set.'Set.singleton' 1
-- @
preset :: Ord a => a -> AdjacencyMap a -> Set a
preset a (AdjacencyMap m) = maybe Set.empty preds (Map.lookup a m)

-- Internal: convert to King-Launchbury graph using succs as adjacency.
-- Only captures the directed flow structure (not fork/join groupings).
graphKL :: Ord a => AdjacencyMap a -> (KL.Graph, KL.Vertex -> a)
graphKL (AdjacencyMap m) = (g, \u -> case r u of (_, v, _) -> v)
  where
    (g, r) = KL.graphFromEdges'
        [ ((), a, Set.toList (succs adj)) | (a, adj) <- Map.toList m ]

-- | Compute the /depth-first search/ forest of a graph, following the
-- directed flow from each edge to its successors.
--
-- @
-- 'dfsForest' 'empty'                          == []
-- 'dfsForest' ('edge' x)                       == [Node x []]
-- 'isSubgraphOf' ('forest' $ 'dfsForest' x) x == True
-- 'dfsForest' . 'forest' . 'dfsForest'         == 'dfsForest'
-- @
dfsForest :: Ord a => AdjacencyMap a -> Forest a
dfsForest m = let (g, r) = graphKL m in fmap (fmap r) (KL.dff g)

-- | Compute the /topological sort/ of a graph. Returns @Nothing@ if the
-- graph contains a cycle (i.e. there is no valid topological ordering).
-- The ordering respects the flow direction: if edge @a@ flows into edge @b@
-- (via 'into'), then @a@ appears before @b@ in the result.
--
-- @
-- 'topSort' ('path' [1, 2, 3]) == Just [1, 2, 3]
-- 'topSort' ('circuit' [1, 2]) == Nothing
-- 'topSort' ('edge' x)         == Just [x]
-- @
topSort :: Ord a => AdjacencyMap a -> Maybe [a]
topSort m = if isTopSort result m then Just result else Nothing
  where
    (g, r) = graphKL m
    result  = map r (KL.topSort g)

-- | Check if a given list of edges is a valid /topological sort/ of
-- a graph. A valid topological sort lists all edges exactly once,
-- and no edge appears after one of its successors.
--
-- @
-- 'isTopSort' [] 'empty'      == True
-- 'isTopSort' [x] ('edge' x)  == True
-- 'isTopSort' [] ('edge' x)   == False
-- @
isTopSort :: Ord a => [a] -> AdjacencyMap a -> Bool
isTopSort xs m = go Set.empty xs
  where
    go seen []     = seen == edgeSet m
    go seen (v:vs) = let newSeen = seen `seq` Set.insert v seen
        in postset v m `Set.intersection` newSeen == Set.empty && go newSeen vs

-- | Compute the /condensation/ of a graph, where each edge is
-- replaced by its strongly connected component (a 'Set' of edges).
-- Edges that form directed cycles are grouped into the same component.
--
-- @
-- 'scc' 'empty'                                   == 'empty'
-- 'scc' ('edge' x)                                == 'edge' (Set.'Set.singleton' x)
-- 'scc' ('circuit' (1:xs))                        == 'edge' (Set.'Set.fromList' (1:xs))
-- 'edgeCount' ('scc' x) >= 'edgeCount' x  == False
-- @
scc :: Ord a => AdjacencyMap a -> AdjacencyMap (Set a)
scc m = gmap (\v -> Map.findWithDefault Set.empty v components) m
  where
    (g, r)     = graphKL m
    components = Map.fromList $ concatMap (expand . map r . toList) (KL.scc g)
    expand xs  = let s = Set.fromList xs in map (\x -> (x, s)) xs
