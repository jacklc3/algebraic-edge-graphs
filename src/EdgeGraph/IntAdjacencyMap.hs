-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.IntAdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the 'IntAdjacencyMap' data type for edge-indexed graphs
-- specialised to @Int@ edges. 'IntAdjacencyMap' is an instance of the
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
    path, circuit, clique, biclique, star, tree, forest,

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

overlays :: [IntAdjacencyMap] -> IntAdjacencyMap
overlays = C.overlays

intos :: [IntAdjacencyMap] -> IntAdjacencyMap
intos = C.intos

isSubgraphOf :: IntAdjacencyMap -> IntAdjacencyMap -> Bool
isSubgraphOf x y = overlay x y == y

path :: [Int] -> IntAdjacencyMap
path = C.path

circuit :: [Int] -> IntAdjacencyMap
circuit = C.circuit

clique :: [Int] -> IntAdjacencyMap
clique = C.clique

biclique :: [Int] -> [Int] -> IntAdjacencyMap
biclique = C.biclique

star :: Int -> [Int] -> IntAdjacencyMap
star = C.star

tree :: Tree Int -> IntAdjacencyMap
tree = C.tree

forest :: Forest Int -> IntAdjacencyMap
forest = C.forest

replaceEdge :: Int -> Int -> IntAdjacencyMap -> IntAdjacencyMap
replaceEdge u v = gmap $ \w -> if w == u then v else w

mergeEdges :: (Int -> Bool) -> Int -> IntAdjacencyMap -> IntAdjacencyMap
mergeEdges p v = gmap $ \u -> if p u then v else u

-- | The /postset/ (set of successors) of an edge in the graph.
--
-- @
-- postset x 'empty'                      == Set.'Set.empty'
-- postset x ('edge' x)                   == Set.'Set.empty'
-- postset 1 ('into' ('edge' 1) ('edge' 2)) == Set.'Set.singleton' 2
-- @
postset :: Int -> IntAdjacencyMap -> Set Int
postset a (IntAdjacencyMap m) = maybe Set.empty succs (Map.lookup a m)

-- | The /preset/ (set of predecessors) of an edge in the graph.
--
-- @
-- preset x 'empty'                      == Set.'Set.empty'
-- preset x ('edge' x)                   == Set.'Set.empty'
-- preset 2 ('into' ('edge' 1) ('edge' 2)) == Set.'Set.singleton' 1
-- @
preset :: Int -> IntAdjacencyMap -> Set Int
preset a (IntAdjacencyMap m) = maybe Set.empty preds (Map.lookup a m)

-- Internal: convert to King-Launchbury graph using succs as adjacency.
graphKL :: IntAdjacencyMap -> (KL.Graph, KL.Vertex -> Int)
graphKL (IntAdjacencyMap m) = (g, \u -> case r u of (_, v, _) -> v)
  where
    (g, r) = KL.graphFromEdges'
        [ ((), a, Set.toList (succs adj)) | (a, adj) <- Map.toList m ]

-- | Compute the /depth-first search/ forest of a graph.
--
-- @
-- 'dfsForest' 'empty'                          == []
-- 'dfsForest' ('edge' x)                       == [Node x []]
-- 'isSubgraphOf' ('forest' $ 'dfsForest' x) x == True
-- 'dfsForest' . 'forest' . 'dfsForest'         == 'dfsForest'
-- @
dfsForest :: IntAdjacencyMap -> Forest Int
dfsForest m = let (g, r) = graphKL m in fmap (fmap r) (KL.dff g)

-- | Compute the /topological sort/ of a graph. Returns @Nothing@ if the
-- graph contains a cycle.
--
-- @
-- 'topSort' ('path' [1, 2, 3]) == Just [1, 2, 3]
-- 'topSort' ('circuit' [1, 2]) == Nothing
-- 'topSort' ('edge' x)         == Just [x]
-- @
topSort :: IntAdjacencyMap -> Maybe [Int]
topSort m = if isTopSort result m then Just result else Nothing
  where
    (g, r) = graphKL m
    result  = map r (KL.topSort g)

-- | Check if a given list of edges is a valid /topological sort/ of
-- a graph.
--
-- @
-- 'isTopSort' [] 'empty'     == True
-- 'isTopSort' [x] ('edge' x) == True
-- 'isTopSort' [] ('edge' x)  == False
-- @
isTopSort :: [Int] -> IntAdjacencyMap -> Bool
isTopSort xs m = go Set.empty xs
  where
    go seen []     = seen == Set.fromList (edgeList m)
    go seen (v:vs) = let newSeen = seen `seq` Set.insert v seen
        in postset v m `Set.intersection` newSeen == Set.empty && go newSeen vs
