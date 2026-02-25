-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.Incidence
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the 'Incidence' data type for algebraic edge graphs,
-- as well as associated operations and algorithms. 'Incidence' is an instance
-- of the 'C.EdgeGraph' type class, which can be used for polymorphic graph
-- construction and manipulation.
--
-- See "EdgeGraph.Incidence.Internal" for the underlying implementation.
-----------------------------------------------------------------------------
module EdgeGraph.Incidence (
    -- * Data structure
    Incidence, Node, nodes,

    -- * Basic graph construction primitives
    empty, edge, overlay, into, pits, tips, edges, fromNodeList, fromIncidenceList,

    -- * Comparisons
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasEdge, edgeCount, nodeCount,
    edgeList, nodeList, edgeSet, nodeSet, edgeIntSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest,

    -- * Graph transformation
    replaceEdge, mergeEdges, detachPit, detachTip, gmap, induce,

    -- * Graph construction from lists
    overlays, intos
  ) where

import EdgeGraph.Incidence.Internal

import qualified EdgeGraph.Class as C
import qualified Data.IntSet         as IntSet
import qualified Data.Tree           as Tree

-- | The 'isSubgraphOf' function takes two incidences and returns 'True' if the
-- first graph is a /subgraph/ of the second, i.e. @overlay x y == y@.
-- Complexity: /O(n^2 * m)/ time.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('edge' x)      'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- @
isSubgraphOf :: Ord a => Incidence a -> Incidence a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Overlay a given list of graphs.
-- Complexity: /O((n) * log(n))/ time and /O(n)/ memory.
--
-- @
-- overlays []    == 'empty'
-- overlays [x]   == x
-- overlays [x,y] == 'overlay' x y
-- @
overlays :: Ord a => [Incidence a] -> Incidence a
overlays = C.overlays

-- | Connect (into) a given list of graphs.
-- Complexity: /O((n) * log(n))/ time and /O(n)/ memory.
--
-- @
-- intos []    == 'empty'
-- intos [x]   == x
-- intos [x,y] == 'into' x y
-- @
intos :: Ord a => [Incidence a] -> Incidence a
intos = C.intos

-- | The /path/ on a list of edges, using 'into' as the connect operator.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'edge' x
-- path [x,y] == 'into' ('edge' x) ('edge' y)
-- @
path :: Ord a => [a] -> Incidence a
path = C.path

-- | The /circuit/ on a list of edges.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'into' ('edge' x) ('edge' x)
-- circuit [x,y] == 'into' ('edge' x) ('into' ('edge' y) ('edge' x))
-- @
circuit :: Ord a => [a] -> Incidence a
circuit = C.circuit

-- | The /clique/ on a list of edges (fully connected via 'into').
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- clique []    == 'empty'
-- clique [x]   == 'edge' x
-- clique [x,y] == 'into' ('edge' x) ('edge' y)
-- @
clique :: Ord a => [a] -> Incidence a
clique = C.clique

-- | The /biclique/ on two lists of edges.
-- Complexity: /O((L1 + L2) * log(L1 + L2))/ time and /O(L1 + L2)/ memory,
-- where /L1/ and /L2/ are the lengths of the given lists.
--
-- @
-- biclique []  []  == 'empty'
-- biclique [x] []  == 'edge' x
-- biclique []  [y] == 'edge' y
-- @
biclique :: Ord a => [a] -> [a] -> Incidence a
biclique = C.biclique

-- | The /star/ formed by a centre edge and a list of leaf edges.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- star x []    == 'edge' x
-- star x [y]   == 'into' ('edge' x) ('edge' y)
-- star x [y,z] == 'into' ('edge' x) ('edges' [y, z])
-- @
star :: Ord a => a -> [a] -> Incidence a
star = C.star

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O(T * log(T))/ time and /O(T)/ memory, where /T/ is the size
-- of the given tree.
tree :: Ord a => Tree.Tree a -> Incidence a
tree = C.tree

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O(F * log(F))/ time and /O(F)/ memory, where /F/ is the size
-- of the given forest.
forest :: Ord a => Tree.Forest a -> Incidence a
forest = C.forest

-- | The function @replaceEdge x y@ replaces edge @x@ with edge
-- label @y@ in a given 'Incidence'. If @y@ already exists, the labels
-- will be merged.
-- Complexity: /O(n * m * log(m))/ time.
--
-- @
-- replaceEdge x x            == id
-- replaceEdge x y ('edge' x)   == 'edge' y
-- replaceEdge x y            == 'mergeEdges' (== x) y
-- @
replaceEdge :: Ord a => a -> a -> Incidence a -> Incidence a
replaceEdge u v = gmap (\w -> if w == u then v else w)

-- | Merge edges satisfying a given predicate with a given edge.
-- Complexity: /O(n * m * log(m))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeEdges (const False) x == id
-- mergeEdges (== x) y        == 'replaceEdge' x y
-- @
mergeEdges :: Ord a => (a -> Bool) -> a -> Incidence a -> Incidence a
mergeEdges p v = gmap (\u -> if p u then v else u)

-- | The set of edges of a given graph, specialised for graphs with
-- edges of type 'Int'.
-- Complexity: /O(n * m)/ time.
--
-- @
-- edgeIntSet 'empty'      == IntSet.'IntSet.empty'
-- edgeIntSet ('edge' x)   == IntSet.'IntSet.singleton' x
-- @
edgeIntSet :: Incidence Int -> IntSet.IntSet
edgeIntSet = IntSet.fromAscList . edgeList
