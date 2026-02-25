-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.HigherKinded.Class
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the core higher-kinded type class 'EdgeGraph' for
-- algebraic edge graphs, a few graph subclasses, and basic polymorphic graph
-- construction primitives. Functions that cannot be implemented fully
-- polymorphically and require the use of an intermediate data type are not
-- included. For example, to compute the number of edge in an 'EdgeGraph'
-- expression you will need to use a concrete data type, such as
-- "EdgeGraph.Fold".
--
-- See "EdgeGraph.Class" for alternative definitions where the core type
-- class is not higher-kinded and permits more instances.
-----------------------------------------------------------------------------
module EdgeGraph.HigherKinded.Class (
    -- * The core type class
    EdgeGraph (..), empty, edge, overlay,

    -- * Basic graph construction primitives
    edges, overlays, intos,

    -- * Comparisons
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasEdge, edgeCount, edgeList, edgeSet,
    edgeIntSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest, mesh, torus, deBruijn,

    -- * Graph transformation
    removeEdge, replaceEdge, mergeEdges, splitEdge,
    induce,

    -- * Graph composition
    box,

    -- * Conversion between graph data types
    ToEdgeGraph (..)

  ) where

import Control.Applicative (empty, (<|>))
import Control.Monad
import Data.Foldable
import Data.Tree

import qualified Data.IntSet as IntSet
import qualified Data.Set    as Set

{-|
The core higher-kinded type class for constructing algebraic edge graphs is
defined by introducing the 'into', 'pits' and 'tips' methods to the standard
'MonadPlus' class and reusing the following existing methods:

* The 'empty' method comes from the 'Control.Applicative.Alternative' class and
corresponds to the /empty graph/. This module simply re-exports it.

* The 'edge' graph construction primitive is an alias for 'pure' of the
'Applicative' type class.

* Graph 'overlay' is an alias for '<|>' of the 'Alternative' type class.

The 'EdgeGraph' type class is characterised by the following minimal set of
axioms. In equations we use @+@ and @*@ as convenient shortcuts for 'overlay'
and 'into', respectively.

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'into' is associative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'into' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'into' can be decomposed:

        > x * y * z == x * y + x * z + y * z

    * 'pits' and 'tips' satisfy analogous axioms (each forms a united monoid
      with 'overlay'), plus reflexivity on edges:

        > pits (edge a) (edge a) == edge a
        > tips (edge a) (edge a) == edge a

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'into':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ will
denote the number of edges in the graph, /m/ will denote the number of
nodes in the graph, and /s/ will denote the /size/ of the corresponding
'EdgeGraph' expression.
-}
class (Traversable g, MonadPlus g) => EdgeGraph g where
    -- | Connect two graphs sequentially (pits of left to tips of right).
    into :: g a -> g a -> g a
    -- | Connect two graphs at pits (where outgoing edges overlap).
    pits :: g a -> g a -> g a
    -- | Connect two graphs at tips (where incoming edges overlap).
    tips :: g a -> g a -> g a

-- | Construct the graph comprising a single edge. An alias for 'pure'.
edge :: EdgeGraph g => a -> g a
edge = pure

-- | Overlay two graphs. An alias for '<|>'.
overlay :: EdgeGraph g => g a -> g a -> g a
overlay = (<|>)

-- | Construct the graph comprising a given list of isolated edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []  == 'empty'
-- edges [x] == 'edge' x
-- @
edges :: EdgeGraph g => [a] -> g a
edges = overlays . map edge

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []    == 'empty'
-- overlays [x]   == x
-- overlays [x,y] == 'overlay' x y
-- @
overlays :: EdgeGraph g => [g a] -> g a
overlays = msum

-- | Connect (into) a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- intos []    == 'empty'
-- intos [x]   == x
-- intos [x,y] == 'into' x y
-- @
intos :: EdgeGraph g => [g a] -> g a
intos = foldr into empty

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second. Here is the current implementation:
--
-- @
-- isSubgraphOf x y = 'overlay' x y == y
-- @
-- The complexity therefore depends on the complexity of equality testing of
-- a particular graph instance.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('edge' x)      'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- @
isSubgraphOf :: (EdgeGraph g, Eq (g a)) => g a -> g a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('edge' x)                    == False
-- isEmpty ('removeEdge' x $ 'edge' x) == True
-- @
isEmpty :: EdgeGraph g => g a -> Bool
isEmpty = null

-- | Check if a graph contains a given edge. A convenient alias for 'elem'.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x 'empty'              == False
-- hasEdge x ('edge' x)           == True
-- hasEdge x . 'removeEdge' x == const False
-- @
hasEdge :: (Eq a, EdgeGraph g) => a -> g a -> Bool
hasEdge = elem

-- | The number of distinct edges in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- edgeCount 'empty'    == 0
-- edgeCount ('edge' x) == 1
-- edgeCount          == 'length' . 'edgeList'
-- @
edgeCount :: (Ord a, EdgeGraph g) => g a -> Int
edgeCount = length . edgeList

-- | The sorted list of distinct edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeList 'empty'    == []
-- edgeList ('edge' x) == [x]
-- @
edgeList :: (Ord a, EdgeGraph g) => g a -> [a]
edgeList = Set.toAscList . edgeSet

-- | The set of distinct edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet . 'edge'     == Set.'Set.singleton'
-- @
edgeSet :: (Ord a, EdgeGraph g) => g a -> Set.Set a
edgeSet = foldr Set.insert Set.empty

-- | The set of edges of a given graph, specialised for graphs with
-- edges of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeIntSet 'empty'    == IntSet.'IntSet.empty'
-- edgeIntSet . 'edge'   == IntSet.'IntSet.singleton'
-- @
edgeIntSet :: EdgeGraph g => g Int -> IntSet.IntSet
edgeIntSet = foldr IntSet.insert IntSet.empty

-- | The /path/ on a list of edges, using 'into' as the connect operator.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'edge' x
-- path [x,y] == 'into' ('edge' x) ('edge' y)
-- @
path :: EdgeGraph g => [a] -> g a
path []  = empty
path [x] = edge x
path xs  = intos (map edge xs)

-- | The /circuit/ on a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'into' ('edge' x) ('edge' x)
-- circuit [x,y] == 'into' ('edge' x) ('into' ('edge' y) ('edge' x))
-- @
circuit :: EdgeGraph g => [a] -> g a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of edges (fully connected via 'into').
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique []    == 'empty'
-- clique [x]   == 'edge' x
-- clique [x,y] == 'into' ('edge' x) ('edge' y)
-- @
clique :: EdgeGraph g => [a] -> g a
clique = intos . map edge

-- | The /biclique/ on two lists of edges.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []  []  == 'empty'
-- biclique [x] []  == 'edge' x
-- biclique []  [y] == 'edge' y
-- @
biclique :: EdgeGraph g => [a] -> [a] -> g a
biclique xs ys = into (edges xs) (edges ys)

-- | The /star/ formed by a centre edge and a list of leaf edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'edge' x
-- star x [y]   == 'into' ('edge' x) ('edge' y)
-- star x [y,z] == 'into' ('edge' x) ('edges' [y, z])
-- @
star :: EdgeGraph g => a -> [a] -> g a
star x ys = into (edge x) (edges ys)

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree.
tree :: EdgeGraph g => Tree a -> g a
tree (Node x f) = overlay (star x $ map rootLabel f) (forest f)

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest.
forest :: EdgeGraph g => Forest a -> g a
forest = overlays . map tree

-- | Construct a /mesh graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh xs     []   == 'empty'
-- mesh []     ys   == 'empty'
-- mesh [x]    [y]  == 'edge' (x, y)
-- @
mesh :: EdgeGraph g => [a] -> [b] -> g (a, b)
mesh xs ys = path xs `box` path ys

-- | Construct a /torus graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus xs     []   == 'empty'
-- torus []     ys   == 'empty'
-- @
torus :: EdgeGraph g => [a] -> [b] -> g (a, b)
torus xs ys = circuit xs `box` circuit ys

-- | Construct a /De Bruijn graph/ of given dimension and symbols of a given
-- alphabet.
-- Complexity: /O(A * D^A)/ time, memory and size, where /A/ is the size of the
-- alphabet and /D/ is the dimension of the graph.
--
-- @
-- deBruijn k []    == 'empty'
-- @
deBruijn :: EdgeGraph g => Int -> [a] -> g [a]
deBruijn len alphabet = skeleton >>= expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = edges [ Left s | s <- overlaps ] `into`
               edges [ Right s | s <- overlaps ]
    expand v = edges [ either ([a] ++) (++ [a]) v | a <- alphabet ]

-- | Remove all occurrences of an edge from the graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x ('edge' x)           == 'empty'
-- removeEdge x . removeEdge x == removeEdge x
-- @
removeEdge :: (Eq a, EdgeGraph g) => a -> g a -> g a
removeEdge v = induce (/= v)

-- | The function @replaceEdge x y@ replaces edge @x@ with edge
-- label @y@ in a given graph. If @y@ already exists, the labels will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceEdge x x            == id
-- replaceEdge x y ('edge' x)   == 'edge' y
-- replaceEdge x y            == 'mergeEdges' (== x) y
-- @
replaceEdge :: (Eq a, EdgeGraph g) => a -> a -> g a -> g a
replaceEdge u v = fmap $ \w -> if w == u then v else w

-- | Merge edges satisfying a given predicate with a given edge.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeEdges (const False) x == id
-- mergeEdges (== x) y        == 'replaceEdge' x y
-- @
mergeEdges :: (Eq a, EdgeGraph g) => (a -> Bool) -> a -> g a -> g a
mergeEdges p v = fmap $ \w -> if p w then v else w

-- | Split an edge into a list of edges with the same connectivity.
-- Complexity: /O(s + k * L)/ time, memory and size, where /k/ is the number of
-- occurrences of the edge in the expression and /L/ is the length of the
-- given list.
--
-- @
-- splitEdge x []             == 'removeEdge' x
-- splitEdge x [x]            == id
-- splitEdge x [y]            == 'replaceEdge' x y
-- @
splitEdge :: (Eq a, EdgeGraph g) => a -> [a] -> g a -> g a
splitEdge v us g = g >>= \w -> if w == v then edges us else edge w

-- | Construct the /induced subgraph/ of a given graph by removing edges
-- that do not satisfy a given predicate.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- induce (const True)  x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeEdge' x
-- induce p . induce q     == induce (\\x -> p x && q x)
-- @
induce :: EdgeGraph g => (a -> Bool) -> g a -> g a
induce = mfilter

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O(s1 * s2)/ time, memory and size, where /s1/ and /s2/ are the
-- sizes of the given graphs.
--
-- @
-- box ('path' [0,1]) ('path' "ab") == 'edges' [ ((0,\'a\'),(0,\'b\')), ((0,\'a\'),(1,\'a\'))
--                                          , ((0,\'b\'),(1,\'b\')), ((1,\'a\'),(1,\'b\')) ]
-- @
-- Up to an isomorphism between the resulting edge types, this operation
-- is /commutative/, /associative/, /distributes/ over 'overlay', has singleton
-- graphs as /identities/ and 'empty' as the /annihilating zero/. Below @~~@
-- stands for the equality up to an isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y             ~~ box y x
-- box x (box y z)     ~~ box (box x y) z
-- box x ('overlay' y z) == 'overlay' (box x y) (box x z)
-- box x ('edge' ())     ~~ x
-- box x 'empty'         ~~ 'empty'
-- @
box :: EdgeGraph g => g a -> g b -> g (a, b)
box x y = msum $ xs ++ ys
  where
    xs = map (\b -> fmap (,b) x) $ toList y
    ys = map (\a -> fmap (a,) y) $ toList x

-- | The 'ToEdgeGraph' type class captures data types that can be converted to
-- polymorphic edge graph expressions. The conversion method 'toEdgeGraph'
-- semantically acts as the identity on graph data structures, but allows to
-- convert graphs between different data representations.
class ToEdgeGraph t where
    toEdgeGraph :: EdgeGraph g => t a -> g a
