-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.HigherKinded.Class
-- Copyright  : (c) Jack Liell-Cock 2025-2026
-- License    : MIT (see the file LICENSE)
-- Maintainer : jackliellcock@gmail.com
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
  edges, overlays, intos, pitss, tipss,

  -- * Comparisons
  isSubgraphOf,

  -- * Graph properties
  isEmpty, hasEdge, edgeCount, edgeList, edgeSet,
  edgeIntSet,

  -- * Standard families of graphs
  path, circuit, clique, biclique, flower, node, tree, forest, mesh, torus, deBruijn,

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

* The 'EdgeGraph.HigherKinded.Class.empty' method comes from the 'Control.Applicative.Alternative' class and
corresponds to the /empty graph/. This module simply re-exports it.

* The 'edge' graph construction primitive is an alias for 'pure' of the
'Applicative' type class.

* Graph 'overlay' is an alias for '<|>' of the 'Control.Applicative.Alternative' type class.

The 'EdgeGraph' type class is characterised by the following minimal set of
axioms. In equations we use the infix operators '(EdgeGraph.Class.+++)' for 'overlay',
'(EdgeGraph.Class.>+>)' for 'into', '(EdgeGraph.Class.<+>)' for 'pits', and '(EdgeGraph.Class.>+<)' for 'tips'.

    * 'overlay' is commutative, associative, and idempotent with 'EdgeGraph.HigherKinded.Class.empty' as
      the identity:

        >         x +++ y == y +++ x
        > x +++ (y +++ z) == (x +++ y) +++ z
        >         x +++ x == x
        >     x +++ empty == x

    * 'EdgeGraph.HigherKinded.Class.empty' is the identity for 'into', 'pits', and 'tips':

        > empty >+> x == x
        > x >+> empty == x
        > empty <+> x == x
        > x <+> empty == x
        > empty >+< x == x
        > x >+< empty == x

    * 'pits' and 'tips' are commutative:

        > x <+> y == y <+> x
        > x >+< y == y >+< x

    * Decomposition: for any two connect operators @f@ and @g@ (each being
      any of '(EdgeGraph.Class.>+>)', '(EdgeGraph.Class.<+>)', or '(EdgeGraph.Class.>+<)'):

        > f x (g y z) == f x y +++ f x z +++ g y z
        > g (f x y) z == f x y +++ g x z +++ g y z

    * Reflexivity on single edges:

        > edge a <+> edge a == edge a
        > edge a >+< edge a == edge a

    * Transitivity (for non-empty @a@):

        > a <+> b +++ a <+> c == a <+> (b <+> c)
        > b >+> a +++ a <+> c == b >+> (a <+> c)
        > a >+> b +++ a >+> c == a >+> (b <+> c)
        > a >+< b +++ a >+> c == (a >+< b) >+> c
        > b >+> a +++ c >+> a == (b >+< c) >+> a
        > a >+< b +++ a >+< c == a >+< (b >+< c)

The following useful theorems can be proved from the above set of axioms.

    * Associativity of all connect operators:

        > x >+> (y >+> z) == (x >+> y) >+> z
        > x <+> (y <+> z) == (x <+> y) <+> z
        > x >+< (y >+< z) == (x >+< y) >+< z

    * Distributivity over 'overlay':

        > x >+> (y +++ z) == x >+> y +++ x >+> z
        > x <+> (y +++ z) == x <+> y +++ x <+> z
        > x >+< (y +++ z) == x >+< y +++ x >+< z

    * Absorption and saturation for each connect operator (shown for 'into'):

        > x >+> y +++ x +++ y == x >+> y
        > x >+> x == (x >+> x) >+> x

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
-- edges []  == 'EdgeGraph.HigherKinded.Class.empty'
-- edges [x] == 'edge' x
-- @
edges :: EdgeGraph g => [a] -> g a
edges = overlays . map edge

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []    == 'EdgeGraph.HigherKinded.Class.empty'
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
-- intos []    == 'EdgeGraph.HigherKinded.Class.empty'
-- intos [x]   == x
-- intos [x,y] == 'into' x y
-- @
intos :: EdgeGraph g => [g a] -> g a
intos = foldr into empty

-- | Connect (pits) a given list of graphs.
pitss :: EdgeGraph g => [g a] -> g a
pitss = foldr pits empty

-- | Connect (tips) a given list of graphs.
tipss :: EdgeGraph g => [g a] -> g a
tipss = foldr tips empty

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
-- isSubgraphOf 'EdgeGraph.HigherKinded.Class.empty'    x       == True
-- isSubgraphOf ('edge' x) 'EdgeGraph.HigherKinded.Class.empty' == False
-- isSubgraphOf x          ('overlay' x y)                      == True
-- @
isSubgraphOf :: (EdgeGraph g, Eq (g a)) => g a -> g a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'EdgeGraph.HigherKinded.Class.empty'                                                  == True
-- isEmpty ('overlay' 'EdgeGraph.HigherKinded.Class.empty' 'EdgeGraph.HigherKinded.Class.empty') == True
-- isEmpty ('edge' x)                                                                            == False
-- isEmpty ('removeEdge' x $ 'edge' x)                                                           == True
-- @
isEmpty :: EdgeGraph g => g a -> Bool
isEmpty = null

-- | Check if a graph contains a given edge. A convenient alias for 'elem'.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x 'EdgeGraph.HigherKinded.Class.empty' == False
-- hasEdge x ('edge' x)                           == True
-- hasEdge x . 'removeEdge' x                     == const False
-- @
hasEdge :: (Eq a, EdgeGraph g) => a -> g a -> Bool
hasEdge = elem

-- | The number of distinct edges in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- edgeCount 'EdgeGraph.HigherKinded.Class.empty' == 0
-- edgeCount ('edge' x)                           == 1
-- edgeCount                                      == 'length' . 'edgeList'
-- @
edgeCount :: (Ord a, EdgeGraph g) => g a -> Int
edgeCount = length . edgeList

-- | The sorted list of distinct edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeList 'EdgeGraph.HigherKinded.Class.empty' == []
-- edgeList ('edge' x)                           == [x]
-- @
edgeList :: (Ord a, EdgeGraph g) => g a -> [a]
edgeList = Set.toAscList . edgeSet

-- | The set of distinct edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeSet 'EdgeGraph.HigherKinded.Class.empty' == 'Data.Set.empty'
-- edgeSet . 'edge'                             == 'Data.Set.singleton'
-- @
edgeSet :: (Ord a, EdgeGraph g) => g a -> Set.Set a
edgeSet = foldr Set.insert Set.empty

-- | The set of edges of a given graph, specialised for graphs with
-- edges of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeIntSet 'EdgeGraph.HigherKinded.Class.empty' == 'Data.IntSet.empty'
-- edgeIntSet . 'edge'                             == 'Data.IntSet.singleton'
-- @
edgeIntSet :: EdgeGraph g => g Int -> IntSet.IntSet
edgeIntSet = foldr IntSet.insert IntSet.empty

-- | The /path/ on a list of edges, connecting consecutive edges via 'into'.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []      == 'EdgeGraph.HigherKinded.Class.empty'
-- path [x]     == 'edge' x
-- path [x,y]   == 'into' ('edge' x) ('edge' y)
-- path [x,y,z] == 'overlays' ['into' ('edge' x) ('edge' y), 'into' ('edge' y) ('edge' z)]
-- @
path :: EdgeGraph g => [a] -> g a
path []  = empty
path [x] = edge x
path xs  = overlays $ zipWith (\a b -> into (edge a) (edge b)) xs (drop 1 xs)

-- | The /circuit/ on a list of edges, connecting consecutive edges via 'into'
-- in a cycle.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []    == 'EdgeGraph.HigherKinded.Class.empty'
-- circuit [x]   == 'into' ('edge' x) ('edge' x)
-- circuit [x,y] == 'overlays' ['into' ('edge' x) ('edge' y), 'into' ('edge' y) ('edge' x)]
-- @
circuit :: EdgeGraph g => [a] -> g a
circuit []       = empty
circuit (x : xs) = overlays $ zipWith (\a b -> into (edge a) (edge b)) (x : xs) (xs ++ [x])

-- | The /clique/ on a list of edges (fully connected via 'into'). Each edge
-- connects to every later edge in the list, due to the decomposition axiom.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique []    == 'EdgeGraph.HigherKinded.Class.empty'
-- clique [x]   == 'edge' x
-- clique [x,y] == 'into' ('edge' x) ('edge' y)
-- @
clique :: EdgeGraph g => [a] -> g a
clique = intos . map edge

-- | The /biclique/ on two lists of edges. Every edge in the first list
-- connects to every edge in the second list via 'into'.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []  []  == 'EdgeGraph.HigherKinded.Class.empty'
-- biclique [x] []  == 'edge' x
-- biclique []  [y] == 'edge' y
-- @
biclique :: EdgeGraph g => [a] -> [a] -> g a
biclique xs ys = into (edges xs) (edges ys)

-- | The /flower graph/ on a list of edges. All edges are fully connected via
-- 'into' in a loop, forming petal-like structures around a central node.
-- This is equivalent to a 'clique' where the first edge is repeated at the
-- end, creating a cycle through the 'into' operator.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- flower []      == 'EdgeGraph.HigherKinded.Class.empty'
-- flower [x]     == 'into' ('edge' x) ('edge' x)
-- flower [x,y]   == 'intos' ['edge' x, 'edge' y, 'edge' x]
-- flower [x,y,z] == 'intos' ['edge' x, 'edge' y, 'edge' z, 'edge' x]
-- @
flower :: EdgeGraph g => [a] -> g a
flower []       = empty
flower (x : xs) = intos (map edge (x : xs ++ [x]))

-- | Construct a /node/ from a list of incoming edges and a list of outgoing
-- edges. The incoming edges share a common pit at the node, and the outgoing
-- edges share a common tip at the node. When one of the lists is empty, 'pits'
-- or 'tips' is used to ensure the remaining edges are still connected at the
-- node.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- node []  []    == 'EdgeGraph.HigherKinded.Class.empty'
-- node [x] []    == 'edge' x
-- node []  [y]   == 'edge' y
-- node [x] [y]   == 'into' ('edge' x) ('edge' y)
-- node [x] [y,z] == 'into' ('edge' x) ('tips' ('edge' y) ('edge' z))
-- node [x,y] [z] == 'into' ('pits' ('edge' x) ('edge' y)) ('edge' z)
-- @
node :: EdgeGraph g => [a] -> [a] -> g a
node xs ys = pitss (map edge xs) `into` tipss (map edge ys)

-- | The /tree graph/ constructed from a given 'Data.Tree.Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree.
tree :: EdgeGraph g => Tree a -> g a
tree (Node x f) = overlay (into (edge x) (edges (map rootLabel f))) (forest f)

-- | The /forest graph/ constructed from a given 'Data.Tree.Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest.
forest :: EdgeGraph g => Forest a -> g a
forest = overlays . map tree

-- | Construct a /mesh graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh xs     []  == 'EdgeGraph.HigherKinded.Class.empty'
-- mesh []     ys  == 'EdgeGraph.HigherKinded.Class.empty'
-- mesh [x]    [y] == 'edge' (x, y)
-- @
mesh :: EdgeGraph g => [a] -> [b] -> g (a, b)
mesh xs ys = path xs `box` path ys

-- | Construct a /torus graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus xs     [] == 'EdgeGraph.HigherKinded.Class.empty'
-- torus []     ys == 'EdgeGraph.HigherKinded.Class.empty'
-- @
torus :: EdgeGraph g => [a] -> [b] -> g (a, b)
torus xs ys = circuit xs `box` circuit ys

-- | Construct a /De Bruijn graph/ of given dimension and symbols of a given
-- alphabet.
-- Complexity: /O(A * D^A)/ time, memory and size, where /A/ is the size of the
-- alphabet and /D/ is the dimension of the graph.
--
-- @
-- deBruijn k [] == 'EdgeGraph.HigherKinded.Class.empty'
-- @
deBruijn :: EdgeGraph g => Int -> [a] -> g [a]
deBruijn len alphabet = skeleton >>= expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = overlays [ into (edge (Left s)) (edge (Right s)) | s <- overlaps ]
    expand v = edges [ either ([a] ++) (++ [a]) v | a <- alphabet ]

-- | Remove all occurrences of an edge from the graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x ('edge' x)     == 'EdgeGraph.HigherKinded.Class.empty'
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
-- replaceEdge x y ('edge' x) == 'edge' y
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
-- splitEdge x []  == 'removeEdge' x
-- splitEdge x [x] == id
-- splitEdge x [y] == 'replaceEdge' x y
-- @
splitEdge :: (Eq a, EdgeGraph g) => a -> [a] -> g a -> g a
splitEdge v us g = g >>= \w -> if w == v then edges us else edge w

-- | Construct the /induced subgraph/ of a given graph by removing edges
-- that do not satisfy a given predicate.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- induce (const True)  x == x
-- induce (const False) x == 'EdgeGraph.HigherKinded.Class.empty'
-- induce (/= x)          == 'removeEdge' x
-- induce p . induce q    == induce (\\x -> p x && q x)
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
-- graphs as /identities/ and 'EdgeGraph.HigherKinded.Class.empty' as the /annihilating zero/. Below @~~@
-- stands for the equality up to an isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y                                    ~~ box y x
-- box x (box y z)                            ~~ box (box x y) z
-- box x ('overlay' y z)                      == 'overlay' (box x y) (box x z)
-- box x ('edge' ())                          ~~ x
-- box x 'EdgeGraph.HigherKinded.Class.empty' ~~ 'EdgeGraph.HigherKinded.Class.empty'
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
