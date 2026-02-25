{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph
-- License    : MIT (see the file LICENSE)
-- Maintainer : jackliellcock@gmail.com
-- Stability  : experimental
--
-- This module defines the core data type 'EdgeGraph' for algebraic edge graphs
-- and associated algorithms. 'EdgeGraph' is a deep embedding of the six
-- algebraic edge graph construction primitives: 'empty', 'edge', 'overlay',
-- 'into', 'pits', and 'tips'.
--
-- 'EdgeGraph' is an instance of the type class defined in "EdgeGraph.Class",
-- which can be used for polymorphic edge graph construction and manipulation.
--
-- The 'Eq' instance is implemented using the 'I.Incidence' as the /canonical
-- graph representation/ and satisfies all axioms of algebraic edge graphs.
--
-----------------------------------------------------------------------------
module EdgeGraph (
    -- * Algebraic data type for edge graphs
    EdgeGraph (..),

    -- * Basic graph construction primitives
    empty, edge, overlay, into, pits, tips, edges, overlays, intos,

    -- * Graph folding
    foldg,

    -- * Comparisons
    isSubgraphOf, (===),

    -- * Graph properties
    isEmpty, size, hasEdge, edgeCount, edgeList, edgeSet,
    edgeIntSet, nodeCount, nodeList, nodeSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest, mesh, torus, deBruijn,

    -- * Graph transformation
    removeEdge, replaceEdge, mergeEdges, splitEdge,
    transpose, induce, simplify,

    -- * Graph composition
    box
  ) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad

import qualified EdgeGraph.Class              as C
import qualified EdgeGraph.HigherKinded.Class as H
import qualified EdgeGraph.Incidence          as I
import qualified Data.IntSet                  as IntSet
import qualified Data.Set                     as Set
import qualified Data.Tree                    as Tree

{-| The 'EdgeGraph' datatype is a deep embedding of the core edge graph
construction primitives 'empty', 'edge', 'overlay', 'into', 'pits' and 'tips'.
The 'Eq' instance is implemented using the 'I.Incidence' as the /canonical
graph representation/ and satisfies all axioms of algebraic edge graphs:

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

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'into':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /s/ will
denote the /size/ of the corresponding 'EdgeGraph' expression.

Note that 'size' is slightly different from the 'length' method of the
'Foldable' type class, as the latter does not count 'Empty' leaves of the
expression:

@'length' 'empty'            == 0
'size'   'empty'             == 1
'length' ('edge' x)          == 1
'size'   ('edge' x)          == 1
'length' ('empty' + 'empty') == 0
'size'   ('empty' + 'empty') == 2@

The 'size' of any graph is positive, and the difference @('size' g - 'length' g)@
corresponds to the number of occurrences of 'empty' in an expression @g@.
-}
data EdgeGraph a = Empty
                 | Edge a
                 | EdgeGraph a :++: EdgeGraph a   -- ^ Overlay
                 | EdgeGraph a :>>: EdgeGraph a   -- ^ Into
                 | EdgeGraph a :<>: EdgeGraph a   -- ^ Pits
                 | EdgeGraph a :><: EdgeGraph a   -- ^ Tips
                 deriving (Foldable, Functor, Show, Traversable)

infixl 6 :++:
infixl 7 :>>:
infixl 7 :<>:
infixl 7 :><:

instance C.EdgeGraph (EdgeGraph a) where
    type Edge (EdgeGraph a) = a
    empty   = empty
    edge    = edge
    overlay = overlay
    into    = into
    pits    = pits
    tips    = tips

instance C.ToEdgeGraph (EdgeGraph a) where
    type ToEdge (EdgeGraph a) = a
    toEdgeGraph = foldg C.empty C.edge C.overlay C.into C.pits C.tips

instance H.ToEdgeGraph EdgeGraph where
    toEdgeGraph = foldg H.empty H.edge H.overlay H.into H.pits H.tips

instance H.EdgeGraph EdgeGraph where
    into = (:>>:)
    pits = (:<>:)
    tips = (:><:)

instance Ord a => Eq (EdgeGraph a) where
    x == y = C.toEdgeGraph x == (C.toEdgeGraph y :: I.Incidence a)

instance Applicative EdgeGraph where
    pure  = Edge
    (<*>) = ap

instance Monad EdgeGraph where
    return  = pure
    g >>= f = foldg Empty f (:++:) (:>>:) (:<>:) (:><:) g

instance Alternative EdgeGraph where
    empty = Empty
    (<|>) = (:++:)

instance MonadPlus EdgeGraph where
    mzero = Empty
    mplus = (:++:)

-- | Construct the /empty graph/. An alias for the constructor 'Empty'.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'   empty == True
-- 'hasEdge' x empty == False
-- 'size'      empty == 1
-- @
empty :: EdgeGraph a
empty = Empty

-- | Construct the graph comprising /a single edge/. An alias for the
-- constructor 'Edge'.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'   (edge x) == False
-- 'hasEdge' x (edge x) == True
-- 'hasEdge' 1 (edge 2) == False
-- 'size'      (edge x) == 1
-- @
edge :: a -> EdgeGraph a
edge = Edge

-- | /Overlay/ two graphs. An alias for the constructor ':++:'. This is an
-- idempotent, commutative and associative operation with the identity 'empty'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty'   (overlay x y) == 'isEmpty' x   && 'isEmpty' y
-- 'size'      (overlay x y) == 'size' x      + 'size' y
-- @
overlay :: EdgeGraph a -> EdgeGraph a -> EdgeGraph a
overlay = (:++:)

-- | /Into/ two graphs. An alias for the constructor ':>>:'. Connects the pits
-- of the left graph to the tips of the right graph. This is an associative
-- operation with the identity 'empty', which distributes over 'overlay' and
-- obeys the decomposition axiom.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty'   (into x y) == 'isEmpty' x && 'isEmpty' y
-- 'size'      (into x y) == 'size' x    + 'size' y
-- @
into :: EdgeGraph a -> EdgeGraph a -> EdgeGraph a
into = (:>>:)

-- | /Pits/ two graphs. An alias for the constructor ':<>:'. Connects nodes
-- where outgoing edges (pits) overlap. This is an associative operation with
-- the identity 'empty', which distributes over 'overlay'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty' (pits x y) == 'isEmpty' x && 'isEmpty' y
-- 'size'    (pits x y) == 'size' x    + 'size' y
-- @
pits :: EdgeGraph a -> EdgeGraph a -> EdgeGraph a
pits = (:<>:)

-- | /Tips/ two graphs. An alias for the constructor ':><:'. Connects nodes
-- where incoming edges (tips) overlap. This is an associative operation with
-- the identity 'empty', which distributes over 'overlay'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty' (tips x y) == 'isEmpty' x && 'isEmpty' y
-- 'size'    (tips x y) == 'size' x    + 'size' y
-- @
tips :: EdgeGraph a -> EdgeGraph a -> EdgeGraph a
tips = (:><:)

-- | Construct the graph comprising a given list of isolated edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []  == 'empty'
-- edges [x] == 'edge' x
-- @
edges :: [a] -> EdgeGraph a
edges = C.edges

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []    == 'empty'
-- overlays [x]   == x
-- overlays [x,y] == 'overlay' x y
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: [EdgeGraph a] -> EdgeGraph a
overlays = C.overlays

-- | Connect (into) a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- intos []    == 'empty'
-- intos [x]   == x
-- intos [x,y] == 'into' x y
-- 'isEmpty' . intos == 'all' 'isEmpty'
-- @
intos :: [EdgeGraph a] -> EdgeGraph a
intos = C.intos

-- | Generalised 'EdgeGraph' folding: recursively collapse an 'EdgeGraph' by
-- applying the provided functions to the leaves and internal nodes of the
-- expression. The order of arguments is: empty, edge, overlay, into, pits,
-- tips.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg 'empty' 'edge' 'overlay' 'into' 'pits' 'tips' == id
-- foldg []    (\\x -> [x])  (++)  (++) (++) (++)      == 'Data.Foldable.toList'
-- foldg 0     (const 1)     (+)   (+)  (+)  (+)       == 'Data.Foldable.length'
-- foldg 1     (const 1)     (+)   (+)  (+)  (+)       == 'size'
-- foldg True  (const False) (&&)  (&&) (&&) (&&)      == 'isEmpty'
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> EdgeGraph a -> b
foldg e v o c p t = go
  where
    go Empty       = e
    go (Edge x)    = v x
    go (x :++: y)  = o (go x) (go y)
    go (x :>>: y)  = c (go x) (go y)
    go (x :<>: y)  = p (go x) (go y)
    go (x :><: y)  = t (go x) (go y)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time, where /m/ is the number of nodes in
-- the canonical incidence representation.
--
-- @
-- isSubgraphOf 'empty'    x               == True
-- isSubgraphOf ('edge' x) 'empty'         == False
-- isSubgraphOf x          ('overlay' x y) == True
-- @
isSubgraphOf :: Ord a => EdgeGraph a -> EdgeGraph a -> Bool
isSubgraphOf x y = I.isSubgraphOf (C.toEdgeGraph x) (C.toEdgeGraph y)

-- | Structural equality on graph expressions. Unlike '==', this function does
-- not perform any canonicalisation and compares expressions as-is.
-- Complexity: /O(s)/ time.
--
-- @
--     x === x           == True
--     x === x + 'empty' == False
-- x + y === x + y       == True
-- 1 + 2 === 2 + 1       == False
-- x + y === x * y       == False
-- @
(===) :: Eq a => EdgeGraph a -> EdgeGraph a -> Bool
Empty        === Empty        = True
(Edge x)     === (Edge y)     = x == y
(x1 :++: y1) === (x2 :++: y2) = x1 === x2 && y1 === y2
(x1 :>>: y1) === (x2 :>>: y2) = x1 === x2 && y1 === y2
(x1 :<>: y1) === (x2 :<>: y2) = x1 === x2 && y1 === y2
(x1 :><: y1) === (x2 :><: y2) = x1 === x2 && y1 === y2
_            === _            = False

infix 4 ===

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                     == True
-- isEmpty ('overlay' 'empty' 'empty') == True
-- isEmpty ('edge' x)                  == False
-- isEmpty ('removeEdge' x $ 'edge' x) == True
-- @
isEmpty :: EdgeGraph a -> Bool
isEmpty = null

-- | The /size/ of a graph, i.e. the number of leaves of the expression
-- including 'Empty' leaves.
-- Complexity: /O(s)/ time.
--
-- @
-- size 'empty'         == 1
-- size ('edge' x)      == 1
-- size ('overlay' x y) == size x + size y
-- size ('into' x y)    == size x + size y
-- size ('pits' x y)    == size x + size y
-- size ('tips' x y)    == size x + size y
-- size x               >= 1
-- @
size :: EdgeGraph a -> Int
size = foldg 1 (const 1) (+) (+) (+) (+)

-- | Check if a graph contains a given edge. A convenient alias for 'elem'.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x 'empty'          == False
-- hasEdge x ('edge' x)       == True
-- hasEdge x . 'removeEdge' x == const False
-- @
hasEdge :: Eq a => a -> EdgeGraph a -> Bool
hasEdge = elem

-- | The number of distinct edges in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- edgeCount 'empty'    == 0
-- edgeCount ('edge' x) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => EdgeGraph a -> Int
edgeCount = I.edgeCount . C.toEdgeGraph

-- | The sorted list of distinct edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeList 'empty'    == []
-- edgeList ('edge' x) == [x]
-- edgeList . 'edges'  == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: Ord a => EdgeGraph a -> [a]
edgeList = Set.toAscList . edgeSet

-- | The set of edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeSet 'empty'   == Set.'Set.empty'
-- edgeSet . 'edge'  == Set.'Set.singleton'
-- edgeSet . 'edges' == Set.'Set.fromList'
-- @
edgeSet :: Ord a => EdgeGraph a -> Set.Set a
edgeSet = foldr Set.insert Set.empty

-- | The set of edges of a given graph. Like 'edgeSet' but
-- specialised for graphs with edges of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeIntSet 'empty'   == IntSet.'IntSet.empty'
-- edgeIntSet . 'edge'  == IntSet.'IntSet.singleton'
-- edgeIntSet . 'edges' == IntSet.'IntSet.fromList'
-- @
edgeIntSet :: EdgeGraph Int -> IntSet.IntSet
edgeIntSet = foldr IntSet.insert IntSet.empty

-- | The number of nodes in the canonical incidence representation.
-- Complexity: /O(s + n * log(n))/ time.
--
-- @
-- nodeCount 'empty'    == 0
-- nodeCount ('edge' x) == 2
-- @
nodeCount :: Ord a => EdgeGraph a -> Int
nodeCount = I.nodeCount . C.toEdgeGraph

-- | The sorted list of nodes in the canonical incidence representation.
-- Complexity: /O(s + n * log(n))/ time and /O(n)/ memory.
--
-- @
-- nodeList 'empty'    == []
-- nodeList ('edge' x) == ['I.Node' (Set.'Set.singleton' x) (Set.'Set.singleton' x)]
-- @
nodeList :: Ord a => EdgeGraph a -> [I.Node a]
nodeList = I.nodeList . C.toEdgeGraph

-- | The set of nodes in the canonical incidence representation.
-- Complexity: /O(s + n * log(n))/ time and /O(n)/ memory.
--
-- @
-- nodeSet 'empty' == Set.'Set.empty'
-- @
nodeSet :: Ord a => EdgeGraph a -> Set.Set (I.Node a)
nodeSet = I.nodeSet . C.toEdgeGraph

-- | The /path/ on a list of edges, using 'into' as the connect operator.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'edge' x
-- path [x,y] == 'into' ('edge' x) ('edge' y)
-- @
path :: [a] -> EdgeGraph a
path = C.path

-- | The /circuit/ on a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'into' ('edge' x) ('edge' x)
-- circuit [x,y] == 'into' ('edge' x) ('into' ('edge' y) ('edge' x))
-- @
circuit :: [a] -> EdgeGraph a
circuit = C.circuit

-- | The /clique/ on a list of edges (fully connected via 'into').
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique []    == 'empty'
-- clique [x]   == 'edge' x
-- clique [x,y] == 'into' ('edge' x) ('edge' y)
-- @
clique :: [a] -> EdgeGraph a
clique = C.clique

-- | The /biclique/ on two lists of edges.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []  []  == 'empty'
-- biclique [x] []  == 'edge' x
-- biclique []  [y] == 'edge' y
-- @
biclique :: [a] -> [a] -> EdgeGraph a
biclique = C.biclique

-- | The /star/ formed by a centre edge and a list of leaf edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'edge' x
-- star x [y]   == 'into' ('edge' x) ('edge' y)
-- star x [y,z] == 'into' ('edge' x) ('edges' [y, z])
-- @
star :: a -> [a] -> EdgeGraph a
star = C.star

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree (i.e. the number of vertices in the tree).
tree :: Tree.Tree a -> EdgeGraph a
tree = C.tree

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest (i.e. the number of vertices in the forest).
forest :: Tree.Forest a -> EdgeGraph a
forest = C.forest

-- | Construct a /mesh graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh xs  []  == 'empty'
-- mesh []  ys  == 'empty'
-- mesh [x] [y] == 'edge' (x, y)
-- @
mesh :: [a] -> [b] -> EdgeGraph (a, b)
mesh = H.mesh

-- | Construct a /torus graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus xs  []  == 'empty'
-- torus []  ys  == 'empty'
-- torus [x] [y] == 'edge' (x, y) :>>: 'edge' (x, y)
-- @
torus :: [a] -> [b] -> EdgeGraph (a, b)
torus = H.torus

-- | Construct a /De Bruijn graph/ of given dimension and symbols of a given
-- alphabet.
-- Complexity: /O(A * D^A)/ time, memory and size, where /A/ is the size of the
-- alphabet and /D/ is the dimension of the graph.
--
-- @
-- deBruijn k []    == 'empty'
-- deBruijn 1 [0,1] == 'edges' [ [0], [0], [1], [1] ]
-- @
deBruijn :: Int -> [a] -> EdgeGraph [a]
deBruijn = H.deBruijn

-- | Remove all occurrences of an edge from the given graph. Edges
-- that are removed leave behind 'empty'.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x ('edge' x)              == 'empty'
-- removeEdge x . removeEdge x   == removeEdge x
-- @
removeEdge :: Eq a => a -> EdgeGraph a -> EdgeGraph a
removeEdge x = induce (/= x)

-- | The function @replaceEdge x y@ replaces edge @x@ with edge
-- label @y@ in a given 'EdgeGraph'. If @y@ already exists, @x@ and @y@ will
-- be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceEdge x x            == id
-- replaceEdge x y ('edge' x)   == 'edge' y
-- replaceEdge x y            == 'mergeEdges' (== x) y
-- @
replaceEdge :: Eq a => a -> a -> EdgeGraph a -> EdgeGraph a
replaceEdge u v = fmap $ \w -> if w == u then v else w

-- | Merge edges satisfying a given predicate with a given edge.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeEdges (const False) x == id
-- mergeEdges (== x) y        == 'replaceEdge' x y
-- @
mergeEdges :: Eq a => (a -> Bool) -> a -> EdgeGraph a -> EdgeGraph a
mergeEdges p v = fmap $ \w -> if p w then v else w

-- | Split an edge into a list of edges with the same connectivity.
-- Complexity: /O(s + k * L)/ time, memory and size, where /k/ is the number of
-- occurrences of the edge in the expression and /L/ is the length of the
-- given list.
--
-- @
-- splitEdge x []    == 'removeEdge' x
-- splitEdge x [x]   == id
-- splitEdge x [y]   == 'replaceEdge' x y
-- @
splitEdge :: Eq a => a -> [a] -> EdgeGraph a -> EdgeGraph a
splitEdge v us g = g >>= \w -> if w == v then edges us else edge w

-- | Transpose a given graph. This operation flips the direction of 'into' and
-- swaps 'pits' and 'tips'.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- transpose 'empty'     == 'empty'
-- transpose ('edge' x)  == 'edge' x
-- transpose . transpose == id
-- @
transpose :: EdgeGraph a -> EdgeGraph a
transpose = foldg Empty Edge (:++:) (flip (:>>:)) (:><:) (:<>:)

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
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> EdgeGraph a -> EdgeGraph a
induce p = foldg Empty (\x -> if p x then Edge x else Empty) (:++:) (:>>:) (:<>:) (:><:)

-- | Simplify a given graph. Semantically, this is the identity function, but
-- it simplifies a given edge graph expression according to the laws of the
-- algebra. The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
-- Complexity: the function performs /O(s)/ graph comparisons. It is guaranteed
-- that the size of the result does not exceed the size of the given expression.
--
-- @
-- simplify x              == x
-- 'size' (simplify x)       <= 'size' x
-- simplify 'empty'         '===' 'empty'
-- simplify ('edge' 1)      '===' 'edge' 1
-- simplify ('edge' 1 + 'edge' 1) '===' 'edge' 1
-- @
simplify :: Ord a => EdgeGraph a -> EdgeGraph a
simplify = foldg Empty Edge (simple (:++:)) (simple (:>>:)) (simple (:<>:)) (simple (:><:))

simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where z = op x y

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O(s1 * s2)/ time, memory and size, where /s1/ and /s2/ are the
-- sizes of the given graphs.
--
-- @
-- box ('path' [0,1]) ('path' "ab") == 'edges' [ ((0,\'a\'),(0,\'b\')), ((0,\'a\'),(1,\'a\'))
--                                             , ((0,\'b\'),(1,\'b\')), ((1,\'a\'),(1,\'b\')) ]
-- @
-- Up to an isomorphism between the resulting edge types, this operation
-- is /commutative/, /associative/, /distributes/ over 'overlay', has singleton
-- graphs as /identities/ and 'empty' as the /annihilating zero/. Below @~~@
-- stands for the equality up to an isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y               ~~ box y x
-- box x (box y z)       ~~ box (box x y) z
-- box x ('overlay' y z) == 'overlay' (box x y) (box x z)
-- box x ('edge' ())     ~~ x
-- box x 'empty'         ~~ 'empty'
-- @
box :: EdgeGraph a -> EdgeGraph b -> EdgeGraph (a, b)
box = H.box
