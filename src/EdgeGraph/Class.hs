{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.Class
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the core type class 'EdgeGraph', a few graph subclasses,
-- and basic polymorphic graph construction primitives. Functions that cannot be
-- implemented fully polymorphically and require the use of an intermediate data
-- type are not included. For example, to compute the number of edges in an
-- 'EdgeGraph' expression you will need to use a concrete data type, such as
-- "EdgeGraph.Fold". Other useful 'EdgeGraph' instances are defined in
-- "EdgeGraph", "EdgeGraph.AdjacencyMap" and "EdgeGraph.Incidence".
--
-- See "EdgeGraph.HigherKinded.Class" for the higher-kinded version of the
-- core graph type class.
-----------------------------------------------------------------------------
module EdgeGraph.Class (
    -- * The core type class
    EdgeGraph (..),

    -- * Operators
    (+++), (>+>), (<+>), (>+<),

    -- * Basic graph construction primitives
    edges, overlays, intos, pitss, tipss,

    -- * Comparisons
    isSubgraphOf,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest,

    -- * Conversion between graph data types
    ToEdgeGraph (..)
  ) where

import Data.Tree

{-|
The core type class for constructing algebraic edge graphs, characterised by the
following minimal set of axioms. In equations we use @+@ and @*@ as convenient
shortcuts for 'overlay' and 'into', respectively.

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
class EdgeGraph g where
    -- | The type of graph edges.
    type Edge g
    -- | Construct the empty graph.
    empty :: g
    -- | Construct the graph with a single edge.
    edge :: Edge g -> g
    -- | Overlay two graphs.
    overlay :: g -> g -> g
    -- | Connect two graphs sequentially (pits of left to tips of right).
    into :: g -> g -> g
    -- | Connect two graphs at pits (where outgoing edges overlap).
    pits :: g -> g -> g
    -- | Connect two graphs at tips (where incoming edges overlap).
    tips :: g -> g -> g

-- | Infix operator for 'overlay'.
(+++) :: EdgeGraph g => g -> g -> g
(+++) = overlay
infixl 6 +++

-- | Infix operator for 'into'.
(>+>) :: EdgeGraph g => g -> g -> g
(>+>) = into
infixl 7 >+>

-- | Infix operator for 'pits'.
(<+>) :: EdgeGraph g => g -> g -> g
(<+>) = pits
infixl 7 <+>

-- | Infix operator for 'tips'.
(>+<) :: EdgeGraph g => g -> g -> g
(>+<) = tips
infixl 7 >+<

instance EdgeGraph () where
    type Edge () = ()
    empty       = ()
    edge  _     = ()
    overlay _ _ = ()
    into    _ _ = ()
    pits    _ _ = ()
    tips    _ _ = ()

-- Note: Maybe g and (a -> g) instances are identical and use the Applicative's
-- pure and <*>. We do not provide a general instance for all Applicative
-- functors because that would lead to overlapping instances.
instance EdgeGraph g => EdgeGraph (Maybe g) where
    type Edge (Maybe g) = Edge g
    empty       = pure empty
    edge        = pure . edge
    overlay x y = overlay <$> x <*> y
    into    x y = into    <$> x <*> y
    pits    x y = pits    <$> x <*> y
    tips    x y = tips    <$> x <*> y

instance EdgeGraph g => EdgeGraph (a -> g) where
    type Edge (a -> g) = Edge g
    empty       = pure empty
    edge        = pure . edge
    overlay x y = overlay <$> x <*> y
    into    x y = into    <$> x <*> y
    pits    x y = pits    <$> x <*> y
    tips    x y = tips    <$> x <*> y

instance (EdgeGraph g, EdgeGraph h) => EdgeGraph (g, h) where
    type Edge (g, h)          = (Edge g     , Edge h     )
    empty                     = (empty      , empty      )
    edge  (x,  y )            = (edge  x    , edge  y    )
    overlay (x1, y1) (x2, y2) = (overlay x1 x2, overlay y1 y2)
    into    (x1, y1) (x2, y2) = (into    x1 x2, into    y1 y2)
    pits    (x1, y1) (x2, y2) = (pits    x1 x2, pits    y1 y2)
    tips    (x1, y1) (x2, y2) = (tips    x1 x2, tips    y1 y2)

instance (EdgeGraph g, EdgeGraph h, EdgeGraph i) => EdgeGraph (g, h, i) where
    type Edge (g, h, i)               = (Edge g     , Edge h     , Edge i     )
    empty                             = (empty      , empty      , empty      )
    edge  (x,  y , z )                = (edge  x    , edge  y    , edge  z    )
    overlay (x1, y1, z1) (x2, y2, z2) = (overlay x1 x2, overlay y1 y2, overlay z1 z2)
    into    (x1, y1, z1) (x2, y2, z2) = (into    x1 x2, into    y1 y2, into    z1 z2)
    pits    (x1, y1, z1) (x2, y2, z2) = (pits    x1 x2, pits    y1 y2, pits    z1 z2)
    tips    (x1, y1, z1) (x2, y2, z2) = (tips    x1 x2, tips    y1 y2, tips    z1 z2)

-- | Construct the graph comprising a given list of isolated edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []  == 'empty'
-- edges [x] == 'edge' x
-- @
edges :: EdgeGraph g => [Edge g] -> g
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
overlays :: EdgeGraph g => [g] -> g
overlays = foldr overlay empty

-- | Connect (into) a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- intos []    == 'empty'
-- intos [x]   == x
-- intos [x,y] == 'into' x y
-- @
intos :: EdgeGraph g => [g] -> g
intos = foldr into empty

-- | Connect (pits) a given list of graphs.
pitss :: EdgeGraph g => [g] -> g
pitss = foldr pits empty

-- | Connect (tips) a given list of graphs.
tipss :: EdgeGraph g => [g] -> g
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
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('edge' x)      'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- @
isSubgraphOf :: (EdgeGraph g, Eq g) => g -> g -> Bool
isSubgraphOf x y = overlay x y == y

-- | The /path/ on a list of edges, using 'into' as the connect operator.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'edge' x
-- path [x,y] == 'into' ('edge' x) ('edge' y)
-- @
path :: EdgeGraph g => [Edge g] -> g
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
circuit :: EdgeGraph g => [Edge g] -> g
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
clique :: EdgeGraph g => [Edge g] -> g
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
biclique :: EdgeGraph g => [Edge g] -> [Edge g] -> g
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
star :: EdgeGraph g => Edge g -> [Edge g] -> g
star x ys = into (edge x) (edges ys)

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree.
tree :: EdgeGraph g => Tree (Edge g) -> g
tree (Node x f) = overlay (star x $ map rootLabel f) (forest f)

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest.
forest :: EdgeGraph g => Forest (Edge g) -> g
forest = overlays . map tree

-- | The 'ToEdgeGraph' type class captures data types that can be converted to
-- polymorphic graph expressions. The conversion method 'toEdgeGraph' semantically
-- acts as the identity on graph data structures, but allows to convert graphs
-- between different data representations.
class ToEdgeGraph t where
    type ToEdge t
    toEdgeGraph :: (EdgeGraph g, Edge g ~ ToEdge t) => t -> g
