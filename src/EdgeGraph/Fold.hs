{-# LANGUAGE RankNTypes, TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.Fold
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the 'Fold' data type -- the Boehm-Berarducci encoding of
-- algebraic edge graphs, which is used for generalised graph folding and for the
-- implementation of polymorphic graph construction and transformation algorithms.
-- 'Fold' is an instance of type classes defined in modules "EdgeGraph.Class"
-- and "EdgeGraph.HigherKinded.Class", which can be used for polymorphic
-- graph construction and manipulation.
--
-- The encoding uses six parameters corresponding to the six primitives of
-- algebraic edge graphs: 'empty', 'edge', 'overlay', 'into', 'pits' and 'tips'.
-----------------------------------------------------------------------------
module EdgeGraph.Fold (
    -- * Boehm-Berarducci encoding of algebraic edge graphs
    Fold,

    -- * Basic graph construction primitives
    empty, edge, overlay, into, pits, tips, edges, overlays, intos,
    C.path, C.circuit, C.clique, C.biclique, C.star, C.tree, C.forest,

    -- * Graph folding
    foldg,

    -- * Comparisons
    C.isSubgraphOf,

    -- * Graph properties
    isEmpty, size, hasEdge, edgeCount, edgeList, edgeSet,
    edgeIntSet, nodeCount, nodeList, nodeSet,

    -- * Standard families of graphs
    mesh, torus, deBruijn,

    -- * Graph transformation
    removeEdge, replaceEdge, mergeEdges, splitEdge,
    transpose, gmap, bind, induce, simplify,

    -- * Graph composition
    box
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Foldable (toList)

import qualified EdgeGraph.Class              as C
import qualified EdgeGraph.HigherKinded.Class as H
import qualified EdgeGraph.Incidence          as I
import qualified Data.IntSet                  as IntSet
import qualified Data.Set                     as Set

{-| The 'Fold' datatype is the Boehm-Berarducci encoding of the core edge graph
construction primitives 'empty', 'edge', 'overlay', 'into', 'pits' and 'tips'.
We define a law-abiding 'Num' instance as a convenient notation for working
with graphs:

    > 0           == edge 0
    > 1 + 2       == overlay (edge 1) (edge 2)
    > 1 * 2       == into (edge 1) (edge 2)
    > 1 + 2 * 3   == overlay (edge 1) (into (edge 2) (edge 3))
    > 1 * (2 + 3) == into (edge 1) (overlay (edge 2) (edge 3))

The 'Show' instance is defined using basic graph construction primitives:

@show ('empty'     :: Fold Int) == "empty"
show (1         :: Fold Int) == "edge 1"
show (1 + 2     :: Fold Int) == "edges [1,2]"
show (1 * 2     :: Fold Int) == "into (edge 1) (edge 2)"@

The 'Eq' instance is currently implemented using the 'I.Incidence' as the
/canonical graph representation/ and satisfies all axioms of algebraic edge
graphs:

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

When specifying the time and memory complexity of graph algorithms, /n/ will
denote the number of edges in the graph, /m/ will denote the number of
nodes in the graph, and /s/ will denote the /size/ of the corresponding
graph expression. For example, if g is a 'Fold' then /n/, /m/ and /s/ can be
computed as follows:

@n == 'edgeCount' g
m == 'nodeCount' g
s == 'size' g@

Note that 'size' is slightly different from the 'length' method of the
'Foldable' type class, as the latter does not count 'empty' leaves of the
expression:

@'length' 'empty'           == 0
'size'   'empty'           == 1
'length' ('edge' x)        == 1
'size'   ('edge' x)        == 1
'length' ('empty' + 'empty') == 0
'size'   ('empty' + 'empty') == 2@

The 'size' of any graph is positive, and the difference @('size' g - 'length' g)@
corresponds to the number of occurrences of 'empty' in an expression @g@.

Converting a 'Fold' to the corresponding 'I.Incidence' takes /O(s + m * log(m))/
time and /O(s + m)/ memory. This is also the complexity of the graph equality
test, because it is currently implemented by converting graph expressions to
canonical representations based on incidences.
-}
newtype Fold a = Fold { runFold :: forall b. b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> b }

instance (Ord a, Show a) => Show (Fold a) where
    show f = show (C.toEdgeGraph f :: I.Incidence a)

instance Ord a => Eq (Fold a) where
    x == y = C.toEdgeGraph x == (C.toEdgeGraph y :: I.Incidence a)

instance C.EdgeGraph (Fold a) where
    type Edge (Fold a) = a
    empty       = Fold $ \e _ _ _ _ _ -> e
    edge x      = Fold $ \_ v _ _ _ _ -> v x
    overlay x y = Fold $ \e v o i p t -> runFold x e v o i p t `o` runFold y e v o i p t
    into    x y = Fold $ \e v o i p t -> runFold x e v o i p t `i` runFold y e v o i p t
    pits    x y = Fold $ \e v o i p t -> runFold x e v o i p t `p` runFold y e v o i p t
    tips    x y = Fold $ \e v o i p t -> runFold x e v o i p t `t` runFold y e v o i p t

instance Num a => Num (Fold a) where
    fromInteger = C.edge . fromInteger
    (+)         = C.overlay
    (*)         = C.into
    signum      = const C.empty
    abs         = id
    negate      = id

instance Functor Fold where
    fmap = gmap

instance Applicative Fold where
    pure  = C.edge
    (<*>) = ap

instance Alternative Fold where
    empty = C.empty
    (<|>) = C.overlay

instance MonadPlus Fold where
    mzero = C.empty
    mplus = C.overlay

instance Monad Fold where
    (>>=)  = bind

instance H.EdgeGraph Fold where
    into = C.into
    pits = C.pits
    tips = C.tips

instance Foldable Fold where
    foldMap f = foldg mempty f mappend mappend mappend mappend

instance Traversable Fold where
    traverse f = foldg (pure C.empty) (fmap C.edge . f) (liftA2 C.overlay) (liftA2 C.into) (liftA2 C.pits) (liftA2 C.tips)

instance C.ToEdgeGraph (Fold a) where
    type ToEdge (Fold a) = a
    toEdgeGraph = foldg C.empty C.edge C.overlay C.into C.pits C.tips

instance H.ToEdgeGraph Fold where
    toEdgeGraph = foldg H.empty H.edge H.overlay H.into H.pits H.tips

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasEdge' x   empty == False
-- 'size'        empty == 1
-- @
empty :: C.EdgeGraph g => g
empty = C.empty

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- 'isEmpty'     (edge x) == False
-- 'hasEdge' x   (edge x) == True
-- 'hasEdge' 1   (edge 2) == False
-- 'size'        (edge x) == 1
-- @
edge :: C.EdgeGraph g => C.Edge g -> g
edge = C.edge

-- | /Overlay/ two graphs. This is an idempotent, commutative and associative
-- operation with the identity 'empty'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'size'        (overlay x y) == 'size' x        + 'size' y
-- @
overlay :: C.EdgeGraph g => g -> g -> g
overlay = C.overlay

-- | /Into/ two graphs. Connects the pits of the left graph to the tips of the
-- right graph. This is an associative operation with the identity 'empty',
-- which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
--
-- @
-- 'isEmpty'     (into x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'size'        (into x y) == 'size' x        + 'size' y
-- @
into :: C.EdgeGraph g => g -> g -> g
into = C.into

-- | /Pits/ two graphs. Connects where outgoing edges (pits) overlap.
-- This is an associative operation with the identity 'empty'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
pits :: C.EdgeGraph g => g -> g -> g
pits = C.pits

-- | /Tips/ two graphs. Connects where incoming edges (tips) overlap.
-- This is an associative operation with the identity 'empty'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
tips :: C.EdgeGraph g => g -> g -> g
tips = C.tips

-- | Construct the graph comprising a given list of isolated edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []  == 'empty'
-- edges [x] == 'edge' x
-- @
edges :: C.EdgeGraph g => [C.Edge g] -> g
edges = C.edges

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []    == 'empty'
-- overlays [x]   == x
-- overlays [x,y] == 'overlay' x y
-- @
overlays :: C.EdgeGraph g => [g] -> g
overlays = C.overlays

-- | Connect (into) a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- intos []    == 'empty'
-- intos [x]   == x
-- intos [x,y] == 'into' x y
-- @
intos :: C.EdgeGraph g => [g] -> g
intos = C.intos

-- | Generalised graph folding: recursively collapse a 'Fold' by applying
-- the provided functions to the leaves and internal nodes of the expression.
-- The order of arguments is: empty, edge, overlay, into, pits, tips.
-- Complexity: /O(s)/ applications of given functions. As an example, the
-- complexity of 'size' is /O(s)/, since all functions have cost /O(1)/.
--
-- @
-- foldg 'empty' 'edge'        'overlay' 'into' 'pits' 'tips'  == id
-- foldg 'empty' 'edge'        'overlay' (flip 'into') 'tips' 'pits' == 'transpose'
-- foldg []    return        (++)    (++) (++) (++)    == 'Data.Foldable.toList'
-- foldg 0     (const 1)     (+)     (+)  (+)  (+)     == 'Data.Foldable.length'
-- foldg 1     (const 1)     (+)     (+)  (+)  (+)     == 'size'
-- foldg True  (const False) (&&)    (&&) (&&) (&&)    == 'isEmpty'
-- @
foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Fold a -> b
foldg e v o i p t g = runFold g e v o i p t

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('edge' x)                    == False
-- isEmpty ('removeEdge' x $ 'edge' x) == True
-- @
isEmpty :: Fold a -> Bool
isEmpty = H.isEmpty

-- | The /size/ of a graph, i.e. the number of leaves of the expression
-- including 'empty' leaves.
-- Complexity: /O(s)/ time.
--
-- @
-- size 'empty'         == 1
-- size ('edge' x)      == 1
-- size ('overlay' x y) == size x + size y
-- size ('into' x y)    == size x + size y
-- size x             >= 1
-- @
size :: Fold a -> Int
size = foldg 1 (const 1) (+) (+) (+) (+)

-- | Check if a graph contains a given edge. A convenient alias for 'elem'.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x 'empty'              == False
-- hasEdge x ('edge' x)           == True
-- hasEdge x . 'removeEdge' x == const False
-- @
hasEdge :: Eq a => a -> Fold a -> Bool
hasEdge = H.hasEdge

-- | The number of distinct edges in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- edgeCount 'empty'    == 0
-- edgeCount ('edge' x) == 1
-- edgeCount          == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => Fold a -> Int
edgeCount = I.edgeCount . toIncidence

-- | The sorted list of distinct edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeList 'empty'    == []
-- edgeList ('edge' x) == [x]
-- @
edgeList :: Ord a => Fold a -> [a]
edgeList = I.edgeList . toIncidence

-- | The set of distinct edges of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet . 'edge'     == Set.'Set.singleton'
-- @
edgeSet :: Ord a => Fold a -> Set.Set a
edgeSet = I.edgeSet . toIncidence

-- | The set of edges of a given graph, specialised for graphs with
-- edges of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- edgeIntSet 'empty'    == IntSet.'IntSet.empty'
-- edgeIntSet . 'edge'   == IntSet.'IntSet.singleton'
-- @
edgeIntSet :: Fold Int -> IntSet.IntSet
edgeIntSet = I.edgeIntSet . toIncidence

-- | The number of nodes in a graph.
-- Complexity: /O(s * log(m))/ time.
--
-- @
-- nodeCount 'empty'    == 0
-- nodeCount ('edge' x) == 2
-- @
nodeCount :: Ord a => Fold a -> Int
nodeCount = I.nodeCount . toIncidence

-- | The sorted list of nodes of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- nodeList 'empty'    == []
-- @
nodeList :: Ord a => Fold a -> [I.Node a]
nodeList = I.nodeList . toIncidence

-- | The set of nodes of a given graph.
-- Complexity: /O(s * log(m))/ time and /O(m)/ memory.
--
-- @
-- nodeSet 'empty' == Set.'Set.empty'
-- @
nodeSet :: Ord a => Fold a -> Set.Set (I.Node a)
nodeSet = I.nodeSet . toIncidence

-- Helper to convert a Fold to a Incidence.
toIncidence :: Ord a => Fold a -> I.Incidence a
toIncidence = C.toEdgeGraph

-- | Construct a /mesh graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh xs     []   == 'empty'
-- mesh []     ys   == 'empty'
-- mesh [x]    [y]  == 'edge' (x, y)
-- @
mesh :: [a] -> [b] -> Fold (a, b)
mesh xs ys = C.overlays
    [ fmap (,b) (C.path (map fst ps)) | b <- ys, let ps = map (,b) xs ] `C.overlay`
    C.overlays
    [ fmap (a,) (C.path (map snd ps)) | a <- xs, let ps = map (a,) ys ]

-- | Construct a /torus graph/ from two lists of edges.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus xs     []   == 'empty'
-- torus []     ys   == 'empty'
-- @
torus :: [a] -> [b] -> Fold (a, b)
torus xs ys = C.overlays
    [ fmap (,b) (C.circuit (map fst ps)) | b <- ys, let ps = map (,b) xs ] `C.overlay`
    C.overlays
    [ fmap (a,) (C.circuit (map snd ps)) | a <- xs, let ps = map (a,) ys ]

-- | Construct a /De Bruijn graph/ of given dimension and symbols of a given
-- alphabet.
-- Complexity: /O(A * D^A)/ time, memory and size, where /A/ is the size of the
-- alphabet and /D/ is the dimension of the graph.
--
-- @
-- deBruijn k []    == 'empty'
-- @
deBruijn :: Int -> [a] -> Fold [a]
deBruijn len alphabet = bind skeleton expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = C.edges [ Left s | s <- overlaps ] `C.overlay`
               C.edges [ Right s | s <- overlaps ]
    expand v = case v of
        Left  s -> foldr C.overlay C.empty [ C.edge ([a] ++ s) | a <- alphabet ]
        Right s -> foldr C.overlay C.empty [ C.edge (s ++ [a]) | a <- alphabet ]

-- | Remove all occurrences of an edge from the graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeEdge x ('edge' x)           == 'empty'
-- removeEdge x . removeEdge x == removeEdge x
-- @
removeEdge :: (Eq (C.Edge g), C.EdgeGraph g) => C.Edge g -> Fold (C.Edge g) -> g
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
replaceEdge :: (Eq (C.Edge g), C.EdgeGraph g) => C.Edge g -> C.Edge g -> Fold (C.Edge g) -> g
replaceEdge u v = gmap $ \w -> if w == u then v else w

-- | Merge edges satisfying a given predicate with a given edge.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeEdges (const False) x == id
-- mergeEdges (== x) y        == 'replaceEdge' x y
-- @
mergeEdges :: C.EdgeGraph g => (C.Edge g -> Bool) -> C.Edge g -> Fold (C.Edge g) -> g
mergeEdges p v = gmap $ \u -> if p u then v else u

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
splitEdge :: (Eq (C.Edge g), C.EdgeGraph g) => C.Edge g -> [C.Edge g] -> Fold (C.Edge g) -> g
splitEdge v vs g = bind g $ \u -> if u == v then C.edges vs else C.edge u

-- | Transpose a given graph. Swaps 'into' arguments and exchanges 'pits'/'tips'.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('edge' x)    == 'edge' x
-- transpose . transpose == id
-- @
transpose :: C.EdgeGraph g => Fold (C.Edge g) -> g
transpose = foldg C.empty C.edge C.overlay (flip C.into) C.tips C.pits

-- | Transform a given graph by applying a function to each of its edges.
-- This is similar to 'fmap' but can be used with non-fully-parametric graphs.
--
-- @
-- gmap f 'empty'    == 'empty'
-- gmap f ('edge' x) == 'edge' (f x)
-- gmap id         == id
-- gmap f . gmap g == gmap (f . g)
-- @
gmap :: C.EdgeGraph g => (a -> C.Edge g) -> Fold a -> g
gmap f = foldg C.empty (C.edge . f) C.overlay C.into C.pits C.tips

-- | Transform a given graph by substituting each of its edges with a subgraph.
-- This is similar to Monad's bind '>>=' but can be used with non-fully-parametric
-- graphs.
--
-- @
-- bind 'empty' f          == 'empty'
-- bind ('edge' x) f       == f x
-- bind ('edges' xs) f     == 'overlays' ('map' f xs)
-- bind x (const 'empty')  == 'empty'
-- bind x 'edge'           == x
-- bind (bind x f) g     == bind x (\\y -> bind (f y) g)
-- @
bind :: C.EdgeGraph g => Fold a -> (a -> g) -> g
bind g f = foldg C.empty f C.overlay C.into C.pits C.tips g

-- | Construct the /induced subgraph/ of a given graph by removing the
-- edges that do not satisfy a given predicate.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- induce (const True)  x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeEdge' x
-- induce p . induce q     == induce (\\x -> p x && q x)
-- @
induce :: C.EdgeGraph g => (C.Edge g -> Bool) -> Fold (C.Edge g) -> g
induce p g = bind g $ \v -> if p v then C.edge v else C.empty

-- | Simplify a given graph. Semantically, this is the identity function, but
-- it simplifies a given polymorphic graph expression according to the laws of
-- the algebra. The function does not compute the simplest possible expression,
-- but uses heuristics to obtain useful simplifications in reasonable time.
-- Complexity: the function performs /O(s)/ graph comparisons. It is guaranteed
-- that the size of the result does not exceed the size of the given expression.
--
-- @
-- simplify x           == x
-- 'size' (simplify x)    <= 'size' x
-- simplify 'empty'       ~> 'empty'
-- simplify ('edge' 1)    ~> 'edge' 1
-- simplify ('edge' 1 + 'edge' 1) ~> 'edge' 1
-- @
simplify :: (Eq g, C.EdgeGraph g) => Fold (C.Edge g) -> g
simplify = foldg C.empty C.edge (simple C.overlay) (simple C.into) (simple C.pits) (simple C.tips)

simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y

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
box :: (C.EdgeGraph g, C.Edge g ~ (u, v)) => Fold u -> Fold v -> g
box x y = C.overlays $ xs ++ ys
  where
    xs = map (\b -> gmap (,b) x) $ toList y
    ys = map (\a -> gmap (a,) y) $ toList x
