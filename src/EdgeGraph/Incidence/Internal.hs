-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.Incidence.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of flow representations (the canonical
-- representation for algebraic edge graphs) as described in the paper.
-- A flow representation is a set of nodes where each edge appears in
-- exactly one node's tips and exactly one node's pits, nodes can have empty
-- tips (source nodes) or empty pits (sink nodes) but not both empty, and
-- distinct nodes have disjoint tips and disjoint pits.
--
-- The API is unstable and unsafe. Where possible use the non-internal module
-- "EdgeGraph.Incidence" instead.
--
-----------------------------------------------------------------------------
module EdgeGraph.Incidence.Internal (
    -- * Data structure
    Node (..), Incidence (..), consistent,

    -- * Normalization
    normalize,

    -- * Basic graph construction primitives
    empty, edge, overlay, into, pits, tips, edges, fromNodeList, fromIncidenceList,

    -- * Graph properties
    nodeList, nodeSet, edgeSet, edgeList,
    edgeCount, nodeCount, isEmpty, hasEdge,

    -- * Graph transformation
    removeEdge, detachPit, detachTip, gmap, induce
  ) where

import Data.Set (Set, union)

import qualified Data.IntMap.Strict  as IntMap
import qualified Data.Map.Strict     as Map
import qualified EdgeGraph.Class as C
import qualified Data.Set            as Set

-- | A 'Node' represents an implicit vertex in an edge-indexed graph.
-- It has a set of incoming edges ('nodeTips') and a set of outgoing
-- edges ('nodePits'). A node may have empty tips (making it a source
-- node) or empty pits (making it a sink node), but not both empty.
data Node a = Node {
    -- | The set of incoming edges (tips) for this node.
    nodeTips :: Set a,
    -- | The set of outgoing edges (pits) for this node.
    nodePits :: Set a
  } deriving (Eq, Ord)

instance (Ord a, Show a) => Show (Node a) where
    show (Node ts ps) = "Node " ++ show (Set.toAscList ts) ++ " " ++ show (Set.toAscList ps)

-- | The 'Incidence' data type represents an edge-indexed graph as a flow
-- representation: a set of nodes where each edge appears in exactly one
-- node's tips and exactly one node's pits. This is the canonical representation
-- for algebraic edge graphs.
--
-- The 'Eq' instance satisfies all axioms of algebraic edge graphs.
newtype Incidence a = Incidence {
    -- | The set of 'Node's in the flow representation.
    nodes :: Set (Node a)
  } deriving Eq

instance (Ord a, Show a) => Show (Incidence a) where
    show (Incidence ns)
        | Set.null ns = "empty"
        | isSimpleEdge = "edge " ++ show singleLabel
        | allIsolated  = "edges " ++ show isolatedLabels
        | otherwise    = "fromNodeList " ++ show (Set.toAscList ns)
      where
        nl = Set.toAscList ns
        -- Check for a single edge: exactly two nodes forming a source-sink pair
        isSimpleEdge = Set.size ns == 2 &&
            case nl of
                [Node ts1 ps1, Node ts2 ps2] ->
                    (Set.null ts1 && Set.size ps1 == 1 &&
                     Set.size ts2 == 1 && Set.null ps2 &&
                     ps1 == ts2)
                    ||
                    (Set.size ts1 == 1 && Set.null ps1 &&
                     Set.null ts2 && Set.size ps2 == 1 &&
                     ts1 == ps2)
                _ -> False
        singleLabel = case nl of
            [Node ts ps, _] | Set.null ts -> Set.findMin ps
            [_, Node ts ps] | Set.null ts -> Set.findMin ps
            _ -> error "singleLabel: not a simple edge"
        -- Check all nodes are isolated source-sink pairs
        allIsolated = even (length nl) &&
            all (\(Node ts ps) -> Set.null ts || Set.null ps) nl &&
            all (\(Node ts ps) -> Set.size ts <= 1 && Set.size ps <= 1) nl &&
            length nl > 0
        isolatedLabels = [Set.findMin ps | Node ts ps <- nl, Set.null ts, Set.size ps == 1]

instance Ord a => C.EdgeGraph (Incidence a) where
    type Edge (Incidence a) = a
    empty   = empty
    edge    = edge
    overlay = overlay
    into    = into
    pits    = pits
    tips    = tips

instance (Ord a, Num a) => Num (Incidence a) where
    fromInteger = edge . fromInteger
    (+)         = overlay
    (*)         = into
    signum      = const empty
    abs         = id
    negate      = id

-- | Check if a flow representation is consistent:
--
-- 1. No (∅,∅) nodes
-- 2. Distinct nodes have disjoint tips
-- 3. Distinct nodes have disjoint pits
-- 4. The union of all tips equals the union of all pits (same edge set)
--
-- @
-- consistent 'empty'         == True
-- consistent ('edge' x)      == True
-- consistent ('overlay' x y) == True
-- consistent ('into' x y)    == True
-- @
consistent :: Ord a => Incidence a -> Bool
consistent (Incidence ns) = noEmptyNodes && disjointTips && disjointPits && sameDomain
  where
    nl = Set.toList ns
    noEmptyNodes = all (\(Node ts ps) -> not (Set.null ts && Set.null ps)) nl
    tipSizes = sum $ map (Set.size . nodeTips) nl
    pitSizes = sum $ map (Set.size . nodePits) nl
    allTipLabels = Set.unions $ map nodeTips nl
    allPitLabels = Set.unions $ map nodePits nl
    disjointTips = tipSizes == Set.size allTipLabels
    disjointPits = pitSizes == Set.size allPitLabels
    sameDomain = allTipLabels == allPitLabels

-- | Normalize a list of nodes into a valid flow representation by merging
-- nodes that transitively share any edge in their tips or pits.
-- Nodes that become (∅,∅) are removed.
--
-- This is the core algorithm that computes the least upper bound (overlay)
-- of a collection of nodes. It uses a union-find data structure indexed by
-- edges to efficiently determine which nodes must be merged.
normalize :: Ord a => [Node a] -> Set (Node a)
normalize [] = Set.empty
normalize rawNodes =
    -- Phase 1: Index nodes and compute unions via label maps
    let indexed = zip [0..] rawNodes
        (_, _, uf) = foldl' processNode (Map.empty, Map.empty, IntMap.empty) indexed
        -- Phase 2: Group nodes by their union-find representative and merge
        groups = IntMap.fromListWith mergeNodes
            [(ufFind uf i, n) | (i, n) <- indexed]
    -- Phase 3: Filter out empty nodes
    in Set.fromList [n | n <- IntMap.elems groups, nonEmptyNode n]
  where
    nonEmptyNode (Node ts ps) = not (Set.null ts && Set.null ps)

    mergeNodes (Node t1 p1) (Node t2 p2) =
        Node (t1 `Set.union` t2) (p1 `Set.union` p2)

    -- For each node, register its labels in the tip/pit maps.
    -- When a label is already mapped to another node, union them.
    processNode (tipMap, pitMap, uf) (i, Node ts ps) =
        let (tipMap', uf1) = foldl' (processLabel i) (tipMap, uf)  (Set.toList ts)
            (pitMap', uf2) = foldl' (processLabel i) (pitMap, uf1) (Set.toList ps)
        in (tipMap', pitMap', uf2)

    processLabel nodeIdx (labelMap, uf) label =
        case Map.lookup label labelMap of
            Nothing          -> (Map.insert label nodeIdx labelMap, uf)
            Just existingIdx -> (labelMap, ufUnion uf nodeIdx existingIdx)

-- | Find the root representative of an element in the union-find.
-- Uses path compression by following parent pointers to the root.
ufFind :: IntMap.IntMap Int -> Int -> Int
ufFind uf x = case IntMap.lookup x uf of
    Nothing -> x
    Just p | p == x    -> x
           | otherwise -> ufFind uf p

-- | Union two elements in the union-find structure.
-- Makes the root of x point to the root of y.
ufUnion :: IntMap.IntMap Int -> Int -> Int -> IntMap.IntMap Int
ufUnion uf x y =
    let rx = ufFind uf x
        ry = ufFind uf y
    in if rx == ry then uf
       else IntMap.insert rx ry uf

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty' empty == True
-- @
empty :: Incidence a
empty = Incidence Set.empty

-- | Construct the graph comprising /a single edge/.
-- An edge has two nodes: a source (with the label as a pit) and a sink
-- (with the label as a tip).
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'        ('edge' x) == False
-- 'hasEdge' x ('edge' x) == True
-- 'edgeCount' ('edge' x) == 1
-- 'nodeCount'      ('edge' x) == 2
-- @
edge :: Ord a => a -> Incidence a
edge a = Incidence $ Set.fromList
    [ Node Set.empty        (Set.singleton a)  -- source: edge a leaves
    , Node (Set.singleton a) Set.empty         -- sink:   edge a arrives
    ]

-- | /Overlay/ two graphs. This computes the least upper bound of two flow
-- representations by merging nodes that share edges.
-- Complexity: /O(n^2 * m)/ time.
--
-- @
-- 'isEmpty'     ('overlay' x y) == 'isEmpty' x && 'isEmpty' y
-- 'overlay' 'empty' x         == x
-- 'overlay' x 'empty'         == x
-- 'overlay' x y             == 'overlay' y x
-- 'overlay' x ('overlay' y z) == 'overlay' ('overlay' x y) z
-- 'overlay' x x             == x
-- @
overlay :: Ord a => Incidence a -> Incidence a -> Incidence a
overlay (Incidence xs) (Incidence ys) =
    Incidence $ normalize (Set.toList xs ++ Set.toList ys)

-- | Helper function c_i from Definition 10 of the paper.
-- Creates the intermediate nodes for the 'into' operation.
ci :: Ord a => a -> a -> [Node a]
ci d e
    | d /= e    = [ Node Set.empty        (Set.singleton d)
                   , Node (Set.singleton d) (Set.singleton e)
                   , Node (Set.singleton e) Set.empty
                   ]
    | otherwise  = [ Node (Set.singleton d) (Set.singleton d) ]

-- | /Into/ two graphs. Connects the sink side of the left graph to the
-- source side of the right graph, creating a sequential composition.
-- Complexity: /O((n + |E_l| * |E_r|)^2 * m)/ time.
--
-- @
-- 'isEmpty' ('into' x y) == 'isEmpty' x && 'isEmpty' y
-- 'into' 'empty' x     == x
-- 'into' x 'empty'     == x
-- 'into' ('edge' x) ('edge' y) /= 'overlay' ('edge' x) ('edge' y)
-- @
into :: Ord a => Incidence a -> Incidence a -> Incidence a
into x y = Incidence $ normalize allNodes
  where
    ds = edgeList x
    es = edgeList y
    ciNodes = concatMap (\d -> concatMap (ci d) es) ds
    allNodes = Set.toList (nodes x) ++ Set.toList (nodes y) ++ ciNodes

-- | Helper function c_p from Definition 10 of the paper.
-- Creates the intermediate nodes for the 'pits' operation.
cp :: Ord a => a -> a -> [Node a]
cp d e = [ Node Set.empty          (Set.fromList [d, e])
         , Node (Set.singleton d) Set.empty
         , Node (Set.singleton e) Set.empty
         ]

-- | /Pits/ two graphs. Connects where outgoing edges (pits) overlap,
-- causing source-side merging.
-- Complexity: /O((n + |E_l| * |E_r|)^2 * m)/ time.
--
-- @
-- 'isEmpty' ('pits' x y) == 'isEmpty' x && 'isEmpty' y
-- @
pits :: Ord a => Incidence a -> Incidence a -> Incidence a
pits x y = Incidence $ normalize allNodes
  where
    ds = edgeList x
    es = edgeList y
    cpNodes = concatMap (\d -> concatMap (cp d) es) ds
    allNodes = Set.toList (nodes x) ++ Set.toList (nodes y) ++ cpNodes

-- | Helper function c_t from Definition 10 of the paper.
-- Creates the intermediate nodes for the 'tips' operation.
ct :: Ord a => a -> a -> [Node a]
ct d e = [ Node Set.empty          (Set.singleton d)
         , Node Set.empty          (Set.singleton e)
         , Node (Set.fromList [d, e]) Set.empty
         ]

-- | /Tips/ two graphs. Connects where incoming edges (tips) overlap,
-- causing sink-side merging.
-- Complexity: /O((n + |E_l| * |E_r|)^2 * m)/ time.
--
-- @
-- 'isEmpty' ('tips' x y) == 'isEmpty' x && 'isEmpty' y
-- @
tips :: Ord a => Incidence a -> Incidence a -> Incidence a
tips x y = Incidence $ normalize allNodes
  where
    ds = edgeList x
    es = edgeList y
    ctNodes = concatMap (\d -> concatMap (ct d) es) ds
    allNodes = Set.toList (nodes x) ++ Set.toList (nodes y) ++ ctNodes

-- | Construct a graph from a given list of edges by overlaying them.
-- Complexity: /O(L^2 * log(L))/ time and /O(L)/ memory.
--
-- @
-- edges []    == 'empty'
-- edges [x]   == 'edge' x
-- @
edges :: Ord a => [a] -> Incidence a
edges = foldr overlay empty . map edge

-- | Construct a graph from a list of nodes. The nodes are normalized
-- (merged where they share labels).
-- Complexity: /O(L^2 * m)/ time and /O(L)/ memory.
fromNodeList :: Ord a => [Node a] -> Incidence a
fromNodeList = Incidence . normalize

-- | Construct a graph from a list of (tips, pits) pairs, where each pair
-- represents a node with its incoming edges (tips) and outgoing edge
-- labels (pits). The resulting graph is normalized (nodes sharing labels
-- are merged).
-- Complexity: /O(L^2 * m)/ time and /O(L)/ memory.
--
-- @
-- fromIncidenceList []                == 'empty'
-- fromIncidenceList [([],[x]),([x],[])] == 'edge' x
-- @
fromIncidenceList :: Ord a => [([a], [a])] -> Incidence a
fromIncidenceList = fromNodeList . map (\(ts, ps) -> Node (Set.fromList ts) (Set.fromList ps))

-- | The sorted list of nodes of a graph.
-- Complexity: /O(n)/ time and /O(n)/ memory.
nodeList :: Incidence a -> [Node a]
nodeList = Set.toAscList . nodes

-- | The set of nodes of a graph.
nodeSet :: Incidence a -> Set (Node a)
nodeSet = nodes

-- | The number of nodes in a graph.
-- Complexity: /O(1)/ time.
nodeCount :: Incidence a -> Int
nodeCount = Set.size . nodes

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
isEmpty :: Incidence a -> Bool
isEmpty = Set.null . nodes

-- | The set of all distinct edges appearing in any node of the graph.
-- Complexity: /O(n * m)/ time where n is the number of nodes and m is the
-- average number of labels per node.
edgeSet :: Ord a => Incidence a -> Set a
edgeSet (Incidence ns) = Set.unions
    [ nodeTips n `union` nodePits n | n <- Set.toList ns ]

-- | The sorted list of all distinct edges.
edgeList :: Ord a => Incidence a -> [a]
edgeList = Set.toAscList . edgeSet

-- | The number of distinct edges.
edgeCount :: Ord a => Incidence a -> Int
edgeCount = Set.size . edgeSet

-- | Check if a graph contains a given edge.
hasEdge :: Ord a => a -> Incidence a -> Bool
hasEdge a = Set.member a . edgeSet

-- | Remove all occurrences of an edge from the graph. Nodes that become
-- (∅,∅) after removal are removed entirely.
-- Complexity: /O(n * log(n))/ time.
--
-- @
-- removeEdge x ('edge' x) == 'empty'
-- @
removeEdge :: Ord a => a -> Incidence a -> Incidence a
removeEdge x (Incidence ns) = Incidence $ Set.fromList
    [ n'
    | n <- Set.toList ns
    , let n' = Node (Set.delete x (nodeTips n)) (Set.delete x (nodePits n))
    , not (Set.null (nodeTips n') && Set.null (nodePits n'))
    ]

-- | Detach an edge from its source node. The edge gets a fresh source
-- node @Node ∅ {a}@ while any other edges sharing the original source node
-- remain together. If the edge is already at its own source or is not in the
-- graph, this is a no-op.
-- Complexity: /O(n * log(n))/ time.
--
-- @
-- detachPit x ('edge' x)                    == 'edge' x
-- detachPit 2 ('into' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- detachPit 1 ('pits' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- @
detachPit :: Ord a => a -> Incidence a -> Incidence a
detachPit a r@(Incidence ns)
    | not (hasEdge a r) = r
    | otherwise = Incidence $ Set.insert freshSource stripped
  where
    freshSource = Node Set.empty (Set.singleton a)
    stripped = Set.fromList
        [ n'
        | n <- Set.toList ns
        , let n' = if Set.member a (nodePits n)
                   then Node (nodeTips n) (Set.delete a (nodePits n))
                   else n
        , not (Set.null (nodeTips n') && Set.null (nodePits n'))
        ]

-- | Detach an edge from its sink node. The edge gets a fresh sink
-- node @Node {a} ∅@ while any other edges sharing the original sink node
-- remain together. If the edge is already at its own sink or is not in the
-- graph, this is a no-op.
-- Complexity: /O(n * log(n))/ time.
--
-- @
-- detachTip x ('edge' x)                    == 'edge' x
-- detachTip 1 ('into' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- detachTip 1 ('tips' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- @
detachTip :: Ord a => a -> Incidence a -> Incidence a
detachTip a r@(Incidence ns)
    | not (hasEdge a r) = r
    | otherwise = Incidence $ Set.insert freshSink stripped
  where
    freshSink = Node (Set.singleton a) Set.empty
    stripped = Set.fromList
        [ n'
        | n <- Set.toList ns
        , let n' = if Set.member a (nodeTips n)
                   then Node (Set.delete a (nodeTips n)) (nodePits n)
                   else n
        , not (Set.null (nodeTips n') && Set.null (nodePits n'))
        ]

-- | Transform a graph by applying a function to each edge. The result
-- is normalized since the function may map different labels to the same value,
-- violating disjointness.
-- Complexity: /O(n^2 * m * log(m))/ time.
--
-- @
-- gmap f 'empty'    == 'empty'
-- gmap f ('edge' x) == 'edge' (f x)
-- gmap id         == id
-- gmap f . gmap g == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> Incidence a -> Incidence b
gmap f (Incidence ns) = Incidence $ normalize $ map mapNode $ Set.toList ns
  where
    mapNode (Node ts ps) = Node (Set.map f ts) (Set.map f ps)

-- | Construct the /induced subgraph/ of a given graph by removing edges
-- that do not satisfy a given predicate. Nodes that become (∅,∅) are removed.
-- Complexity: /O(n * m)/ time.
--
-- @
-- induce (const True)  x == x
-- induce (const False) x == 'empty'
-- @
induce :: Ord a => (a -> Bool) -> Incidence a -> Incidence a
induce p (Incidence ns) = Incidence $ Set.fromList
    [ n'
    | n <- Set.toList ns
    , let n' = Node (Set.filter p (nodeTips n)) (Set.filter p (nodePits n))
    , not (Set.null (nodeTips n') && Set.null (nodePits n'))
    ]

