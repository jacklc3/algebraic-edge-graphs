-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.AdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of edge-indexed adjacency maps.
-- An 'AdjacencyMap' stores, for each edge, four sets describing its
-- neighbourhood in the flow representation. This is equivalent to 'Incidence'
-- but indexed by edge for O(log n) lookups.
--
-- The API is unstable and unsafe. Where possible use the non-internal module
-- "EdgeGraph.AdjacencyMap" instead.
--
-----------------------------------------------------------------------------
module EdgeGraph.AdjacencyMap.Internal (
    -- * Data structure
    Adjacency (..), AdjacencyMap (..), consistent,

    -- * Conversion
    toIncidence, fromIncidence,

    -- * Basic graph construction primitives
    empty, edge, overlay, into, pits, tips, edges, fromNodeList, fromIncidenceList,

    -- * Graph properties
    nodeList, nodeSet, edgeSet, edgeList, adjacencyList,
    edgeCount, nodeCount, isEmpty, hasEdge,

    -- * Graph transformation
    removeEdge, detachPit, detachTip, gmap, induce
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified EdgeGraph.Class             as C
import qualified EdgeGraph.Incidence.Internal as I

-- | Neighbourhood information for a single edge in the graph.
--
-- For an edge @a@, this records its relationships to other edges through
-- the two nodes it connects (its source and sink in the flow representation):
--
-- * 'forks': all edges departing from the same source node as @a@ (including @a@)
-- * 'joins': all edges arriving at the same sink node as @a@ (including @a@)
-- * 'preds': edges arriving at @a@'s source node (predecessors)
-- * 'succs': edges departing from @a@'s sink node (successors)
data Adjacency a = Adjacency
    { forks :: Set a
    , joins :: Set a
    , preds :: Set a
    , succs :: Set a
    } deriving (Eq, Ord, Show)

-- | The 'AdjacencyMap' data type represents an edge-indexed graph as a map
-- from each edge to its 'Adjacency' information. This is an equivalent
-- representation to 'I.Incidence' (the canonical flow representation for
-- algebraic edge graphs), but indexed by edge for efficient lookups.
--
-- The 'Eq' instance is derived from the underlying 'Map' and is correct
-- because the representation is canonical (one-to-one with 'I.Incidence').
newtype AdjacencyMap a = AdjacencyMap
    { adjacencyMap :: Map a (Adjacency a)
    } deriving Eq

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    show = show . toIncidence

instance Ord a => C.EdgeGraph (AdjacencyMap a) where
    type Edge (AdjacencyMap a) = a
    empty   = empty
    edge    = edge
    overlay = overlay
    into    = into
    pits    = pits
    tips    = tips

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = edge . fromInteger
    (+)         = overlay
    (*)         = into
    signum      = const empty
    abs         = id
    negate      = id

-- | Convert an 'AdjacencyMap' to its equivalent 'I.Incidence' representation.
--
-- For each edge, reconstruct its source node (pitNode) and sink node (tipNode),
-- then collect all unique nodes into a set.
--
-- @
-- 'toIncidence' 'empty'    == 'I.empty'
-- 'toIncidence' ('edge' x) == 'I.edge' x
-- @
toIncidence :: Ord a => AdjacencyMap a -> I.Incidence a
toIncidence (AdjacencyMap m)
    | Map.null m = I.Incidence Set.empty
    | otherwise  = I.Incidence $ Set.fromList allNodes
  where
    allNodes = concatMap nodesPair (Map.elems m)
    nodesPair adj =
        [ I.Node (preds adj) (forks adj)   -- source node
        , I.Node (joins adj) (succs adj)   -- sink node
        ]

-- | Convert an 'I.Incidence' to an 'AdjacencyMap'.
--
-- Scans all nodes once to build edge-to-source and edge-to-sink maps,
-- then constructs the 'Adjacency' record for each edge.
--
-- @
-- 'fromIncidence' 'I.empty'    == 'empty'
-- 'fromIncidence' ('I.edge' x) == 'edge' x
-- @
fromIncidence :: Ord a => I.Incidence a -> AdjacencyMap a
fromIncidence (I.Incidence ns)
    | Set.null ns = AdjacencyMap Map.empty
    | otherwise   = AdjacencyMap $ Map.fromSet buildAdj allEdges
  where
    nl = Set.toList ns
    -- Build maps: edge -> the node where it's in pits (source),
    --             edge -> the node where it's in tips (sink)
    sourceMap = Map.fromList
        [ (a, n) | n <- nl, a <- Set.toList (I.nodePits n) ]
    sinkMap = Map.fromList
        [ (a, n) | n <- nl, a <- Set.toList (I.nodeTips n) ]
    allEdges = Map.keysSet sourceMap
    buildAdj a =
        let srcNode  = sourceMap Map.! a
            sinkNode = sinkMap   Map.! a
        in Adjacency
            { forks = I.nodePits srcNode
            , joins = I.nodeTips sinkNode
            , preds = I.nodeTips srcNode
            , succs = I.nodePits sinkNode
            }

-- | Check if an 'AdjacencyMap' is internally consistent.
--
-- Verifies the following invariants:
--
-- 1. Self-membership: @a ∈ forks(a)@ and @a ∈ joins(a)@
-- 2. Group equivalence: @b ∈ forks(a)@ implies @forks(b) = forks(a)@
-- 3. Group equivalence: @b ∈ joins(a)@ implies @joins(b) = joins(a)@
-- 4. Cross-consistency: @b ∈ preds(a)@ implies @succs(b) = forks(a)@ and @joins(b) = preds(a)@
-- 5. Cross-consistency: @b ∈ succs(a)@ implies @preds(b) = joins(a)@ and @forks(b) = succs(a)@
-- 6. All referenced edges exist in the map
--
-- @
-- consistent 'empty'         == True
-- consistent ('edge' x)      == True
-- consistent ('overlay' x y) == True
-- consistent ('into' x y)    == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (AdjacencyMap m) =
    selfMembership && forkEquiv && joinEquiv &&
    predCross && succCross && allExist
  where
    keys = Map.keysSet m
    entries = Map.toList m

    -- 1. Self-membership
    selfMembership = all (\(a, adj) ->
        Set.member a (forks adj) && Set.member a (joins adj)) entries

    -- 2. Forks equivalence
    forkEquiv = all (\(_, adj) ->
        all (\b -> case Map.lookup b m of
            Just adjB -> forks adjB == forks adj
            Nothing   -> False
        ) (Set.toList $ forks adj)) entries

    -- 3. Joins equivalence
    joinEquiv = all (\(_, adj) ->
        all (\b -> case Map.lookup b m of
            Just adjB -> joins adjB == joins adj
            Nothing   -> False
        ) (Set.toList $ joins adj)) entries

    -- 4. Preds cross-consistency
    predCross = all (\(_, adj) ->
        all (\b -> case Map.lookup b m of
            Just adjB -> succs adjB == forks adj
                      && joins adjB == preds adj
            Nothing   -> False
        ) (Set.toList $ preds adj)) entries

    -- 5. Succs cross-consistency
    succCross = all (\(_, adj) ->
        all (\b -> case Map.lookup b m of
            Just adjB -> preds adjB == joins adj
                      && forks adjB == succs adj
            Nothing   -> False
        ) (Set.toList $ succs adj)) entries

    -- 6. All referenced edges exist
    allExist = all (\(_, adj) ->
        Set.isSubsetOf (forks adj) keys &&
        Set.isSubsetOf (joins adj) keys &&
        Set.isSubsetOf (preds adj) keys &&
        Set.isSubsetOf (succs adj) keys) entries

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty' empty == True
-- @
empty :: AdjacencyMap a
empty = AdjacencyMap Map.empty

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'        ('edge' x) == False
-- 'hasEdge' x ('edge' x) == True
-- 'edgeCount' ('edge' x) == 1
-- 'nodeCount'      ('edge' x) == 2
-- @
edge :: Ord a => a -> AdjacencyMap a
edge a = AdjacencyMap $ Map.singleton a $ Adjacency
    { forks = Set.singleton a
    , joins = Set.singleton a
    , preds = Set.empty
    , succs = Set.empty
    }

-- | /Overlay/ two graphs. This computes the least upper bound of two flow
-- representations by merging nodes that share edges.
--
-- @
-- 'isEmpty'     ('overlay' x y) == 'isEmpty' x && 'isEmpty' y
-- 'overlay' 'empty' x         == x
-- 'overlay' x 'empty'         == x
-- 'overlay' x y             == 'overlay' y x
-- 'overlay' x ('overlay' y z) == 'overlay' ('overlay' x y) z
-- 'overlay' x x             == x
-- @
overlay :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay x y = fromIncidence $ I.overlay (toIncidence x) (toIncidence y)

-- | /Into/ two graphs. Connects the sink side of the left graph to the
-- source side of the right graph, creating a sequential composition.
--
-- @
-- 'isEmpty' ('into' x y) == 'isEmpty' x && 'isEmpty' y
-- 'into' 'empty' x     == x
-- 'into' x 'empty'     == x
-- @
into :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
into x y = fromIncidence $ I.into (toIncidence x) (toIncidence y)

-- | /Pits/ two graphs. Connects where outgoing edges (pits) overlap,
-- causing source-side merging.
--
-- @
-- 'isEmpty' ('pits' x y) == 'isEmpty' x && 'isEmpty' y
-- @
pits :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
pits x y = fromIncidence $ I.pits (toIncidence x) (toIncidence y)

-- | /Tips/ two graphs. Connects where incoming edges (tips) overlap,
-- causing sink-side merging.
--
-- @
-- 'isEmpty' ('tips' x y) == 'isEmpty' x && 'isEmpty' y
-- @
tips :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
tips x y = fromIncidence $ I.tips (toIncidence x) (toIncidence y)

-- | Construct a graph from a given list of edges by overlaying them.
--
-- @
-- edges []    == 'empty'
-- edges [x]   == 'edge' x
-- @
edges :: Ord a => [a] -> AdjacencyMap a
edges = fromIncidence . I.edges

-- | Construct a graph from a list of nodes. The nodes are normalized
-- (merged where they share labels).
fromNodeList :: Ord a => [I.Node a] -> AdjacencyMap a
fromNodeList = fromIncidence . I.fromNodeList

-- | Construct a graph from a list of (tips, pits) pairs, where each pair
-- represents a node with its incoming edges (tips) and outgoing edge
-- labels (pits). The resulting graph is normalized (nodes sharing labels
-- are merged).
--
-- @
-- fromIncidenceList []                == 'empty'
-- fromIncidenceList [([],[x]),([x],[])] == 'edge' x
-- @
fromIncidenceList :: Ord a => [([a], [a])] -> AdjacencyMap a
fromIncidenceList = fromIncidence . I.fromIncidenceList

-- | The sorted list of nodes of a graph.
nodeList :: Ord a => AdjacencyMap a -> [I.Node a]
nodeList = I.nodeList . toIncidence

-- | The set of nodes of a graph.
nodeSet :: Ord a => AdjacencyMap a -> Set (I.Node a)
nodeSet = I.nodeSet . toIncidence

-- | The number of nodes in a graph.
nodeCount :: Ord a => AdjacencyMap a -> Int
nodeCount = I.nodeCount . toIncidence

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
isEmpty :: AdjacencyMap a -> Bool
isEmpty (AdjacencyMap m) = Map.null m

-- | The set of all distinct edges.
edgeSet :: AdjacencyMap a -> Set a
edgeSet (AdjacencyMap m) = Map.keysSet m

-- | The sorted list of all distinct edges.
edgeList :: AdjacencyMap a -> [a]
edgeList (AdjacencyMap m) = Map.keys m

-- | The sorted /adjacency list/ of a graph. Each entry is an edge
-- paired with its 'Adjacency' record.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- adjacencyList 'empty'    == []
-- adjacencyList ('edge' x) == [(x, Adjacency (Set.'Data.Set.singleton' x) (Set.'Data.Set.singleton' x) Set.'Data.Set.empty' Set.'Data.Set.empty')]
-- @
adjacencyList :: AdjacencyMap a -> [(a, Adjacency a)]
adjacencyList (AdjacencyMap m) = Map.toAscList m

-- | The number of distinct edges.
edgeCount :: AdjacencyMap a -> Int
edgeCount (AdjacencyMap m) = Map.size m

-- | Check if a graph contains a given edge.
-- Complexity: /O(log n)/ time.
hasEdge :: Ord a => a -> AdjacencyMap a -> Bool
hasEdge a (AdjacencyMap m) = Map.member a m

-- | Remove an edge from the graph, updating all neighbour references.
-- Complexity: /O((|forks| + |joins| + |preds| + |succs|) * log n)/ time.
--
-- @
-- removeEdge x ('edge' x) == 'empty'
-- @
removeEdge :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
removeEdge x (AdjacencyMap m) = case Map.lookup x m of
    Nothing  -> AdjacencyMap m
    Just adj ->
        let xForks   = forks adj
            xJoins   = joins adj
            xPreds   = preds adj
            xSuccs   = succs adj
            newForks = Set.delete x xForks
            newJoins = Set.delete x xJoins
            -- Delete x from the map
            m1 = Map.delete x m
            -- Update forks of former fork-siblings
            m2 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { forks = newForks }) b acc)
                    m1 newForks
            -- Update joins of former join-siblings
            m3 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { joins = newJoins }) b acc)
                    m2 newJoins
            -- Update succs of predecessors
            m4 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { succs = newForks }) b acc)
                    m3 xPreds
            -- Update preds of successors
            m5 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { preds = newJoins }) b acc)
                    m4 xSuccs
        in AdjacencyMap m5

-- | Detach an edge from its source node. The edge gets a fresh source
-- while any other edges sharing the original source node remain together.
-- Complexity: /O((|forks| + |preds|) * log n)/ time.
--
-- @
-- detachPit x ('edge' x)                    == 'edge' x
-- detachPit 2 ('into' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- detachPit 1 ('pits' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- @
detachPit :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
detachPit a (AdjacencyMap m) = case Map.lookup a m of
    Nothing  -> AdjacencyMap m
    Just adj ->
        let oldForks = forks adj
            oldPreds = preds adj
            -- Update a: fresh source node means forks={a}, preds=∅
            m1 = Map.adjust (\r -> r { forks = Set.singleton a
                                     , preds = Set.empty }) a m
            -- Remove a from forks of former siblings
            m2 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { forks = Set.delete a (forks r) }) b acc)
                    m1 (Set.delete a oldForks)
            -- Remove a from succs of former predecessors
            m3 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { succs = Set.delete a (succs r) }) b acc)
                    m2 oldPreds
        in AdjacencyMap m3

-- | Detach an edge from its sink node. The edge gets a fresh sink
-- while any other edges sharing the original sink node remain together.
-- Complexity: /O((|joins| + |succs|) * log n)/ time.
--
-- @
-- detachTip x ('edge' x)                    == 'edge' x
-- detachTip 1 ('into' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- detachTip 1 ('tips' ('edge' 1) ('edge' 2))  == 'edges' [1, 2]
-- @
detachTip :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
detachTip a (AdjacencyMap m) = case Map.lookup a m of
    Nothing  -> AdjacencyMap m
    Just adj ->
        let oldJoins = joins adj
            oldSuccs = succs adj
            -- Update a: fresh sink node means joins={a}, succs=∅
            m1 = Map.adjust (\r -> r { joins = Set.singleton a
                                     , succs = Set.empty }) a m
            -- Remove a from joins of former siblings
            m2 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { joins = Set.delete a (joins r) }) b acc)
                    m1 (Set.delete a oldJoins)
            -- Remove a from preds of former successors
            m3 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { preds = Set.delete a (preds r) }) b acc)
                    m2 oldSuccs
        in AdjacencyMap m3

-- | Transform a graph by applying a function to each edge.
--
-- @
-- gmap f 'empty'    == 'empty'
-- gmap f ('edge' x) == 'edge' (f x)
-- gmap id         == id
-- gmap f . gmap g == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap f = fromIncidence . I.gmap f . toIncidence

-- | Construct the /induced subgraph/ of a given graph by removing edges
-- that do not satisfy a given predicate. Keeps only edges where @p@ returns
-- 'True', and updates all neighbour sets accordingly.
-- Complexity: /O(n * m)/ time.
--
-- @
-- induce (const True)  x == x
-- induce (const False) x == 'empty'
-- @
induce :: Ord a => (a -> Bool) -> AdjacencyMap a -> AdjacencyMap a
induce p (AdjacencyMap m) = AdjacencyMap $ Map.map updateAdj kept
  where
    kept     = Map.filterWithKey (\k _ -> p k) m
    keptKeys = Map.keysSet kept
    updateAdj adj = Adjacency
        { forks = Set.intersection (forks adj) keptKeys
        , joins = Set.intersection (joins adj) keptKeys
        , preds = Set.intersection (preds adj) keptKeys
        , succs = Set.intersection (succs adj) keptKeys
        }
