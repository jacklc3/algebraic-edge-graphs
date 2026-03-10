-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.IntAdjacencyMap.Internal
-- Copyright  : (c) Jack Liell-Cock 2025-2026
-- License    : MIT (see the file LICENSE)
-- Maintainer : jackliellcock@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of Int-specialised edge-indexed
-- adjacency maps. The API is unstable and unsafe. Where possible use
-- non-internal module "EdgeGraph.IntAdjacencyMap" instead.
--
-----------------------------------------------------------------------------
module EdgeGraph.IntAdjacencyMap.Internal (
  -- * Data structure
  Adjacency (..), IntAdjacencyMap (..), consistent,

  -- * Conversion
  toIncidence, fromIncidence,

  -- * Basic graph construction primitives
  empty, edge, overlay, into, pits, tips, edges, fromNodeList, fromIncidenceList,

  -- * Graph properties
  nodeList, nodeSet, edgeList, adjacencyList, edgeIntSet,
  edgeCount, nodeCount, isEmpty, hasEdge,

  -- * Graph transformation
  removeEdge, detachPit, detachTip, gmap, induce
) where

import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.IntSet                  as IntSet
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified EdgeGraph.Class              as C
import qualified EdgeGraph.Incidence.Internal as I

-- | Neighbourhood information for a single Int edge.
-- See "EdgeGraph.AdjacencyMap.Internal" for the general version.
data Adjacency = Adjacency
  { forks :: Set Int
  , joins :: Set Int
  , preds :: Set Int
  , succs :: Set Int
  } deriving (Eq, Ord, Show)

-- | Int-specialized edge-indexed adjacency map.
newtype IntAdjacencyMap = IntAdjacencyMap
  { adjacencyIntMap :: Map Int Adjacency
  } deriving Eq

instance Show IntAdjacencyMap where
  show = show . toIncidence

instance C.EdgeGraph IntAdjacencyMap where
  type Edge IntAdjacencyMap = Int
  empty   = empty
  edge    = edge
  overlay = overlay
  into    = into
  pits    = pits
  tips    = tips

-- | Convert an t'IntAdjacencyMap' to its equivalent 'I.Incidence' representation.
--
-- For each edge, reconstruct its source node (pitNode) and sink node (tipNode),
-- then collect all unique nodes into a set.
--
-- @
-- 'toIncidence' 'EdgeGraph.IntAdjacencyMap.Internal.empty'    == 'I.empty'
-- 'toIncidence' ('EdgeGraph.IntAdjacencyMap.Internal.edge' x) == 'I.edge' x
-- @
toIncidence :: IntAdjacencyMap -> I.Incidence Int
toIncidence (IntAdjacencyMap m)
    | Map.null m = I.Incidence Set.empty
    | otherwise  = I.Incidence $ Set.fromList allNodes
  where
    allNodes = concatMap nodesPair (Map.elems m)
    nodesPair adj =
      [ I.Node (preds adj) (forks adj)
      , I.Node (joins adj) (succs adj)
      ]

-- | Convert an 'I.Incidence' to an t'IntAdjacencyMap'.
--
-- Scans all nodes once to build edge-to-source and edge-to-sink maps,
-- then constructs the t'Adjacency' record for each edge.
--
-- @
-- 'fromIncidence' 'I.empty'    == 'EdgeGraph.IntAdjacencyMap.Internal.empty'
-- 'fromIncidence' ('I.edge' x) == 'EdgeGraph.IntAdjacencyMap.Internal.edge' x
-- @
fromIncidence :: I.Incidence Int -> IntAdjacencyMap
fromIncidence (I.Incidence ns)
    | Set.null ns = IntAdjacencyMap Map.empty
    | otherwise   = IntAdjacencyMap $ Map.fromSet buildAdj allEdges
  where
    nl = Set.toList ns
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

-- | Check if an t'IntAdjacencyMap' is internally consistent.
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
-- consistent 'EdgeGraph.IntAdjacencyMap.Internal.empty'         == True
-- consistent ('EdgeGraph.IntAdjacencyMap.Internal.edge' x)      == True
-- consistent ('overlay' x y) == True
-- consistent ('into' x y)    == True
-- @
consistent :: IntAdjacencyMap -> Bool
consistent (IntAdjacencyMap m) =
    selfMembership && forkEquiv && joinEquiv &&
    predCross && succCross && allExist
  where
    keys = Map.keysSet m
    entries = Map.toList m

    selfMembership = all (\(a, adj) ->
      Set.member a (forks adj) && Set.member a (joins adj)) entries

    forkEquiv = all (\(_, adj) ->
      all (\b -> case Map.lookup b m of
        Just adjB -> forks adjB == forks adj
        Nothing   -> False
      ) (Set.toList $ forks adj)) entries

    joinEquiv = all (\(_, adj) ->
      all (\b -> case Map.lookup b m of
        Just adjB -> joins adjB == joins adj
        Nothing   -> False
      ) (Set.toList $ joins adj)) entries

    predCross = all (\(_, adj) ->
      all (\b -> case Map.lookup b m of
        Just adjB -> succs adjB == forks adj
                  && joins adjB == preds adj
        Nothing   -> False
      ) (Set.toList $ preds adj)) entries

    succCross = all (\(_, adj) ->
      all (\b -> case Map.lookup b m of
        Just adjB -> preds adjB == joins adj
                  && forks adjB == succs adj
        Nothing   -> False
      ) (Set.toList $ succs adj)) entries

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
empty :: IntAdjacencyMap
empty = IntAdjacencyMap Map.empty

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty' ('EdgeGraph.IntAdjacencyMap.Internal.edge' x)   == False
-- 'hasEdge' x ('EdgeGraph.IntAdjacencyMap.Internal.edge' x) == True
-- 'edgeCount' ('EdgeGraph.IntAdjacencyMap.Internal.edge' x) == 1
-- 'nodeCount' ('EdgeGraph.IntAdjacencyMap.Internal.edge' x) == 2
-- @
edge :: Int -> IntAdjacencyMap
edge a = IntAdjacencyMap $ Map.singleton a $ Adjacency
  { forks = Set.singleton a
  , joins = Set.singleton a
  , preds = Set.empty
  , succs = Set.empty
  }

-- | /Overlay/ two graphs. This computes the least upper bound of two flow
-- representations by merging nodes that share edges.
--
-- @
-- 'isEmpty' ('overlay' x y)   == 'isEmpty' x && 'isEmpty' y
-- 'overlay' 'EdgeGraph.IntAdjacencyMap.Internal.empty' x         == x
-- 'overlay' x 'EdgeGraph.IntAdjacencyMap.Internal.empty'         == x
-- 'overlay' x y               == 'overlay' y x
-- 'overlay' x ('overlay' y z) == 'overlay' ('overlay' x y) z
-- 'overlay' x x               == x
-- @
overlay :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
overlay x y = fromIncidence $ I.overlay (toIncidence x) (toIncidence y)

-- | /Into/ two graphs. Connects the sink side of the left graph to the
-- source side of the right graph, creating a sequential composition.
--
-- @
-- 'isEmpty' ('into' x y) == 'isEmpty' x && 'isEmpty' y
-- 'into' 'EdgeGraph.IntAdjacencyMap.Internal.empty' x       == x
-- 'into' x 'EdgeGraph.IntAdjacencyMap.Internal.empty'       == x
-- @
into :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
into x y = fromIncidence $ I.into (toIncidence x) (toIncidence y)

-- | /Pits/ two graphs. Connects where outgoing edges (pits) overlap,
-- causing source-side merging.
--
-- @
-- 'isEmpty' ('pits' x y) == 'isEmpty' x && 'isEmpty' y
-- @
pits :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
pits x y = fromIncidence $ I.pits (toIncidence x) (toIncidence y)

-- | /Tips/ two graphs. Connects where incoming edges (tips) overlap,
-- causing sink-side merging.
--
-- @
-- 'isEmpty' ('tips' x y) == 'isEmpty' x && 'isEmpty' y
-- @
tips :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
tips x y = fromIncidence $ I.tips (toIncidence x) (toIncidence y)

-- | Construct a graph from a given list of edges by overlaying them.
--
-- @
-- edges []  == 'EdgeGraph.IntAdjacencyMap.Internal.empty'
-- edges [x] == 'EdgeGraph.IntAdjacencyMap.Internal.edge' x
-- @
edges :: [Int] -> IntAdjacencyMap
edges = fromIncidence . I.edges

-- | Construct a graph from a list of nodes. The nodes are normalized
-- (merged where they share labels).
fromNodeList :: [I.Node Int] -> IntAdjacencyMap
fromNodeList = fromIncidence . I.fromNodeList

-- | Construct a graph from a list of (tips, pits) pairs, where each pair
-- represents a node with its incoming edges (tips) and outgoing edge
-- labels (pits). The resulting graph is normalized (nodes sharing labels
-- are merged).
--
-- @
-- fromIncidenceList []                  == 'EdgeGraph.IntAdjacencyMap.Internal.empty'
-- fromIncidenceList [([],[x]),([x],[])] == 'EdgeGraph.IntAdjacencyMap.Internal.edge' x
-- @
fromIncidenceList :: [([Int], [Int])] -> IntAdjacencyMap
fromIncidenceList = fromIncidence . I.fromIncidenceList

-- | The sorted list of nodes of a graph.
nodeList :: IntAdjacencyMap -> [I.Node Int]
nodeList = I.nodeList . toIncidence

-- | The set of nodes of a graph.
nodeSet :: IntAdjacencyMap -> Set (I.Node Int)
nodeSet = I.nodeSet . toIncidence

-- | The number of nodes in a graph.
nodeCount :: IntAdjacencyMap -> Int
nodeCount = I.nodeCount . toIncidence

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
isEmpty :: IntAdjacencyMap -> Bool
isEmpty (IntAdjacencyMap m) = Map.null m

-- | The sorted list of all distinct edges.
edgeList :: IntAdjacencyMap -> [Int]
edgeList (IntAdjacencyMap m) = Map.keys m

-- | The 'IntSet' of all distinct edges.
edgeIntSet :: IntAdjacencyMap -> IntSet
edgeIntSet (IntAdjacencyMap m) = IntSet.fromDistinctAscList (Map.keys m)

-- | The sorted /adjacency list/ of a graph. Each entry is an edge
-- paired with its t'Adjacency' record.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- adjacencyList 'EdgeGraph.IntAdjacencyMap.Internal.empty' == []
-- @
adjacencyList :: IntAdjacencyMap -> [(Int, Adjacency)]
adjacencyList (IntAdjacencyMap m) = Map.toAscList m

-- | The number of distinct edges.
edgeCount :: IntAdjacencyMap -> Int
edgeCount (IntAdjacencyMap m) = Map.size m

-- | Check if a graph contains a given edge.
-- Complexity: /O(log n)/ time.
hasEdge :: Int -> IntAdjacencyMap -> Bool
hasEdge a (IntAdjacencyMap m) = Map.member a m

-- | Remove an edge from the graph, updating all neighbour references.
-- Complexity: /O((|forks| + |joins| + |preds| + |succs|) * log n)/ time.
--
-- @
-- removeEdge x ('EdgeGraph.IntAdjacencyMap.Internal.edge' x) == 'EdgeGraph.IntAdjacencyMap.Internal.empty'
-- @
removeEdge :: Int -> IntAdjacencyMap -> IntAdjacencyMap
removeEdge x (IntAdjacencyMap m) = case Map.lookup x m of
    Nothing  -> IntAdjacencyMap m
    Just adj ->
        let newForks = Set.delete x (forks adj)
            newJoins = Set.delete x (joins adj)
            m1 = Map.delete x m
            m2 = Set.foldl' (\acc b ->
              Map.adjust (\r -> r { forks = newForks }) b acc)
              m1 newForks
            m3 = Set.foldl' (\acc b ->
              Map.adjust (\r -> r { joins = newJoins }) b acc)
              m2 newJoins
            m4 = Set.foldl' (\acc b ->
              Map.adjust (\r -> r { succs = newForks }) b acc)
              m3 (preds adj)
            m5 = Set.foldl' (\acc b ->
              Map.adjust (\r -> r { preds = newJoins }) b acc)
              m4 (succs adj)
        in IntAdjacencyMap m5

-- | Detach an edge from its source node. The edge gets a fresh source
-- while any other edges sharing the original source node remain together.
-- Complexity: /O((|forks| + |preds|) * log n)/ time.
--
-- @
-- detachPit x ('EdgeGraph.IntAdjacencyMap.Internal.edge' x)                      == 'EdgeGraph.IntAdjacencyMap.Internal.edge' x
-- detachPit 2 ('into' ('EdgeGraph.IntAdjacencyMap.Internal.edge' 1) ('EdgeGraph.IntAdjacencyMap.Internal.edge' 2))  == 'edges' [1, 2]
-- detachPit 1 ('pits' ('EdgeGraph.IntAdjacencyMap.Internal.edge' 1) ('EdgeGraph.IntAdjacencyMap.Internal.edge' 2))  == 'edges' [1, 2]
-- @
detachPit :: Int -> IntAdjacencyMap -> IntAdjacencyMap
detachPit a (IntAdjacencyMap m) = case Map.lookup a m of
    Nothing  -> IntAdjacencyMap m
    Just adj ->
        let oldForks = forks adj
            oldPreds = preds adj
            m1 = Map.adjust (\r -> r { forks = Set.singleton a
                                     , preds = Set.empty }) a m
            m2 = Set.foldl' (\acc b ->
              Map.adjust (\r -> r { forks = Set.delete a (forks r) }) b acc)
              m1 (Set.delete a oldForks)
            m3 = Set.foldl' (\acc b ->
              Map.adjust (\r -> r { succs = Set.delete a (succs r) }) b acc)
              m2 oldPreds
        in IntAdjacencyMap m3

-- | Detach an edge from its sink node. The edge gets a fresh sink
-- while any other edges sharing the original sink node remain together.
-- Complexity: /O((|joins| + |succs|) * log n)/ time.
--
-- @
-- detachTip x ('EdgeGraph.IntAdjacencyMap.Internal.edge' x)                      == 'EdgeGraph.IntAdjacencyMap.Internal.edge' x
-- detachTip 1 ('into' ('EdgeGraph.IntAdjacencyMap.Internal.edge' 1) ('EdgeGraph.IntAdjacencyMap.Internal.edge' 2))  == 'edges' [1, 2]
-- detachTip 1 ('tips' ('EdgeGraph.IntAdjacencyMap.Internal.edge' 1) ('EdgeGraph.IntAdjacencyMap.Internal.edge' 2))  == 'edges' [1, 2]
-- @
detachTip :: Int -> IntAdjacencyMap -> IntAdjacencyMap
detachTip a (IntAdjacencyMap m) = case Map.lookup a m of
    Nothing  -> IntAdjacencyMap m
    Just adj ->
      let oldJoins = joins adj
          oldSuccs = succs adj
          m1 = Map.adjust (\r -> r { joins = Set.singleton a
                                   , succs = Set.empty }) a m
          m2 = Set.foldl' (\acc b ->
            Map.adjust (\r -> r { joins = Set.delete a (joins r) }) b acc)
            m1 (Set.delete a oldJoins)
          m3 = Set.foldl' (\acc b ->
            Map.adjust (\r -> r { preds = Set.delete a (preds r) }) b acc)
            m2 oldSuccs
      in IntAdjacencyMap m3

-- | Transform a graph by applying a function to each edge.
--
-- @
-- gmap f 'EdgeGraph.IntAdjacencyMap.Internal.empty'    == 'EdgeGraph.IntAdjacencyMap.Internal.empty'
-- gmap f ('EdgeGraph.IntAdjacencyMap.Internal.edge' x) == 'EdgeGraph.IntAdjacencyMap.Internal.edge' (f x)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Int -> Int) -> IntAdjacencyMap -> IntAdjacencyMap
gmap f = fromIncidence . I.gmap f . toIncidence

-- | Construct the /induced subgraph/ of a given graph by removing edges
-- that do not satisfy a given predicate. Keeps only edges where @p@ returns
-- 'True', and updates all neighbour sets accordingly.
-- Complexity: /O(n * m)/ time.
--
-- @
-- induce (const True)  x == x
-- induce (const False) x == 'EdgeGraph.IntAdjacencyMap.Internal.empty'
-- @
induce :: (Int -> Bool) -> IntAdjacencyMap -> IntAdjacencyMap
induce p (IntAdjacencyMap m) = IntAdjacencyMap $ Map.map updateAdj kept
  where
    kept          = Map.filterWithKey (\k _ -> p k) m
    keptKeys      = Map.keysSet kept
    updateAdj adj = Adjacency
      { forks = Set.intersection (forks adj) keptKeys
      , joins = Set.intersection (joins adj) keptKeys
      , preds = Set.intersection (preds adj) keptKeys
      , succs = Set.intersection (succs adj) keptKeys
      }
