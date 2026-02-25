-----------------------------------------------------------------------------
-- |
-- Module     : EdgeGraph.IntAdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
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

import qualified Data.IntSet                 as IntSet
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified EdgeGraph.Class             as C
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

instance Num IntAdjacencyMap where
    fromInteger = edge . fromInteger
    (+)         = overlay
    (*)         = into
    signum      = const empty
    abs         = id
    negate      = id

-- | Convert an 'IntAdjacencyMap' to its equivalent 'I.Incidence' representation.
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

-- | Convert an 'I.Incidence' to an 'IntAdjacencyMap'.
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

-- | Check consistency of an 'IntAdjacencyMap'.
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

empty :: IntAdjacencyMap
empty = IntAdjacencyMap Map.empty

edge :: Int -> IntAdjacencyMap
edge a = IntAdjacencyMap $ Map.singleton a $ Adjacency
    { forks = Set.singleton a
    , joins = Set.singleton a
    , preds = Set.empty
    , succs = Set.empty
    }

overlay :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
overlay x y = fromIncidence $ I.overlay (toIncidence x) (toIncidence y)

into :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
into x y = fromIncidence $ I.into (toIncidence x) (toIncidence y)

pits :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
pits x y = fromIncidence $ I.pits (toIncidence x) (toIncidence y)

tips :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
tips x y = fromIncidence $ I.tips (toIncidence x) (toIncidence y)

edges :: [Int] -> IntAdjacencyMap
edges = fromIncidence . I.edges

fromNodeList :: [I.Node Int] -> IntAdjacencyMap
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
fromIncidenceList :: [([Int], [Int])] -> IntAdjacencyMap
fromIncidenceList = fromIncidence . I.fromIncidenceList

nodeList :: IntAdjacencyMap -> [I.Node Int]
nodeList = I.nodeList . toIncidence

nodeSet :: IntAdjacencyMap -> Set (I.Node Int)
nodeSet = I.nodeSet . toIncidence

nodeCount :: IntAdjacencyMap -> Int
nodeCount = I.nodeCount . toIncidence

isEmpty :: IntAdjacencyMap -> Bool
isEmpty (IntAdjacencyMap m) = Map.null m

edgeList :: IntAdjacencyMap -> [Int]
edgeList (IntAdjacencyMap m) = Map.keys m

edgeIntSet :: IntAdjacencyMap -> IntSet
edgeIntSet (IntAdjacencyMap m) = IntSet.fromDistinctAscList (Map.keys m)

-- | The sorted /adjacency list/ of a graph. Each entry is an edge
-- paired with its 'Adjacency' record.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- adjacencyList 'empty'    == []
-- @
adjacencyList :: IntAdjacencyMap -> [(Int, Adjacency)]
adjacencyList (IntAdjacencyMap m) = Map.toAscList m

edgeCount :: IntAdjacencyMap -> Int
edgeCount (IntAdjacencyMap m) = Map.size m

hasEdge :: Int -> IntAdjacencyMap -> Bool
hasEdge a (IntAdjacencyMap m) = Map.member a m

removeEdge :: Int -> IntAdjacencyMap -> IntAdjacencyMap
removeEdge x (IntAdjacencyMap m) = case Map.lookup x m of
    Nothing  -> IntAdjacencyMap m
    Just adj ->
        let xForks   = forks adj
            xJoins   = joins adj
            xPreds   = preds adj
            xSuccs   = succs adj
            newForks = Set.delete x xForks
            newJoins = Set.delete x xJoins
            m1 = Map.delete x m
            m2 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { forks = newForks }) b acc)
                    m1 newForks
            m3 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { joins = newJoins }) b acc)
                    m2 newJoins
            m4 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { succs = newForks }) b acc)
                    m3 xPreds
            m5 = Set.foldl' (\acc b ->
                    Map.adjust (\r -> r { preds = newJoins }) b acc)
                    m4 xSuccs
        in IntAdjacencyMap m5

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

gmap :: (Int -> Int) -> IntAdjacencyMap -> IntAdjacencyMap
gmap f = fromIncidence . I.gmap f . toIncidence

induce :: (Int -> Bool) -> IntAdjacencyMap -> IntAdjacencyMap
induce p (IntAdjacencyMap m) = IntAdjacencyMap $ Map.map updateAdj kept
  where
    kept     = Map.filterWithKey (\k _ -> p k) m
    keptKeys = Map.keysSet kept
    updateAdj adj = Adjacency
        { forks = Set.intersection (forks adj) keptKeys
        , joins = Set.intersection (joins adj) keptKeys
        , preds = Set.intersection (preds adj) keptKeys
        , succs = Set.intersection (succs adj) keptKeys
        }
