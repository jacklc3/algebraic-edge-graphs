{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables #-}

module Testable.AdjacencyGraph (
  -- * TestableAdjacencyGraph class (AdjacencyMap, IntAdjacencyMap)
  TestableAdjacencyGraph (..),

  -- * Shared test groups
  testConsistentGroup, testPostsetGroup, testPresetGroup,
  testDfsForestGroup, testTopSortGroup, testIsTopSortGroup,
  testDetachPitGroup, testDetachTipGroup, testToFromIncidenceGroup,
  testAdjacencyEdgeGroup, testAdjacencyConnectGroup,
) where

import EdgeGraph.Class
import EdgeGraph.Incidence.Internal (Incidence)
import Data.Tree (Forest, Tree (..))
import qualified Data.Set as Set

import Testable.Graph (TestableGraph(..), test, sizeLimit)

-- ---------------------------------------------------------------------------
-- TestableAdjacencyGraph class (AdjacencyMap, IntAdjacencyMap)
-- ---------------------------------------------------------------------------

-- | Type class for adjacency map graph types that support directed graph
-- queries and structural consistency checks.
class TestableGraph g => TestableAdjacencyGraph g where
  consistent    :: g -> Bool
  postset       :: Edge g -> g -> Set.Set (Edge g)
  preset        :: Edge g -> g -> Set.Set (Edge g)
  dfsForest     :: g -> Forest (Edge g)
  topSort       :: g -> Maybe [Edge g]
  isTopSort     :: [Edge g] -> g -> Bool
  detachPit     :: Edge g -> g -> g
  detachTip     :: Edge g -> g -> g
  toIncidence   :: g -> Incidence (Edge g)
  fromIncidence :: Incidence (Edge g) -> g
  forkSet       :: Edge g -> g -> Set.Set (Edge g)
  joinSet       :: Edge g -> g -> Set.Set (Edge g)
  predSet       :: Edge g -> g -> Set.Set (Edge g)
  succSet       :: Edge g -> g -> Set.Set (Edge g)

-- ---------------------------------------------------------------------------
-- Shared test groups (TestableAdjacencyGraph)
-- ---------------------------------------------------------------------------

testConsistentGroup :: forall g. TestableAdjacencyGraph g => IO ()
testConsistentGroup = do
  putStrLn "\n============ consistent ============"
  test "Consistency of arbitrary graphs" $ sizeLimit $ \(x :: g) ->
        consistent x

testPostsetGroup :: forall g. TestableAdjacencyGraph g => IO ()
testPostsetGroup = do
  putStrLn "\n============ postset ============"
  test "postset x empty                    == Set.empty" $ \(x :: Edge g) ->
        postset x (empty :: g) == Set.empty
  test "postset x (edge x)                 == Set.empty" $ \(x :: Edge g) ->
        postset x (edge x :: g) == Set.empty
  test "postset 1 (into (edge 1) (edge 2)) == Set.singleton 2" $
        postset 1 (into (edge 1) (edge 2) :: g) == Set.singleton 2

testPresetGroup :: forall g. TestableAdjacencyGraph g => IO ()
testPresetGroup = do
  putStrLn "\n============ preset ============"
  test "preset x empty                    == Set.empty" $ \(x :: Edge g) ->
        preset x (empty :: g) == Set.empty
  test "preset x (edge x)                 == Set.empty" $ \(x :: Edge g) ->
        preset x (edge x :: g) == Set.empty
  test "preset 2 (into (edge 1) (edge 2)) == Set.singleton 1" $
        preset 2 (into (edge 1) (edge 2) :: g) == Set.singleton 1

testDfsForestGroup :: forall g. TestableAdjacencyGraph g => IO ()
testDfsForestGroup = do
  putStrLn "\n============ dfsForest ============"
  test "dfsForest empty                       == []" $
        dfsForest (empty :: g) == []
  test "dfsForest (edge x)                    == [Node x []]" $ \(x :: Edge g) ->
        dfsForest (edge x :: g) == [Node x []]
  test "dfsForest (path [1,2,3])              == [Node 1 [Node 2 [Node 3 []]]]" $
        dfsForest (path [1,2,3] :: g) == [Node 1 [Node 2 [Node 3 []]]]
  test "isSubgraphOf (forest $ dfsForest x) x == True" $ sizeLimit $ \(x :: g) ->
        isSubgraphOf (forest $ dfsForest x) x == True
  test "dfsForest . forest . dfsForest        == dfsForest" $ sizeLimit $ \(x :: g) ->
        dfsForest (forest (dfsForest x) :: g) == dfsForest x

testTopSortGroup :: forall g. TestableAdjacencyGraph g => IO ()
testTopSortGroup = do
  putStrLn "\n============ topSort ============"
  test "topSort (edge x)                    == Just [x]" $ \(x :: Edge g) ->
        topSort (edge x :: g) == Just [x]
  test "topSort (into (edge 1) (edge 2))    == Just [1,2]" $
        topSort (into (edge 1) (edge 2) :: g) == Just [1,2]
  test "topSort (circuit [1,2])             == Nothing" $
        topSort (circuit [1,2] :: g) == Nothing
  test "topSort (path [1,2,3])              == Just [1,2,3]" $
        topSort (path [1,2,3] :: g) == Just [1,2,3]
  test "fmap (flip isTopSort x) (topSort x) /= Just False" $ sizeLimit $ \(x :: g) ->
        fmap (flip isTopSort x) (topSort x) /= Just False

testIsTopSortGroup :: forall g. TestableAdjacencyGraph g => IO ()
testIsTopSortGroup = do
  putStrLn "\n============ isTopSort ============"
  test "isTopSort [] empty     == True" $
        isTopSort [] (empty :: g) == True
  test "isTopSort [x] (edge x) == True" $ \(x :: Edge g) ->
        isTopSort [x] (edge x :: g) == True
  test "isTopSort [] (edge x)  == False" $ \(x :: Edge g) ->
        isTopSort [] (edge x :: g) == False

testDetachPitGroup :: forall g. TestableAdjacencyGraph g => IO ()
testDetachPitGroup = do
  putStrLn "\n============ detachPit ============"
  test "detachPit x (edge x)                 == edge x" $ \(x :: Edge g) ->
        detachPit x (edge x :: g) == (edge x :: g)
  test "detachPit 2 (into (edge 1) (edge 2)) == edges [1, 2]" $
        detachPit 2 (into (edge 1) (edge 2) :: g) == edges [1, 2]
  test "detachPit 1 (pits (edge 1) (edge 2)) == edges [1, 2]" $
        detachPit 1 (pits (edge 1) (edge 2) :: g) == edges [1, 2]
  test "detachPit preserves edges" $ sizeLimit $ \(x :: g) (a :: Edge g) ->
        edgeSet (detachPit a x) == edgeSet x
  test "detachPit consistent" $ sizeLimit $ \(x :: g) (a :: Edge g) ->
        consistent (detachPit a x)

testDetachTipGroup :: forall g. TestableAdjacencyGraph g => IO ()
testDetachTipGroup = do
  putStrLn "\n============ detachTip ============"
  test "detachTip x (edge x)                 == edge x" $ \(x :: Edge g) ->
        detachTip x (edge x :: g) == (edge x :: g)
  test "detachTip 1 (into (edge 1) (edge 2)) == edges [1, 2]" $
        detachTip 1 (into (edge 1) (edge 2) :: g) == edges [1, 2]
  test "detachTip 1 (tips (edge 1) (edge 2)) == edges [1, 2]" $
        detachTip 1 (tips (edge 1) (edge 2) :: g) == edges [1, 2]
  test "detachTip preserves edges" $ sizeLimit $ \(x :: g) (a :: Edge g) ->
        edgeSet (detachTip a x) == edgeSet x
  test "detachTip consistent" $ sizeLimit $ \(x :: g) (a :: Edge g) ->
        consistent (detachTip a x)

testToFromIncidenceGroup :: forall g. TestableAdjacencyGraph g => IO ()
testToFromIncidenceGroup = do
  putStrLn "\n============ toIncidence/fromIncidence ============"
  test "fromIncidence (toIncidence m) == m" $ sizeLimit $ \(m :: g) ->
        fromIncidence (toIncidence m) == m

testAdjacencyEdgeGroup :: forall g. TestableAdjacencyGraph g => IO ()
testAdjacencyEdgeGroup = do
  putStrLn "\n============ Adjacency record (edge) ============"
  test "edge x: forkSet == {x}" $ \(x :: Edge g) ->
        forkSet x (edge x :: g) == Set.singleton x
  test "edge x: joinSet == {x}" $ \(x :: Edge g) ->
        joinSet x (edge x :: g) == Set.singleton x
  test "edge x: predSet == {}" $ \(x :: Edge g) ->
        predSet x (edge x :: g) == Set.empty
  test "edge x: succSet == {}" $ \(x :: Edge g) ->
        succSet x (edge x :: g) == Set.empty

testAdjacencyConnectGroup :: forall g. TestableAdjacencyGraph g => IO ()
testAdjacencyConnectGroup = do
  putStrLn "\n============ Adjacency record (into/pits/tips) ============"
  test "into (edge x) (edge y), x /= y: succSet x == {y}" $ \(x :: Edge g) ->
        let y = x + 1
        in succSet x (into (edge x) (edge y) :: g) == Set.singleton y
  test "into (edge x) (edge y), x /= y: predSet y == {x}" $ \(x :: Edge g) ->
        let y = x + 1
        in predSet y (into (edge x) (edge y) :: g) == Set.singleton x
  test "into (edge x) (edge y), x /= y: forkSet x == {x}" $ \(x :: Edge g) ->
        let y = x + 1
        in forkSet x (into (edge x) (edge y) :: g) == Set.singleton x
  test "into (edge x) (edge y), x /= y: joinSet y == {y}" $ \(x :: Edge g) ->
        let y = x + 1
        in joinSet y (into (edge x) (edge y) :: g) == Set.singleton y
  test "pits (edge x) (edge y), x /= y: forkSet x == {x, y}" $ \(x :: Edge g) ->
        let y = x + 1
        in forkSet x (pits (edge x) (edge y) :: g) == Set.fromList [x, y]
  test "tips (edge x) (edge y), x /= y: joinSet y == {x, y}" $ \(x :: Edge g) ->
        let y = x + 1
        in joinSet y (tips (edge x) (edge y) :: g) == Set.fromList [x, y]
