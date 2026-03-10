{-# LANGUAGE TypeApplications #-}

module Test.AdjacencyMap (testAdjacencyMap) where

import EdgeGraph.Class
import Testable.Graph
import Testable.AdjacencyGraph
import Testable.Instances ()
import Arbitrary ()
import qualified EdgeGraph.AdjacencyMap as AM

import qualified Data.Set as Set

type G = AM.AdjacencyMap Int

testAdjacencyMap :: IO ()
testAdjacencyMap = do
  putStrLn "\n============ AdjacencyMap ============"

  -- Shared test groups (TestableGraph)
  testAxiomsGroup @G
  testEmptyGroup @G
  testEdgeGroup @G
  testOverlayGroup @G
  testIntoGroup @G
  testEdgesGroup @G
  testIsSubgraphOfGroup @G
  testIsEmptyGroup @G
  testHasEdgeGroup @G
  testEdgeCountGroup @G
  testNodeCountGroup @G
  testEdgeListGroup @G
  testEdgeSetGroup @G
  testEdgeIntSetGroup @G
  testPathGroup @G
  testCircuitGroup @G
  testCliqueGroup @G
  testBicliqueGroup @G
  testFlowerGroup @G
  testNodeGroup @G
  testRemoveEdgeGroup @G
  testReplaceEdgeGroup @G
  testGmapGroup @G
  testInduceGroup @G

  -- Shared test groups (TestableAdjacencyGraph)
  testConsistentGroup @G
  testPostsetGroup @G
  testPresetGroup @G
  testDfsForestGroup @G
  testTopSortGroup @G
  testIsTopSortGroup @G
  testDetachPitGroup @G
  testDetachTipGroup @G
  testToFromIncidenceGroup @G
  testAdjacencyEdgeGroup @G
  testAdjacencyConnectGroup @G

  -- Type-specific: scc
  putStrLn "\n============ scc ============"
  test "scc empty            == empty" $
        AM.scc (empty :: G) == empty
  test "scc (edge x)         == edge (Set.singleton x)" $ \(x :: Int) ->
        AM.scc (edge x) == edge (Set.singleton x)
  test "scc (circuit (1:xs)) == circuit [Set.fromList (1:xs)]" $
      sizeLimit $ \(xs :: [Int]) ->
        AM.scc (circuit (1:xs) :: G) == circuit [Set.fromList (1:xs)]
