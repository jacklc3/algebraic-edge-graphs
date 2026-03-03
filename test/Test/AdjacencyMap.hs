{-# LANGUAGE TypeApplications #-}

module Test.AdjacencyMap (testAdjacencyMap) where

import EdgeGraph.Class
import Testable.Graph
import Testable.AdjacencyGraph
import Testable.Instances ()
import Arbitrary ()
import qualified EdgeGraph.AdjacencyMap as AM

import qualified Data.Set as Set

type AI = AM.AdjacencyMap Int

testAdjacencyMap :: IO ()
testAdjacencyMap = do
  putStrLn "\n============ AdjacencyMap ============"

  -- Shared test groups (TestableGraph)
  testAxiomsGroup @AI
  testEmptyGroup @AI
  testEdgeGroup @AI
  testOverlayGroup @AI
  testIntoGroup @AI
  testEdgesGroup @AI
  testIsSubgraphOfGroup @AI
  testIsEmptyGroup @AI
  testHasEdgeGroup @AI
  testEdgeCountGroup @AI
  testNodeCountGroup @AI
  testEdgeListGroup @AI
  testEdgeSetGroup @AI
  testEdgeIntSetGroup @AI
  testPathGroup @AI
  testCircuitGroup @AI
  testCliqueGroup @AI
  testBicliqueGroup @AI
  testFlowerGroup @AI
  testNodeGroup @AI
  testRemoveEdgeGroup @AI
  testReplaceEdgeGroup @AI
  testGmapGroup @AI
  testInduceGroup @AI

  -- Shared test groups (TestableAdjacencyGraph)
  testConsistentGroup @AI
  testPostsetGroup @AI
  testPresetGroup @AI
  testDfsForestGroup @AI
  testTopSortGroup @AI
  testIsTopSortGroup @AI
  testDetachPitGroup @AI
  testDetachTipGroup @AI
  testToFromIncidenceGroup @AI
  testAdjacencyEdgeGroup @AI
  testAdjacencyConnectGroup @AI

  -- Type-specific: scc
  putStrLn "\n============ scc ============"
  test "scc empty == empty" $
        AM.scc (empty :: AI) == empty
  test "scc (edge x) == edge (Set.singleton x)" $ \(x :: Int) ->
        AM.scc (edge x) == edge (Set.singleton x)
  test "scc (circuit (1:xs)) == circuit [Set.fromList (1:xs)]" $
      sizeLimit $ \(xs :: [Int]) ->
        AM.scc (circuit (1:xs) :: AI) == circuit [Set.fromList (1:xs)]
