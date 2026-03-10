{-# LANGUAGE TypeApplications #-}

module Test.IntAdjacencyMap (testIntAdjacencyMap) where

import Testable.Graph
import Testable.AdjacencyGraph
import Testable.Instances ()
import Arbitrary ()
import EdgeGraph.IntAdjacencyMap (IntAdjacencyMap)

type G = IntAdjacencyMap

testIntAdjacencyMap :: IO ()
testIntAdjacencyMap = do
  putStrLn "\n============ IntAdjacencyMap ============"

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
