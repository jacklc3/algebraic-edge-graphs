{-# LANGUAGE TypeApplications #-}
module Test.IntAdjacencyMap (testIntAdjacencyMap) where

import Testable.Graph
import Testable.AdjacencyGraph
import Testable.Instances ()
import Arbitrary ()
import EdgeGraph.IntAdjacencyMap.Internal (IntAdjacencyMap)

testIntAdjacencyMap :: IO ()
testIntAdjacencyMap = do
    putStrLn "\n============ IntAdjacencyMap ============"
    testAxiomsGroup @IntAdjacencyMap

    -- Shared test groups (TestableGraph)
    testEmptyGroup @IntAdjacencyMap
    testEdgeGroup @IntAdjacencyMap
    testOverlayGroup @IntAdjacencyMap
    testIntoGroup @IntAdjacencyMap
    testEdgesGroup @IntAdjacencyMap
    testIsSubgraphOfGroup @IntAdjacencyMap
    testIsEmptyGroup @IntAdjacencyMap
    testHasEdgeGroup @IntAdjacencyMap
    testEdgeCountGroup @IntAdjacencyMap
    testNodeCountGroup @IntAdjacencyMap
    testEdgeListGroup @IntAdjacencyMap
    testEdgeSetGroup @IntAdjacencyMap
    testEdgeIntSetGroup @IntAdjacencyMap
    testPathGroup @IntAdjacencyMap
    testCircuitGroup @IntAdjacencyMap
    testCliqueGroup @IntAdjacencyMap
    testBicliqueGroup @IntAdjacencyMap
    testFlowerGroup @IntAdjacencyMap
    testNodeGroup @IntAdjacencyMap
    testRemoveEdgeGroup @IntAdjacencyMap
    testReplaceEdgeGroup @IntAdjacencyMap
    testGmapGroup @IntAdjacencyMap
    testInduceGroup @IntAdjacencyMap

    -- Shared test groups (TestableAdjacencyGraph)
    testConsistentGroup @IntAdjacencyMap
    testPostsetGroup @IntAdjacencyMap
    testPresetGroup @IntAdjacencyMap
    testDfsForestGroup @IntAdjacencyMap
    testTopSortGroup @IntAdjacencyMap
    testIsTopSortGroup @IntAdjacencyMap
    testDetachPitGroup @IntAdjacencyMap
    testDetachTipGroup @IntAdjacencyMap
    testToFromIncidenceGroup @IntAdjacencyMap

    -- Shared test groups (TestableAdjacencyGraph) — adjacency records
    testAdjacencyEdgeGroup @IntAdjacencyMap
    testAdjacencyConnectGroup @IntAdjacencyMap
