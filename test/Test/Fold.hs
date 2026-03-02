{-# LANGUAGE TypeApplications #-}
module Test.Fold (testFold) where

import EdgeGraph.Class
import Testable.Graph
import Testable.AlgebraicGraph
import Testable.Instances ()
import Arbitrary ()
import qualified EdgeGraph.Fold as F

type FO = F.Fold Int

testFold :: IO ()
testFold = do
    putStrLn "\n============ Fold ============"
    testAxiomsGroup @FO

    -- Type-specific: Show
    putStrLn "\n============ Show ============"
    test "show (empty :: Fold Int) == \"empty\"" $
          show (empty :: FO) == "empty"
    test "show (edge 1 :: Fold Int) == \"edge 1\"" $
          show (edge 1 :: FO) == "edge 1"

    -- Shared test groups (TestableGraph)
    testEmptyGroup @FO
    testEdgeGroup @FO
    testOverlayGroup @FO
    testIntoGroup @FO
    testEdgesGroup @FO
    testIsSubgraphOfGroup @FO
    testIsEmptyGroup @FO
    testHasEdgeGroup @FO
    testEdgeCountGroup @FO
    testNodeCountGroup @FO
    testEdgeListGroup @FO
    testEdgeSetGroup @FO
    testEdgeIntSetGroup @FO
    testPathGroup @FO
    testCircuitGroup @FO
    testCliqueGroup @FO
    testBicliqueGroup @FO
    testFlowerGroup @FO
    testNodeGroup @FO
    testRemoveEdgeGroup @FO
    testReplaceEdgeGroup @FO
    testGmapGroup @FO
    testInduceGroup @FO

    -- Shared test groups (TestableAlgebraicGraph)
    testSizeGroup @FO
    testFoldgGroup @FO
    testMergeEdgesGroup @FO
    testSplitEdgeGroup @FO
    testTransposeGroup @FO
    testSimplifyGroup @FO
    testBindGroup @FO
