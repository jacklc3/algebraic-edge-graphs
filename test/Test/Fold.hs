{-# LANGUAGE TypeApplications #-}

module Test.Fold (testFold) where

import EdgeGraph.Class
import Testable.Graph
import Testable.AlgebraicGraph
import Testable.Instances ()
import Arbitrary ()
import qualified EdgeGraph.Fold as F

type G = F.Fold Int

testFold :: IO ()
testFold = do
  putStrLn "\n============ Fold ============"

  -- Type-specific: Show
  putStrLn "\n============ Show ============"
  test "show (empty :: Fold Int)  == \"empty\"" $
        show (empty :: G) == "empty"
  test "show (edge 1 :: Fold Int) == \"edge 1\"" $
        show (edge 1 :: G) == "edge 1"

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

  -- Shared test groups (TestableAlgebraicGraph)
  testSizeGroup @G
  testFoldgGroup @G
  testMergeEdgesGroup @G
  testSplitEdgeGroup @G
  testTransposeGroup @G
  testSimplifyGroup @G
  testBindGroup @G
