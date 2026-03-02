{-# LANGUAGE TypeApplications, ViewPatterns #-}

module Test.EdgeGraph (testGraph) where

import EdgeGraph.Class
import Testable.Graph
import Testable.AlgebraicGraph
import Testable.Instances ()
import Arbitrary ()
import EdgeGraph ((===))
import qualified EdgeGraph as EG

type G  = EG.EdgeGraph Int

testGraph :: IO ()
testGraph = do
  putStrLn "\n============ EdgeGraph ============"

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

  -- Type-specific: structural equality
  putStrLn "\n============ (===) ============"
  test "    x === x                          == True" $ sizeLimit $ \(x :: G) ->
           (x === x)                         == True
  test "    x === overlay x empty            == False" $ sizeLimit $ \(x :: G) ->
           (x === overlay x empty)           == False
  test "overlay x y === overlay x y          == True" $ sizeLimit $ \(x :: G) y ->
       (overlay x y === overlay x y)         == True
  test "overlay (edge 1) (edge 2) === overlay (edge 2) (edge 1) == False" $
       (overlay (edge 1) (edge 2) === overlay (edge 2) (edge (1 :: Int))) == False

  -- Type-specific: foldg length (not in Fold)
  putStrLn "\n============ foldg (length) ============"
  test "foldg 0     (const 1) (+) (+) (+) (+) == length" $ sizeLimit $ \(x :: G) ->
        EG.foldg 0 (const 1) (+) (+) (+) (+) x == length x
