{-# LANGUAGE TypeApplications #-}

module Test.Incidence (testIncidence) where

import EdgeGraph.Class
import Testable.Graph
import Testable.Instances ()
import Arbitrary ()
import qualified EdgeGraph.Incidence.Internal as II

type G = II.Incidence Int

testIncidence :: IO ()
testIncidence = do
  putStrLn "\n============ Incidence ============"

  test "Consistency of arbitraryIncidence" $ sizeLimit $ \(m :: G) ->
      II.consistent m

  -- Type-specific: Show
  putStrLn "\n============ Show ============"
  test "show (empty  :: Incidence Int)                    == \"empty\"" $
        show (empty  :: G) == "empty"
  test "show (edge 1 :: Incidence Int)                    == \"edge 1\"" $
        show (edge 1 :: G) == "edge 1"
  test "show (overlay (edge 1) (edge 2) :: Incidence Int) == \"edges [1,2]\"" $
        show (overlay (edge 1) (edge 2) :: G) == "edges [1,2]"

  -- Shared test groups
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
