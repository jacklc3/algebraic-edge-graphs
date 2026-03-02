{-# LANGUAGE TypeApplications #-}
module Test.Incidence (testIncidence) where

import EdgeGraph.Class
import Testable.Graph
import Testable.Instances ()
import Arbitrary ()
import qualified EdgeGraph.Incidence.Internal as II

type FI = II.Incidence Int

testIncidence :: IO ()
testIncidence = do
    putStrLn "\n============ Incidence ============"
    testAxiomsGroup @FI

    test "Consistency of arbitraryIncidence" $ sizeLimit $ \(m :: FI) ->
        II.consistent m

    -- Type-specific: Show
    putStrLn "\n============ Show ============"
    test "show (empty     :: Incidence Int) == \"empty\"" $
          show (empty     :: FI) == "empty"
    test "show (edge 1    :: Incidence Int) == \"edge 1\"" $
          show (edge 1    :: FI) == "edge 1"
    test "show (overlay (edge 1) (edge 2) :: Incidence Int) == \"edges [1,2]\"" $
          show (overlay (edge 1) (edge 2) :: FI) == "edges [1,2]"

    -- Shared test groups
    testEmptyGroup @FI
    testEdgeGroup @FI
    testOverlayGroup @FI
    testIntoGroup @FI
    testEdgesGroup @FI
    testIsSubgraphOfGroup @FI
    testIsEmptyGroup @FI
    testHasEdgeGroup @FI
    testEdgeCountGroup @FI
    testNodeCountGroup @FI
    testEdgeListGroup @FI
    testEdgeSetGroup @FI
    testEdgeIntSetGroup @FI
    testPathGroup @FI
    testCircuitGroup @FI
    testCliqueGroup @FI
    testBicliqueGroup @FI
    testFlowerGroup @FI
    testNodeGroup @FI
    testRemoveEdgeGroup @FI
    testReplaceEdgeGroup @FI
    testGmapGroup @FI
    testInduceGroup @FI
