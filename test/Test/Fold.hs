{-# LANGUAGE TypeApplications #-}

module Test.Fold (testFold) where

import EdgeGraph.Class
import Testable.Graph
import Testable.AlgebraicGraph
import Testable.Instances ()
import Arbitrary ()
import Test.QuickCheck (Property, Testable, mapSize)

import qualified EdgeGraph                as EG
import qualified EdgeGraph.Fold           as F
import qualified Data.Map.Strict          as Map

type G = F.Fold Int

testFold :: IO ()
testFold = do
  putStrLn "\n============ Fold ============"

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

  putStrLn "\n============ shortestPaths ============"
  test "shortestPaths id empty == Map.empty" $
        F.shortestPaths id (F.empty :: G) == Map.empty
  test "shortestPaths id (edge x) has self-distances 0" $ \(x :: Int) ->
    let sp = F.shortestPaths id (F.edge x :: G)
    in Map.lookup (F.Pit x, F.Pit x) sp == Just 0
    && Map.lookup (F.Tip x, F.Tip x) sp == Just 0
  test "shortestPaths id (edge x) has traversal distance x" $ \(x :: Int) ->
    let sp = F.shortestPaths id (F.edge x :: G)
    in Map.lookup (F.Pit x, F.Tip x) sp == Just x
  test "shortestPaths id (into (edge 1) (edge 2)) connects tip 1 to pit 2" $
    let g  = F.into (F.edge 1) (F.edge 2) :: G
        sp = F.shortestPaths id g
    in Map.lookup (F.Tip 1, F.Pit 2) sp == Just 0
  test "shortestPaths id (into (edge 1) (edge 2)) pit 1 to tip 2 == 3" $
    let g  = F.into (F.edge 1) (F.edge 2) :: G
        sp = F.shortestPaths id g
    in Map.lookup (F.Pit 1, F.Tip 2) sp == Just 3
  test "shortestPaths (const 1) gives hop count" $
    let g  = F.into (F.edge 1) (F.edge 2) :: G
        sp = F.shortestPaths (const (1 :: Int)) g
    in Map.lookup (F.Pit 1, F.Tip 2) sp == Just 2

  putStrLn "\n============ toFold ============"
  test "shortestPaths agrees via toFold" $ smallLimit $ \(g :: EG.EdgeGraph Int) ->
    let spDirect = F.shortestPaths id (EG.foldg F.empty F.edge F.overlay F.into F.pits F.tips g)
        spToFold = F.shortestPaths id (EG.toFold g)
    in spDirect == spToFold

  putStrLn "\n============ reachable ============"
  test "reachable empty == Map.empty" $
        F.reachable (F.empty :: G) == Map.empty
  test "reachable (edge x) has pit-to-tip" $ \(x :: Int) ->
    let r = F.reachable (F.edge x :: G)
    in Map.lookup (F.Pit x, F.Tip x) r == Just True
  test "reachable (into (edge 1) (edge 2)) pit 1 reaches tip 2" $
    let g = F.into (F.edge 1) (F.edge 2) :: G
        r = F.reachable g
    in Map.lookup (F.Pit 1, F.Tip 2) r == Just True
  test "reachable (edge 1 + edge 2) pit 1 does not reach tip 2" $
    let g = F.overlay (F.edge 1) (F.edge 2) :: G
        r = F.reachable g
    in Map.lookup (F.Pit 1, F.Tip 2) r == Nothing

  putStrLn "\n============ isReachable ============"
  test "isReachable 1 2 (into (edge 1) (edge 2)) == True" $
        F.isReachable 1 2 (F.into (F.edge 1) (F.edge 2) :: G) == True
  test "isReachable 2 1 (into (edge 1) (edge 2)) == False" $
        F.isReachable 2 1 (F.into (F.edge 1) (F.edge 2) :: G) == False
  test "isReachable 1 1 (edge 1) == False" $
        F.isReachable 1 1 (F.edge 1 :: G) == False
  test "isReachable 1 1 (into (edge 1) (edge 1)) == True" $
        F.isReachable 1 1 (F.into (F.edge 1) (F.edge 1) :: G) == True

  putStrLn "\n============ isAcyclic ============"
  test "isAcyclic empty == True" $
        F.isAcyclic (F.empty :: G) == True
  test "isAcyclic (edge 1) == True" $
        F.isAcyclic (F.edge 1 :: G) == True
  test "isAcyclic (into (edge 1) (edge 2)) == True" $
        F.isAcyclic (F.into (F.edge 1) (F.edge 2) :: G) == True
  test "isAcyclic (into (edge 1) (edge 1)) == False (petal)" $
        F.isAcyclic (F.into (F.edge 1) (F.edge 1) :: G) == False
  test "isAcyclic (circuit [1,2]) == False" $
        F.isAcyclic (F.circuit [1, 2] :: G) == False
  test "isAcyclic (path [1,2,3]) == True" $
        F.isAcyclic (F.path [1, 2, 3] :: G) == True

  putStrLn "\n============ Fold consistency ============"
  test "reachable respects overlay commutativity" $ smallLimit $ \(x :: G) y ->
    F.reachable (F.overlay x y) == F.reachable (F.overlay y x)
  test "reachable respects overlay idempotence" $ smallLimit $ \(x :: G) ->
    F.reachable (F.overlay x x) == F.reachable x
  test "reachable respects empty identity for into" $ smallLimit $ \(x :: G) ->
    F.reachable (F.into F.empty x) == F.reachable x

  -- widestPaths
  putStrLn "\n============ widestPaths ============"
  test "widestPaths id empty == Map.empty" $
        F.widestPaths id (F.empty :: G) == Map.empty
  test "widestPaths id (edge x) has traversal width x" $ \(x :: Int) ->
    let wp = F.widestPaths id (F.edge x :: G)
    in Map.lookup (F.Pit x, F.Tip x) wp == Just x
  test "widestPaths id (into (edge 5) (edge 3)) bottleneck == 3" $
    let g  = F.into (F.edge 5) (F.edge 3) :: G
        wp = F.widestPaths id g
    in Map.lookup (F.Pit 5, F.Tip 3) wp == Just 3
  test "widestPaths id (into (edge 2) (edge 8)) bottleneck == 2" $
    let g  = F.into (F.edge 2) (F.edge 8) :: G
        wp = F.widestPaths id g
    in Map.lookup (F.Pit 2, F.Tip 8) wp == Just 2

-- | Tighter size limit for fold tests involving transitive closure.
smallLimit :: Testable a => a -> Property
smallLimit = mapSize (min 5)
