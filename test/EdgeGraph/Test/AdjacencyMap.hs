{-# LANGUAGE ViewPatterns #-}
module EdgeGraph.Test.AdjacencyMap (testAdjacencyMap) where

import Data.Tree (Tree (..))

import EdgeGraph.AdjacencyMap
import EdgeGraph.AdjacencyMap.Internal
import EdgeGraph.Test

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

type AI = AdjacencyMap Int
type II = Int -> Int

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testAdjacencyMap :: IO ()
testAdjacencyMap = do
    putStrLn "\n============ AdjacencyMap ============"
    test "Axioms of edge graphs" $ sizeLimit $ (axioms :: GraphTestsuite AI)

    test "Consistency of arbitraryAdjacencyMap" $ sizeLimit $ \(m :: AI) ->
        consistent m

    putStrLn "\n============ empty ============"
    test "isEmpty     empty == True" $
          isEmpty    (empty :: AI) == True
    test "edgeCount empty == 0" $
          edgeCount(empty :: AI) == 0
    test "nodeCount   empty == 0" $
          nodeCount  (empty :: AI) == 0

    putStrLn "\n============ edge ============"
    test "isEmpty     (edge x) == False" $ \(x :: Int) ->
          isEmpty     (edge x) == False
    test "hasEdge x (edge x) == True" $ \(x :: Int) ->
          hasEdge x (edge x) == True
    test "edgeCount (edge x) == 1" $ \(x :: Int) ->
          edgeCount (edge x) == 1
    test "nodeCount   (edge x) == 2" $ \(x :: Int) ->
          nodeCount   (edge x) == 2

    putStrLn "\n============ overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: AI) y ->
          isEmpty     (overlay x y) == (isEmpty   x   && isEmpty   y)

    putStrLn "\n============ into ============"
    test "isEmpty     (into x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: AI) y ->
          isEmpty     (into x y) == (isEmpty   x   && isEmpty   y)

    putStrLn "\n============ isSubgraphOf ============"
    test "isSubgraphOf empty x == True" $ sizeLimit $ \(x :: AI) ->
          isSubgraphOf empty x == True
    test "isSubgraphOf x (overlay x y) == True" $ sizeLimit $ \(x :: AI) y ->
          isSubgraphOf x (overlay x y) == True

    putStrLn "\n============ edgeCount ============"
    test "edgeCount empty == 0" $
          edgeCount (empty :: AI) == 0
    test "edgeCount (edge x) == 1" $ \(x :: Int) ->
          edgeCount (edge x) == 1

    putStrLn "\n============ nodeCount ============"
    test "nodeCount empty == 0" $
          nodeCount (empty :: AI) == 0
    test "nodeCount (edge x) == 2" $ \(x :: Int) ->
          nodeCount (edge x) == 2

    putStrLn "\n============ path ============"
    test "path []  == empty" $
          path []  == (empty :: AI)
    test "path [x] == edge x" $ \(x :: Int) ->
          path [x] == (edge x :: AI)

    putStrLn "\n============ removeEdge ============"
    test "removeEdge x (edge x) == empty" $ \(x :: Int) ->
          removeEdge x (edge x) == (empty :: AI)

    putStrLn "\n============ gmap ============"
    test "gmap f empty    == empty" $ \(apply -> f :: II) ->
          gmap f empty    == empty
    test "gmap f (edge x) == edge (f x)" $ \(apply -> f :: II) x ->
          gmap f (edge x) == edge (f x)
    test "gmap id         == id" $ sizeLimit $ \x ->
          gmap id x       == (x :: AI)

    putStrLn "\n============ induce ============"
    test "induce (const True)  x == x" $ sizeLimit $ \(x :: AI) ->
          induce (const True)  x == x
    test "induce (const False) x == empty" $ sizeLimit $ \(x :: AI) ->
          induce (const False) x == (empty :: AI)

    putStrLn "\n============ toIncidence/fromIncidence ============"
    test "fromIncidence (toIncidence m) == m" $ sizeLimit $ \(m :: AI) ->
          fromIncidence (toIncidence m) == m

    putStrLn "\n============ Adjacency record (edge) ============"
    test "edge x: forks == {x}" $ \(x :: Int) ->
          forks (adjacencyMap (edge x) Map.! x) == Set.singleton x
    test "edge x: joins == {x}" $ \(x :: Int) ->
          joins (adjacencyMap (edge x) Map.! x) == Set.singleton x
    test "edge x: preds == {}" $ \(x :: Int) ->
          preds (adjacencyMap (edge x) Map.! x) == Set.empty
    test "edge x: succs == {}" $ \(x :: Int) ->
          succs (adjacencyMap (edge x) Map.! x) == Set.empty

    putStrLn "\n============ Adjacency record (into) ============"
    test "into (edge x) (edge y), x /= y: succs of x == {y}" $ \(x :: Int) ->
          let y = x + 1
              m = adjacencyMap (into (edge x) (edge y))
          in succs (m Map.! x) == Set.singleton y
    test "into (edge x) (edge y), x /= y: preds of y == {x}" $ \(x :: Int) ->
          let y = x + 1
              m = adjacencyMap (into (edge x) (edge y))
          in preds (m Map.! y) == Set.singleton x
    test "pits (edge x) (edge y), x /= y: forks of x == {x}" $ \(x :: Int) ->
          let y = x + 1
              m = adjacencyMap (pits (edge x) (edge y))
          in forks (m Map.! x) == Set.fromList [x, y]
    test "tips (edge x) (edge y), x /= y: joins of y == {y}" $ \(x :: Int) ->
          let y = x + 1
              m = adjacencyMap (tips (edge x) (edge y))
          in joins (m Map.! y) == Set.fromList [x, y]

    putStrLn "\n============ postset ============"
    test "postset x empty == Set.empty" $ \(x :: Int) ->
          postset x (empty :: AI) == Set.empty
    test "postset x (edge x) == Set.empty" $ \(x :: Int) ->
          postset x (edge x) == Set.empty
    test "postset 1 (into (edge 1) (edge 2)) == Set.singleton 2" $
          postset 1 (into (edge 1) (edge 2) :: AI) == Set.singleton 2

    putStrLn "\n============ preset ============"
    test "preset x empty == Set.empty" $ \(x :: Int) ->
          preset x (empty :: AI) == Set.empty
    test "preset x (edge x) == Set.empty" $ \(x :: Int) ->
          preset x (edge x) == Set.empty
    test "preset 2 (into (edge 1) (edge 2)) == Set.singleton 1" $
          preset 2 (into (edge 1) (edge 2) :: AI) == Set.singleton 1

    putStrLn "\n============ dfsForest ============"
    test "dfsForest empty == []" $
          dfsForest (empty :: AI) == []
    test "dfsForest (edge x) == [Node x []]" $ \(x :: Int) ->
          dfsForest (edge x) == [Node x []]
    test "dfsForest (path [1,2,3]) == [Node 1 [Node 2 [Node 3 []]]]" $
          dfsForest (path [1,2,3] :: AI) == [Node 1 [Node 2 [Node 3 []]]]
    test "isSubgraphOf (forest $ dfsForest x) x == True" $ sizeLimit $ \(x :: AI) ->
          isSubgraphOf (forest $ dfsForest x) x == True
    test "dfsForest . forest . dfsForest == dfsForest" $ sizeLimit $ \(x :: AI) ->
         (dfsForest . forest . dfsForest) x == dfsForest x

    putStrLn "\n============ topSort ============"
    test "topSort (edge x) == Just [x]" $ \(x :: Int) ->
          topSort (edge x :: AI) == Just [x]
    test "topSort (into (edge 1) (edge 2)) == Just [1,2]" $
          topSort (into (edge 1) (edge 2) :: AI) == Just [1,2]
    test "topSort (circuit [1,2]) == Nothing" $
          topSort (circuit [1,2] :: AI) == Nothing
    test "topSort (path [1,2,3]) == Nothing (transit node self-loop)" $
          topSort (path [1,2,3] :: AI) == Nothing
    test "fmap (flip isTopSort x) (topSort x) /= Just False" $ sizeLimit $ \(x :: AI) ->
          fmap (flip isTopSort x) (topSort x) /= Just False

    putStrLn "\n============ isTopSort ============"
    test "isTopSort [] empty == True" $
          isTopSort [] (empty :: AI) == True
    test "isTopSort [x] (edge x) == True" $ \(x :: Int) ->
          isTopSort [x] (edge x :: AI) == True
    test "isTopSort [] (edge x) == False" $ \(x :: Int) ->
          isTopSort [] (edge x :: AI) == False

    putStrLn "\n============ detachPit ============"
    test "detachPit x (edge x) == edge x" $ \(x :: Int) ->
          detachPit x (edge x) == (edge x :: AI)
    test "detachPit 2 (into (edge 1) (edge 2)) == edges [1, 2]" $
          detachPit 2 (into (edge 1) (edge 2) :: AI) == edges [1, 2]
    test "detachPit 1 (pits (edge 1) (edge 2)) == edges [1, 2]" $
          detachPit 1 (pits (edge 1) (edge 2) :: AI) == edges [1, 2]
    test "detachPit preserves edges" $ sizeLimit $ \(x :: AI) (a :: Int) ->
          edgeSet (detachPit a x) == edgeSet x
    test "detachPit consistent" $ sizeLimit $ \(x :: AI) (a :: Int) ->
          consistent (detachPit a x)

    putStrLn "\n============ detachTip ============"
    test "detachTip x (edge x) == edge x" $ \(x :: Int) ->
          detachTip x (edge x) == (edge x :: AI)
    test "detachTip 1 (into (edge 1) (edge 2)) == edges [1, 2]" $
          detachTip 1 (into (edge 1) (edge 2) :: AI) == edges [1, 2]
    test "detachTip 1 (tips (edge 1) (edge 2)) == edges [1, 2]" $
          detachTip 1 (tips (edge 1) (edge 2) :: AI) == edges [1, 2]
    test "detachTip preserves edges" $ sizeLimit $ \(x :: AI) (a :: Int) ->
          edgeSet (detachTip a x) == edgeSet x
    test "detachTip consistent" $ sizeLimit $ \(x :: AI) (a :: Int) ->
          consistent (detachTip a x)

    putStrLn "\n============ scc ============"
    test "scc empty == empty" $
          scc (empty :: AI) == empty
    test "scc (edge x) == edge (Set.singleton x)" $ \(x :: Int) ->
          scc (edge x) == edge (Set.singleton x)
    test "scc (circuit (1:xs)) == circuit [Set.fromList (1:xs)]" $
        sizeLimit $ \(xs :: [Int]) ->
          scc (circuit (1:xs) :: AI) == circuit [Set.fromList (1:xs)]
