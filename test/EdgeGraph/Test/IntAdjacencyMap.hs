{-# LANGUAGE ViewPatterns #-}
module EdgeGraph.Test.IntAdjacencyMap (testIntAdjacencyMap) where

import Data.Tree (Tree (..))

import EdgeGraph.IntAdjacencyMap
import EdgeGraph.IntAdjacencyMap.Internal
import EdgeGraph.Test
import qualified Data.IntSet     as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testIntAdjacencyMap :: IO ()
testIntAdjacencyMap = do
    putStrLn "\n============ IntAdjacencyMap ============"
    test "Axioms of edge graphs" $ sizeLimit $ (axioms :: GraphTestsuite IntAdjacencyMap)

    test "Consistency of arbitraryIntAdjacencyMap" $ sizeLimit $ \m ->
        consistent m

    putStrLn "\n============ empty ============"
    test "isEmpty     empty == True" $
          isEmpty     empty == True
    test "edgeCount empty == 0" $
          edgeCount empty == 0
    test "nodeCount   empty == 0" $
          nodeCount   empty == 0

    putStrLn "\n============ edge ============"
    test "isEmpty     (edge x) == False" $ \x ->
          isEmpty     (edge x) == False
    test "hasEdge x (edge x) == True" $ \x ->
          hasEdge x (edge x) == True
    test "edgeCount (edge x) == 1" $ \x ->
          edgeCount (edge x) == 1
    test "nodeCount   (edge x) == 2" $ \x ->
          nodeCount   (edge x) == 2

    putStrLn "\n============ overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \x y ->
          isEmpty     (overlay x y) == (isEmpty   x   && isEmpty   y)

    putStrLn "\n============ into ============"
    test "isEmpty     (into x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \x y ->
          isEmpty     (into x y) == (isEmpty   x   && isEmpty   y)

    putStrLn "\n============ isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ sizeLimit $ \x ->
          isSubgraphOf empty         x             == True
    test "isSubgraphOf x             (overlay x y) == True" $ sizeLimit $ \x y ->
          isSubgraphOf x             (overlay x y) == True

    putStrLn "\n============ edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount empty      == 0
    test "edgeCount (edge x)   == 1" $ \x ->
          edgeCount (edge x)   == 1

    putStrLn "\n============ nodeCount ============"
    test "nodeCount empty      == 0" $
          nodeCount empty      == 0
    test "nodeCount (edge x)   == 2" $ \x ->
          nodeCount (edge x)   == 2

    putStrLn "\n============ edgeIntSet ============"
    test "edgeIntSet empty      == IntSet.empty" $
          edgeIntSet empty      == IntSet.empty
    test "edgeIntSet . edge     == IntSet.singleton" $ \x ->
         (edgeIntSet . edge) x  == IntSet.singleton x

    putStrLn "\n============ path ============"
    test "path []    == empty" $
          path []    == empty
    test "path [x]   == edge x" $ \x ->
          path [x]   == edge x

    putStrLn "\n============ removeEdge ============"
    test "removeEdge x (edge x)              == empty" $ \x ->
          removeEdge x (edge x)              == empty

    putStrLn "\n============ gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f) ->
          gmap f empty      == empty
    test "gmap f (edge x)   == edge (f x)" $ \(apply -> f) x ->
          gmap f (edge x)   == edge (f x)
    test "gmap id           == id" $ sizeLimit $ \x ->
          gmap id x         == x

    putStrLn "\n============ induce ============"
    test "induce (const True)  x      == x" $ sizeLimit $ \x ->
          induce (const True)  x      == x
    test "induce (const False) x      == empty" $ sizeLimit $ \x ->
          induce (const False) x      == empty

    putStrLn "\n============ toIncidence/fromIncidence ============"
    test "fromIncidence (toIncidence m) == m" $ sizeLimit $ \m ->
          fromIncidence (toIncidence m) == m

    putStrLn "\n============ Adjacency record (edge) ============"
    test "edge x: forks == {x}" $ \x ->
          forks (adjacencyIntMap (edge x) Map.! x) == Set.singleton x
    test "edge x: joins == {x}" $ \x ->
          joins (adjacencyIntMap (edge x) Map.! x) == Set.singleton x
    test "edge x: preds == {}" $ \x ->
          preds (adjacencyIntMap (edge x) Map.! x) == Set.empty
    test "edge x: succs == {}" $ \x ->
          succs (adjacencyIntMap (edge x) Map.! x) == Set.empty

    putStrLn "\n============ Adjacency record (into) ============"
    test "into (edge x) (edge y), x /= y: succs of x == {y}" $ \x ->
          let y = x + 1
              m = adjacencyIntMap (into (edge x) (edge y))
          in succs (m Map.! x) == Set.singleton y
    test "into (edge x) (edge y), x /= y: preds of y == {x}" $ \x ->
          let y = x + 1
              m = adjacencyIntMap (into (edge x) (edge y))
          in preds (m Map.! y) == Set.singleton x
    test "into (edge x) (edge y), x /= y: forks of x == {x}" $ \x ->
          let y = x + 1
              m = adjacencyIntMap (into (edge x) (edge y))
          in forks (m Map.! x) == Set.singleton x
    test "into (edge x) (edge y), x /= y: joins of y == {y}" $ \x ->
          let y = x + 1
              m = adjacencyIntMap (into (edge x) (edge y))
          in joins (m Map.! y) == Set.singleton y

    putStrLn "\n============ detachPit ============"
    test "detachPit x (edge x) == edge x" $ \x ->
          detachPit x (edge x) == edge x
    test "detachPit 2 (into (edge 1) (edge 2)) == edges [1, 2]" $
          detachPit 2 (into (edge 1) (edge 2)) == edges [1, 2]
    test "detachPit 1 (pits (edge 1) (edge 2)) == edges [1, 2]" $
          detachPit 1 (pits (edge 1) (edge 2)) == edges [1, 2]
    test "detachPit preserves edges" $ sizeLimit $ \x a ->
          edgeIntSet (detachPit a x) == edgeIntSet x
    test "detachPit consistent" $ sizeLimit $ \x a ->
          consistent (detachPit a x)

    putStrLn "\n============ detachTip ============"
    test "detachTip x (edge x) == edge x" $ \x ->
          detachTip x (edge x) == edge x
    test "detachTip 1 (into (edge 1) (edge 2)) == edges [1, 2]" $
          detachTip 1 (into (edge 1) (edge 2)) == edges [1, 2]
    test "detachTip 1 (tips (edge 1) (edge 2)) == edges [1, 2]" $
          detachTip 1 (tips (edge 1) (edge 2)) == edges [1, 2]
    test "detachTip preserves edges" $ sizeLimit $ \x a ->
          edgeIntSet (detachTip a x) == edgeIntSet x
    test "detachTip consistent" $ sizeLimit $ \x a ->
          consistent (detachTip a x)

    putStrLn "\n============ postset ============"
    test "postset x empty == Set.empty" $ \x ->
          postset x empty == Set.empty
    test "postset x (edge x) == Set.empty" $ \x ->
          postset x (edge x) == Set.empty
    test "postset 1 (into (edge 1) (edge 2)) == Set.singleton 2" $
          postset 1 (into (edge 1) (edge 2)) == Set.singleton 2

    putStrLn "\n============ preset ============"
    test "preset x empty == Set.empty" $ \x ->
          preset x empty == Set.empty
    test "preset x (edge x) == Set.empty" $ \x ->
          preset x (edge x) == Set.empty
    test "preset 2 (into (edge 1) (edge 2)) == Set.singleton 1" $
          preset 2 (into (edge 1) (edge 2)) == Set.singleton 1

    putStrLn "\n============ dfsForest ============"
    test "dfsForest empty == []" $
          dfsForest empty == []
    test "dfsForest (edge x) == [Node x []]" $ \x ->
          dfsForest (edge x) == [Node x []]
    test "dfsForest (path [1,2,3]) == [Node 1 [Node 2 [Node 3 []]]]" $
          dfsForest (path [1,2,3]) == [Node 1 [Node 2 [Node 3 []]]]
    test "isSubgraphOf (forest $ dfsForest x) x == True" $ sizeLimit $ \x ->
          isSubgraphOf (forest $ dfsForest x) x == True
    test "dfsForest . forest . dfsForest == dfsForest" $ sizeLimit $ \x ->
         (dfsForest . forest . dfsForest) x == dfsForest x

    putStrLn "\n============ topSort ============"
    test "topSort (edge x) == Just [x]" $ \x ->
          topSort (edge x) == Just [x]
    test "topSort (into (edge 1) (edge 2)) == Just [1,2]" $
          topSort (into (edge 1) (edge 2)) == Just [1,2]
    test "topSort (circuit [1,2]) == Nothing" $
          topSort (circuit [1,2]) == Nothing
    test "topSort (path [1,2,3]) == Nothing (transit node self-loop)" $
          topSort (path [1,2,3]) == Nothing
    test "fmap (flip isTopSort x) (topSort x) /= Just False" $ sizeLimit $ \x ->
          fmap (flip isTopSort x) (topSort x) /= Just False

    putStrLn "\n============ isTopSort ============"
    test "isTopSort [] empty == True" $
          isTopSort [] empty == True
    test "isTopSort [x] (edge x) == True" $ \x ->
          isTopSort [x] (edge x) == True
    test "isTopSort [] (edge x) == False" $ \x ->
          isTopSort [] (edge x) == False
