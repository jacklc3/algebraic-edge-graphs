{-# LANGUAGE ViewPatterns #-}
module EdgeGraph.Test.Incidence (testIncidence) where

import EdgeGraph.Incidence
import EdgeGraph.Incidence.Internal
import EdgeGraph.Test

import qualified Data.Set            as Set

type FI = Incidence Int
type II = Int -> Int

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testIncidence :: IO ()
testIncidence = do
    putStrLn "\n============ Incidence ============"
    test "Axioms of edge graphs" $ sizeLimit $ (axioms :: GraphTestsuite FI)

    test "Consistency of arbitraryIncidence" $ sizeLimit $ \(m :: FI) ->
        consistent m

    putStrLn "\n============ Show ============"
    test "show (empty     :: Incidence Int) == \"empty\"" $
          show (empty     :: Incidence Int) == "empty"
    test "show (1         :: Incidence Int) == \"edge 1\"" $
          show (1         :: Incidence Int) == "edge 1"
    test "show (1 + 2     :: Incidence Int) == \"edges [1,2]\"" $
          show (1 + 2     :: Incidence Int) == "edges [1,2]"

    putStrLn "\n============ empty ============"
    test "isEmpty     empty == True" $
          isEmpty    (empty :: FI) == True
    test "edgeCount empty == 0" $
          edgeCount(empty :: FI) == 0
    test "nodeCount   empty == 0" $
          nodeCount  (empty :: FI) == 0

    putStrLn "\n============ edge ============"
    test "isEmpty     (edge x) == False" $ \(x :: Int) ->
          isEmpty     (edge x) == False
    test "hasEdge x (edge x) == True" $ \(x :: Int) ->
          hasEdge x (edge x) == True
    test "hasEdge 1 (edge 2) == False" $
          hasEdge 1 (edge 2 :: FI) == False
    test "edgeCount (edge x) == 1" $ \(x :: Int) ->
          edgeCount (edge x) == 1
    test "nodeCount   (edge x) == 2" $ \(x :: Int) ->
          nodeCount   (edge x) == 2

    putStrLn "\n============ overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: FI) y ->
          isEmpty     (overlay x y) == (isEmpty   x   && isEmpty   y)
    test "edgeCount (overlay x y) >= edgeCount x" $ sizeLimit $ \(x :: FI) y ->
          edgeCount (overlay x y) >= edgeCount x
    test "edgeCount (overlay x y) <= edgeCount x + edgeCount y" $ sizeLimit $ \(x :: FI) y ->
          edgeCount (overlay x y) <= edgeCount x + edgeCount y
    test "nodeCount   (overlay x y) <= nodeCount x + nodeCount y" $ sizeLimit $ \(x :: FI) y ->
          nodeCount   (overlay x y) <= nodeCount x + nodeCount y

    putStrLn "\n============ into ============"
    test "isEmpty     (into x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: FI) y ->
          isEmpty     (into x y) == (isEmpty   x   && isEmpty   y)

    putStrLn "\n============ edges ============"
    test "edges []    == empty" $
          edges []    == (empty :: FI)
    test "edges [x]   == edge x" $ \(x :: Int) ->
          edges [x]   == (edge x :: FI)

    putStrLn "\n============ isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ sizeLimit $ \(x :: FI) ->
          isSubgraphOf empty         x             == True
    test "isSubgraphOf x             (overlay x y) == True" $ sizeLimit $ \(x :: FI) y ->
          isSubgraphOf x             (overlay x y) == True
    test "isSubgraphOf (overlay x y) (into x y)    == True" $ sizeLimit $ \(x :: FI) y ->
          isSubgraphOf (overlay x y) (into x y)    == True

    putStrLn "\n============ isEmpty ============"
    test "isEmpty empty                                     == True" $
          isEmpty (empty :: FI)                              == True
    test "isEmpty (overlay empty empty)                     == True" $
          isEmpty (overlay empty empty :: FI)                == True
    test "isEmpty (edge x)                                  == False" $ \(x :: Int) ->
          isEmpty (edge x)                                  == False
    test "isEmpty (removeEdge x $ edge x)              == True" $ \(x :: Int) ->
          isEmpty (removeEdge x $ edge x)              == True

    putStrLn "\n============ hasEdge ============"
    test "hasEdge x empty            == False" $ \(x :: Int) ->
          hasEdge x empty            == False
    test "hasEdge x (edge x)         == True" $ \(x :: Int) ->
          hasEdge x (edge x)         == True

    putStrLn "\n============ edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount (empty :: FI) == 0
    test "edgeCount (edge x)   == 1" $ \(x :: Int) ->
          edgeCount (edge x)   == 1
    test "edgeCount            == length . edgeList" $ sizeLimit $ \(x :: FI) ->
          edgeCount x          == (length . edgeList) x

    putStrLn "\n============ nodeCount ============"
    test "nodeCount empty      == 0" $
          nodeCount (empty :: FI) == 0
    test "nodeCount (edge x)   == 2" $ \(x :: Int) ->
          nodeCount (edge x)   == 2
    test "nodeCount            == length . nodeList" $ sizeLimit $ \(x :: FI) ->
          nodeCount x          == (length . nodeList) x

    putStrLn "\n============ edgeList ============"
    test "edgeList empty      == []" $
          edgeList (empty :: FI) == []
    test "edgeList (edge x)   == [x]" $ \(x :: Int) ->
          edgeList (edge x)   == [x]

    putStrLn "\n============ edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet (empty :: FI) == Set.empty
    test "edgeSet . edge     == Set.singleton" $ \(x :: Int) ->
         (edgeSet . edge) x   == Set.singleton x

    putStrLn "\n============ path ============"
    test "path []    == empty" $
          path []    == (empty :: FI)
    test "path [x]   == edge x" $ \(x :: Int) ->
          path [x]   == (edge x :: FI)
    test "path [x,y] == into (edge x) (edge y)" $ \(x :: Int) y ->
          path [x,y] == (into (edge x) (edge y) :: FI)

    putStrLn "\n============ removeEdge ============"
    test "removeEdge x (edge x)                   == empty" $ \(x :: Int) ->
          removeEdge x (edge x)                   == (empty :: FI)
    test "removeEdge x . removeEdge x        == removeEdge x" $ sizeLimit $ \x (y :: FI) ->
         (removeEdge x . removeEdge x)y      ==(removeEdge x y :: FI)

    putStrLn "\n============ gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f :: II) ->
          gmap f empty      == empty
    test "gmap f (edge x)   == edge (f x)" $ \(apply -> f :: II) x ->
          gmap f (edge x)   == edge (f x)
    test "gmap id           == id" $ sizeLimit $ \x ->
          gmap id x         == (x :: FI)
    test "gmap f . gmap g   == gmap (f . g)" $ sizeLimit $ \(apply -> f :: II) (apply -> g :: II) x ->
         (gmap f . gmap g) x== gmap (f . g) x

    putStrLn "\n============ induce ============"
    test "induce (const True)  x      == x" $ sizeLimit $ \(x :: FI) ->
          induce (const True)  x      == x
    test "induce (const False) x      == empty" $ sizeLimit $ \(x :: FI) ->
          induce (const False) x      == (empty :: FI)
    test "induce (/= x)               == removeEdge x" $ sizeLimit $ \x (y :: FI) ->
          induce (/= x) y             == (removeEdge x y :: FI)

