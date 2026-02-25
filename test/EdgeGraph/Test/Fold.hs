{-# LANGUAGE ViewPatterns #-}
module EdgeGraph.Test.Fold (testFold) where

import Data.Foldable
import EdgeGraph.Fold
import EdgeGraph.Test
import qualified EdgeGraph.Class as C
import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet

type F  = Fold Int
type II = Int -> Int
type IF = Int -> F

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testFold :: IO ()
testFold = do
    putStrLn "\n============ Fold ============"
    test "Axioms of edge graphs"   $ sizeLimit $ (axioms :: GraphTestsuite F)

    putStrLn "\n============ Show ============"
    test "show (empty :: Fold Int) == \"empty\"" $
          show (empty :: Fold Int) == "empty"
    test "show (1     :: Fold Int) == \"edge 1\"" $
          show (1     :: Fold Int) == "edge 1"

    putStrLn "\n============ empty ============"
    test "isEmpty     empty == True" $
          isEmpty    (empty :: F) == True
    test "edgeCount empty == 0" $
          edgeCount(empty :: F) == 0
    test "nodeCount   empty == 0" $
          nodeCount  (empty :: F) == 0
    test "size        empty == 1" $
          size       (empty :: F) == 1

    putStrLn "\n============ edge ============"
    test "isEmpty     (edge x) == False" $ \(x :: Int) ->
          isEmpty     (edge x) == False
    test "hasEdge x   (edge x) == True" $ \(x :: Int) ->
          hasEdge x   (edge x) == True
    test "edgeCount (edge x) == 1" $ \(x :: Int) ->
          edgeCount (edge x) == 1
    test "nodeCount   (edge x) == 2" $ \(x :: Int) ->
          nodeCount   (edge x) == 2
    test "size        (edge x) == 1" $ \(x :: Int) ->
          size        (edge x) == 1

    putStrLn "\n============ overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: F) y ->
          isEmpty     (overlay x y) ==(isEmpty   x   && isEmpty   y)
    test "size        (overlay x y) == size x + size y" $ sizeLimit $ \(x :: F) y ->
          size        (overlay x y) == size x + size y

    putStrLn "\n============ into ============"
    test "isEmpty     (into x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: F) y ->
          isEmpty     (into x y) ==(isEmpty   x   && isEmpty   y)
    test "size        (into x y) == size x + size y" $ sizeLimit $ \(x :: F) y ->
          size        (into x y) == size x + size y

    putStrLn "\n============ edges ============"
    test "edges []    == empty" $
          edges []    ==(empty :: F)
    test "edges [x]   == edge x" $ \(x :: Int) ->
          edges [x]   == (edge x :: F)

    putStrLn "\n============ foldg ============"
    test "foldg empty edge overlay into pits tips == id" $ sizeLimit $ \(x :: F) ->
          foldg empty edge overlay into pits tips x == x
    test "foldg [] return (++) (++) (++) (++) == toList" $ sizeLimit $ \(x :: F) ->
          foldg [] return (++) (++) (++) (++) x == toList x
    test "foldg 1 (const 1) (+) (+) (+) (+) == size" $ sizeLimit $ \(x :: F) ->
          foldg 1 (const 1) (+) (+) (+) (+) x == size x
    test "foldg True (const False) (&&) (&&) (&&) (&&) == isEmpty" $ sizeLimit $ \(x :: F) ->
          foldg True (const False) (&&) (&&) (&&) (&&) x == isEmpty x

    putStrLn "\n============ isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ sizeLimit $ \(x :: F) ->
          C.isSubgraphOf empty       x             == True
    test "isSubgraphOf x             (overlay x y) == True" $ sizeLimit $ \(x :: F) y ->
          C.isSubgraphOf x           (overlay x y) == True

    putStrLn "\n============ isEmpty ============"
    test "isEmpty empty                             == True" $
          isEmpty (empty :: F)                      == True
    test "isEmpty (overlay empty empty)             == True" $
          isEmpty (overlay empty empty :: F)        == True
    test "isEmpty (edge x)                          == False" $ \(x :: Int) ->
          isEmpty (edge x)                          == False
    test "isEmpty (removeEdge x $ edge x)      == True" $ \(x :: Int) ->
          isEmpty (removeEdge x $ edge x)      == True

    putStrLn "\n============ size ============"
    test "size empty         == 1" $
          size (empty :: F)  == 1
    test "size (edge x)      == 1" $ \(x :: Int) ->
          size (edge x)      == 1
    test "size x             >= 1" $ sizeLimit $ \(x :: F) ->
          size x             >= 1

    putStrLn "\n============ hasEdge ============"
    test "hasEdge x empty      == False" $ \(x :: Int) ->
          hasEdge x empty      == False
    test "hasEdge x (edge x)   == True" $ \(x :: Int) ->
          hasEdge x (edge x)   == True

    putStrLn "\n============ edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount (empty :: F) == 0
    test "edgeCount (edge x)   == 1" $ \(x :: Int) ->
          edgeCount (edge x)   == 1
    test "edgeCount            == length . edgeList" $ sizeLimit $ \(x :: F) ->
          edgeCount x          ==(length . edgeList) x

    putStrLn "\n============ edgeList ============"
    test "edgeList empty      == []" $
          edgeList (empty :: F) == []
    test "edgeList (edge x)   == [x]" $ \(x :: Int) ->
          edgeList (edge x)   == [x]

    putStrLn "\n============ edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet (empty :: F) == Set.empty
    test "edgeSet . edge     == Set.singleton" $ \(x :: Int) ->
         (edgeSet . edge) x   == Set.singleton x

    putStrLn "\n============ edgeIntSet ============"
    test "edgeIntSet empty      == IntSet.empty" $
          edgeIntSet (empty :: F) == IntSet.empty
    test "edgeIntSet . edge     == IntSet.singleton" $ \(x :: Int) ->
         (edgeIntSet . edge) x   == IntSet.singleton x

    putStrLn "\n============ path ============"
    test "path []    == empty" $
          path []    == (empty :: F)
    test "path [x]   == edge x" $ \(x :: Int) ->
          path [x]   == (edge x :: F)
    test "path [x,y] == into (edge x) (edge y)" $ \(x :: Int) y ->
          path [x,y] == (into (edge x) (edge y) :: F)

    putStrLn "\n============ removeEdge ============"
    test "removeEdge x (edge x)              == empty" $ \(x :: Int) ->
          removeEdge x (edge x)              == (empty :: F)
    test "removeEdge x . removeEdge x   == removeEdge x" $ sizeLimit $ \x (y :: F) ->
         (removeEdge x . removeEdge x)y ==(removeEdge x y :: F)

    putStrLn "\n============ replaceEdge ============"
    test "replaceEdge x x            == id" $ sizeLimit $ \x (y :: F) ->
          replaceEdge x x y          == y
    test "replaceEdge x y (edge x)   == edge y" $ \x (y :: Int) ->
          replaceEdge x y (edge x)   == (edge y :: F)

    putStrLn "\n============ transpose ============"
    test "transpose empty       == empty" $
          transpose empty       == (empty :: F)
    test "transpose (edge x)    == edge x" $ \(x :: Int) ->
          transpose (edge x)    == (edge x :: F)
    test "transpose . transpose == id" $ sizeLimit $ \(x :: F) ->
         (transpose . transpose) x == x

    putStrLn "\n============ gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f :: II) ->
          gmap f empty      == (empty :: F)
    test "gmap f (edge x)   == edge (f x)" $ \(apply -> f :: II) x ->
          gmap f (edge x)   == (edge (f x) :: F)
    test "gmap id           == id" $ sizeLimit $ \(x :: F) ->
          gmap id x         == x
    test "gmap f . gmap g   == gmap (f . g)" $ sizeLimit $ \(apply -> f :: II) (apply -> g :: II) (x :: F) ->
         (gmap f . gmap g) x== (gmap (f . g) x :: F)

    putStrLn "\n============ bind ============"
    test "bind empty f         == empty" $ sizeLimit $ \(apply -> f :: IF) ->
          bind empty f         == empty
    test "bind (edge x) f      == f x" $ sizeLimit $ \(apply -> f :: IF) x ->
          bind (edge x) f      == f x
    test "bind x (const empty) == empty" $ sizeLimit $ \(x :: F) ->
          bind x (const empty) == (empty :: F)
    test "bind x edge          == x" $ sizeLimit $ \(x :: F) ->
          bind x edge          == x

    putStrLn "\n============ induce ============"
    test "induce (const True)  x      == x" $ sizeLimit $ \(x :: F) ->
          induce (const True)  x      == x
    test "induce (const False) x      == empty" $ sizeLimit $ \(x :: F) ->
          induce (const False) x      == (empty :: F)
    test "induce (/= x)               == removeEdge x" $ sizeLimit $ \x (y :: F) ->
          induce (/= x) y             == (removeEdge x y :: F)

    putStrLn "\n============ simplify ============"
    test "simplify x            == x" $ sizeLimit $ \(x :: F) ->
          simplify x            == x
    test "size (simplify x)     <= size x" $ sizeLimit $ \(x :: F) ->
          size (simplify x)     <= size x
