{-# LANGUAGE ViewPatterns #-}
module EdgeGraph.Test.EdgeGraph (testGraph) where

import Data.Foldable
import EdgeGraph
import EdgeGraph.Test
import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet

type G  = EdgeGraph Int
type II = Int -> Int
type IG = Int -> G

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testGraph :: IO ()
testGraph = do
    putStrLn "\n============ EdgeGraph ============"
    test "Axioms of edge graphs"   $ sizeLimit $ (axioms   :: GraphTestsuite G)
    test "Theorems of edge graphs" $ sizeLimit $ (theorems :: GraphTestsuite G)

    putStrLn "\n============ empty ============"
    test "isEmpty     empty == True" $
          isEmpty    (empty :: G) == True
    test "edgeCount empty == 0" $
          edgeCount (empty :: G) == 0
    test "nodeCount   empty == 0" $
          nodeCount  (empty :: G) == 0
    test "size        empty == 1" $
          size       (empty :: G) == 1

    putStrLn "\n============ edge ============"
    test "isEmpty     (edge x) == False" $ \(x :: Int) ->
          isEmpty     (edge x) == False
    test "hasEdge x   (edge x) == True" $ \(x :: Int) ->
          hasEdge x   (edge x) == True
    test "hasEdge 1   (edge 2) == False" $
          hasEdge 1   (edge 2 :: G) == False
    test "edgeCount (edge x) == 1" $ \(x :: Int) ->
          edgeCount (edge x) == 1
    test "nodeCount   (edge x) == 2" $ \(x :: Int) ->
          nodeCount   (edge x) == 2
    test "size        (edge x) == 1" $ \(x :: Int) ->
          size        (edge x) == 1

    putStrLn "\n============ overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: G) y ->
          isEmpty     (overlay x y) ==(isEmpty   x   && isEmpty   y)
    test "edgeCount (overlay x y) >= edgeCount x" $ sizeLimit $ \(x :: G) y ->
          edgeCount (overlay x y) >= edgeCount x
    test "edgeCount (overlay x y) <= edgeCount x + edgeCount y" $ sizeLimit $ \(x :: G) y ->
          edgeCount (overlay x y) <= edgeCount x + edgeCount y
    test "size        (overlay x y) == size x + size y" $ sizeLimit $ \(x :: G) y ->
          size        (overlay x y) == size x        + size y

    putStrLn "\n============ into ============"
    test "isEmpty     (into x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: G) y ->
          isEmpty     (into x y) ==(isEmpty   x   && isEmpty   y)
    test "size        (into x y) == size x + size y" $ sizeLimit $ \(x :: G) y ->
          size        (into x y) == size x        + size y

    putStrLn "\n============ edges ============"
    test "edges []    == empty" $
          edges []    ==(empty :: G)
    test "edges [x]   == edge x" $ \(x :: Int) ->
          edges [x]   == edge x

    putStrLn "\n============ foldg ============"
    test "foldg empty edge overlay into pits tips == id" $ sizeLimit $ \(x :: G) ->
          foldg empty edge overlay into pits tips x == x
    test "foldg []    return (++) (++) (++) (++) == toList" $ sizeLimit $ \(x :: G) ->
          foldg [] return (++) (++) (++) (++) x == toList x
    test "foldg 0     (const 1) (+) (+) (+) (+) == length" $ sizeLimit $ \(x :: G) ->
          foldg 0 (const 1) (+) (+) (+) (+) x == length x
    test "foldg 1     (const 1) (+) (+) (+) (+) == size" $ sizeLimit $ \(x :: G) ->
          foldg 1 (const 1) (+) (+) (+) (+) x == size x
    test "foldg True  (const False) (&&) (&&) (&&) (&&) == isEmpty" $ sizeLimit $ \(x :: G) ->
          foldg True (const False) (&&) (&&) (&&) (&&) x == isEmpty x

    putStrLn "\n============ isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ sizeLimit $ \(x :: G) ->
          isSubgraphOf empty         x             == True
    test "isSubgraphOf x             (overlay x y) == True" $ sizeLimit $ \(x :: G) y ->
          isSubgraphOf x             (overlay x y) == True
    test "isSubgraphOf (overlay x y) (into x y)    == True" $ sizeLimit $ \(x :: G) y ->
          isSubgraphOf (overlay x y) (into x y)    == True

    putStrLn "\n============ (===) ============"
    test "    x === x         == True" $ sizeLimit $ \(x :: G) ->
             (x === x)        == True
    test "    x === x + empty == False" $ sizeLimit $ \(x :: G) ->
             (x === x + empty)== False
    test "x + y === x + y     == True" $ sizeLimit $ \(x :: G) y ->
         (x + y === x + y)    == True
    test "1 + 2 === 2 + 1     == False" $
         (1 + 2 === 2 + (1 :: G)) == False

    putStrLn "\n============ isEmpty ============"
    test "isEmpty empty                             == True" $
          isEmpty (empty :: G)                      == True
    test "isEmpty (overlay empty empty)             == True" $
          isEmpty (overlay empty empty :: G)        == True
    test "isEmpty (edge x)                          == False" $ \(x :: Int) ->
          isEmpty (edge x)                          == False
    test "isEmpty (removeEdge x $ edge x)      == True" $ \(x :: Int) ->
          isEmpty (removeEdge x $ edge x)      == True

    putStrLn "\n============ size ============"
    test "size empty         == 1" $
          size (empty :: G)  == 1
    test "size (edge x)      == 1" $ \(x :: Int) ->
          size (edge x)      == 1
    test "size (overlay x y) == size x + size y" $ sizeLimit $ \(x :: G) y ->
          size (overlay x y) == size x + size y
    test "size (into x y)    == size x + size y" $ sizeLimit $ \(x :: G) y ->
          size (into x y)    == size x + size y
    test "size x             >= 1" $ sizeLimit $ \(x :: G) ->
          size x             >= 1

    putStrLn "\n============ hasEdge ============"
    test "hasEdge x empty      == False" $ \(x :: Int) ->
          hasEdge x empty      == False
    test "hasEdge x (edge x)   == True" $ \(x :: Int) ->
          hasEdge x (edge x)   == True

    putStrLn "\n============ edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount (empty :: G) == 0
    test "edgeCount (edge x)   == 1" $ \(x :: Int) ->
          edgeCount (edge x)   == 1
    test "edgeCount            == length . edgeList" $ sizeLimit $ \(x :: G) ->
          edgeCount x          ==(length . edgeList) x

    putStrLn "\n============ edgeList ============"
    test "edgeList empty      == []" $
          edgeList (empty :: G) == []
    test "edgeList (edge x)   == [x]" $ \(x :: Int) ->
          edgeList (edge x)   == [x]
    test "edgeList . edges    == nub . sort" $ \(xs :: [Int]) ->
         (edgeList . edges) xs ==(nubOrd . sort) xs

    putStrLn "\n============ edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet (empty :: G) == Set.empty
    test "edgeSet . edge     == Set.singleton" $ \(x :: Int) ->
         (edgeSet . edge) x   == Set.singleton x
    test "edgeSet . edges    == Set.fromList" $ \(xs :: [Int]) ->
         (edgeSet . edges) xs == Set.fromList xs

    putStrLn "\n============ edgeIntSet ============"
    test "edgeIntSet empty      == IntSet.empty" $
          edgeIntSet (empty :: G) == IntSet.empty
    test "edgeIntSet . edge     == IntSet.singleton" $ \(x :: Int) ->
         (edgeIntSet . edge) x   == IntSet.singleton x

    putStrLn "\n============ path ============"
    test "path []    == empty" $
          path []    ==(empty :: G)
    test "path [x]   == edge x" $ \(x :: Int) ->
          path [x]   == edge x
    test "path [x,y] == into (edge x) (edge y)" $ \(x :: Int) y ->
          path [x,y] == into (edge x) (edge y)

    putStrLn "\n============ circuit ============"
    test "circuit []    == empty" $
          circuit []    ==(empty :: G)
    test "circuit [x]   == into (edge x) (edge x)" $ \(x :: Int) ->
          circuit [x]   == into (edge x) (edge x)

    putStrLn "\n============ clique ============"
    test "clique []      == empty" $
          clique []      ==(empty :: G)
    test "clique [x]     == edge x" $ \(x :: Int) ->
          clique [x]     == edge x
    test "clique [x,y]   == into (edge x) (edge y)" $ \(x :: Int) y ->
          clique [x,y]   == into (edge x) (edge y)

    putStrLn "\n============ removeEdge ============"
    test "removeEdge x (edge x)                   == empty" $ \(x :: Int) ->
          removeEdge x (edge x)                   == empty
    test "removeEdge x . removeEdge x        == removeEdge x" $ sizeLimit $ \x (y :: G) ->
         (removeEdge x . removeEdge x)y      == removeEdge x y

    putStrLn "\n============ replaceEdge ============"
    test "replaceEdge x x            == id" $ sizeLimit $ \x (y :: G) ->
          replaceEdge x x y          == y
    test "replaceEdge x y (edge x)   == edge y" $ \x (y :: Int) ->
          replaceEdge x y (edge x)   == edge y
    test "replaceEdge x y            == mergeEdges (== x) y" $ sizeLimit $ \x y z ->
          replaceEdge x y z          == mergeEdges (== x) y (z :: G)

    putStrLn "\n============ mergeEdges ============"
    test "mergeEdges (const False) x    == id" $ sizeLimit $ \x (y :: G) ->
          mergeEdges (const False) x y  == y
    test "mergeEdges (== x) y           == replaceEdge x y" $ sizeLimit $ \x y (z :: G) ->
          mergeEdges (== x) y z         == replaceEdge x y z

    putStrLn "\n============ splitEdge ============"
    test "splitEdge x []                == removeEdge x" $ sizeLimit $ \x (y :: G) ->
         (splitEdge x []) y             == removeEdge x y
    test "splitEdge x [x]               == id" $ sizeLimit $ \x (y :: G) ->
         (splitEdge x [x]) y            == y
    test "splitEdge x [y]               == replaceEdge x y" $ sizeLimit $ \x y (z :: G) ->
         (splitEdge x [y]) z            == replaceEdge x y z

    putStrLn "\n============ transpose ============"
    test "transpose empty       == empty" $
          transpose empty       ==(empty :: G)
    test "transpose (edge x)    == edge x" $ \(x :: Int) ->
          transpose (edge x)    == edge x
    test "transpose . transpose == id" $ sizeLimit $ \(x :: G) ->
         (transpose . transpose) x == x

    putStrLn "\n============ fmap ============"
    test "fmap f empty      == empty" $ \(apply -> f :: II) ->
          fmap f empty      == empty
    test "fmap f (edge x)   == edge (f x)" $ \(apply -> f :: II) x ->
          fmap f (edge x)   == edge (f x)
    test "fmap id           == id" $ sizeLimit $ \(x :: G) ->
          fmap id x         == x
    test "fmap f . fmap g   == fmap (f . g)" $ sizeLimit $ \(apply -> f :: II) (apply -> g :: II) (x :: G) ->
         (fmap f . fmap g) x== fmap (f . g) x

    putStrLn "\n============ >>= ============"
    test "empty >>= f          == empty" $ sizeLimit $ \(apply -> f :: IG) ->
         (empty >>= f)         == empty
    test "edge x >>= f         == f x" $ sizeLimit $ \(apply -> f :: IG) x ->
         (edge x >>= f)        == f x
    test "x >>= const empty    == empty" $ sizeLimit $ \(x :: G) ->
         (x >>= const empty)   ==(empty :: G)
    test "x >>= edge           == x" $ sizeLimit $ \(x :: G) ->
         (x >>= edge)          == x

    putStrLn "\n============ induce ============"
    test "induce (const True)  x      == x" $ sizeLimit $ \(x :: G) ->
          induce (const True)  x      == x
    test "induce (const False) x      == empty" $ sizeLimit $ \(x :: G) ->
          induce (const False) x      == empty
    test "induce (/= x)               == removeEdge x" $ sizeLimit $ \x (y :: G) ->
          induce (/= x) y             == removeEdge x y

    putStrLn "\n============ simplify ============"
    test "simplify x            == x" $ sizeLimit $ \(x :: G) ->
          simplify x            == x
    test "size (simplify x)     <= size x" $ sizeLimit $ \(x :: G) ->
          size (simplify x)     <= size x
