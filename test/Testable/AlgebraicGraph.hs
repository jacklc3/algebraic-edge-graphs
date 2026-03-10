{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, ViewPatterns #-}

module Testable.AlgebraicGraph (
  -- * TestableAlgebraicGraph class (EdgeGraph, Fold)
  TestableAlgebraicGraph (..),

  -- * Shared test groups
  testSizeGroup, testFoldgGroup, testMergeEdgesGroup,
  testSplitEdgeGroup, testTransposeGroup, testSimplifyGroup,
  testBindGroup,
) where

import Test.QuickCheck.Function

import EdgeGraph.Class
import Testable.Graph (TestableGraph(..), test, sizeLimit)

-- ---------------------------------------------------------------------------
-- TestableAlgebraicGraph class (EdgeGraph, Fold)
-- ---------------------------------------------------------------------------

-- | Type class for deep-embedding graph types that support algebraic
-- operations like folding, transposing, and binding.
class TestableGraph g => TestableAlgebraicGraph g where
  size       :: g -> Int
  foldg      :: b -> (Edge g -> b) -> (b -> b -> b) -> (b -> b -> b)
             -> (b -> b -> b) -> (b -> b -> b) -> g -> b
  mergeEdges :: (Edge g -> Bool) -> Edge g -> g -> g
  splitEdge  :: Edge g -> [Edge g] -> g -> g
  transpose  :: g -> g
  simplify   :: g -> g
  bind       :: g -> (Edge g -> g) -> g
  toEdgeList :: g -> [Edge g]

-- ---------------------------------------------------------------------------
-- Shared test groups (TestableAlgebraicGraph)
-- ---------------------------------------------------------------------------

testSizeGroup :: forall g. TestableAlgebraicGraph g => IO ()
testSizeGroup = do
  putStrLn "\n============ size ============"
  test "size empty         == 1" $
        size (empty :: g)  == 1
  test "size (edge x)      == 1" $ \(x :: Edge g) ->
        size (edge x :: g) == 1
  test "size (overlay x y) == size x + size y" $ sizeLimit $ \(x :: g) y ->
        size (overlay x y) == size x + size y
  test "size (into x y)    == size x + size y" $ sizeLimit $ \(x :: g) y ->
        size (into x y)    == size x + size y
  test "size x             >= 1" $ sizeLimit $ \(x :: g) ->
        size x             >= 1

testFoldgGroup :: forall g. TestableAlgebraicGraph g => IO ()
testFoldgGroup = do
  putStrLn "\n============ foldg ============"
  test "foldg empty edge overlay into pits tips       == id" $ sizeLimit $ \(x :: g) ->
        foldg empty edge overlay into pits tips x == x
  test "foldg [] return (++) (++) (++) (++)           == toList" $ sizeLimit $ \(x :: g) ->
        foldg [] return (++) (++) (++) (++) x == toEdgeList x
  test "foldg 1 (const 1) (+) (+) (+) (+)             == size" $ sizeLimit $ \(x :: g) ->
        foldg 1 (const 1) (+) (+) (+) (+) x == size x
  test "foldg True  (const False) (&&) (&&) (&&) (&&) == isEmpty" $ sizeLimit $ \(x :: g) ->
        foldg True (const False) (&&) (&&) (&&) (&&) x == isEmpty x

testMergeEdgesGroup :: forall g. TestableAlgebraicGraph g => IO ()
testMergeEdgesGroup = do
  putStrLn "\n============ mergeEdges ============"
  test "mergeEdges (const False) x   == id" $ sizeLimit $ \(x :: Edge g) (y :: g) ->
        mergeEdges (const False) x y == y
  test "mergeEdges (== x) y          == replaceEdge x y" $ sizeLimit $ \x y (z :: g) ->
        mergeEdges (== x) y z        == replaceEdge x y z

testSplitEdgeGroup :: forall g. TestableAlgebraicGraph g => IO ()
testSplitEdgeGroup = do
  putStrLn "\n============ splitEdge ============"
  test "splitEdge x []     == removeEdge x" $ sizeLimit $ \(x :: Edge g) (y :: g) ->
       (splitEdge x []) y  == removeEdge x y
  test "splitEdge x [x]    == id" $ sizeLimit $ \(x :: Edge g) (y :: g) ->
       (splitEdge x [x]) y == y
  test "splitEdge x [y]    == replaceEdge x y" $ sizeLimit $ \x y (z :: g) ->
       (splitEdge x [y]) z == replaceEdge x y z

testTransposeGroup :: forall g. TestableAlgebraicGraph g => IO ()
testTransposeGroup = do
  putStrLn "\n============ transpose ============"
  test "transpose empty       == empty" $
        transpose (empty :: g) == (empty :: g)
  test "transpose (edge x)    == edge x" $ \(x :: Edge g) ->
        transpose (edge x :: g) == (edge x :: g)
  test "transpose . transpose == id" $ sizeLimit $ \(x :: g) ->
       (transpose . transpose) x == x

testSimplifyGroup :: forall g. TestableAlgebraicGraph g => IO ()
testSimplifyGroup = do
  putStrLn "\n============ simplify ============"
  test "simplify x        == x" $ sizeLimit $ \(x :: g) ->
        simplify x        == x
  test "size (simplify x) <= size x" $ sizeLimit $ \(x :: g) ->
        size x            >= size (simplify x)

testBindGroup :: forall g. TestableAlgebraicGraph g => IO ()
testBindGroup = do
  putStrLn "\n============ bind ============"
  test "bind empty f         == empty" $ sizeLimit $ \(apply -> f :: Edge g -> g) ->
        bind (empty :: g) f  == (empty :: g)
  test "bind (edge x) f      == f x" $ sizeLimit $ \(apply -> f :: Edge g -> g) x ->
        bind (edge x :: g) f == f x
  test "bind x (const empty) == empty" $ sizeLimit $ \(x :: g) ->
        bind x (const empty) == (empty :: g)
  test "bind x edge          == x" $ sizeLimit $ \(x :: g) ->
        bind x edge          == x
