{-# OPTIONS_GHC -fno-warn-orphans #-}
module EdgeGraph.Test.Arbitrary (
    arbitraryGraph, arbitraryIncidence, arbitraryAdjacencyMap, arbitraryIntAdjacencyMap
  ) where

import Test.QuickCheck

import EdgeGraph
import EdgeGraph.AdjacencyMap.Internal (AdjacencyMap (..))
import EdgeGraph.Fold (Fold)
import EdgeGraph.IntAdjacencyMap.Internal (IntAdjacencyMap (..))
import EdgeGraph.Incidence.Internal (Incidence)
import qualified EdgeGraph.Class                    as C

-- | Generate an arbitrary 'EdgeGraph' value of a specified size.
arbitraryGraph :: (C.EdgeGraph g, Arbitrary (C.Edge g)) => Gen g
arbitraryGraph = sized expr
  where
    expr 0 = return C.empty
    expr 1 = C.edge <$> arbitrary
    expr n = do
        left <- choose (0, n)
        oneof [ C.overlay <$> (expr left) <*> (expr $ n - left)
              , C.into    <$> (expr left) <*> (expr $ n - left)
              , C.pits    <$> (expr left) <*> (expr $ n - left)
              , C.tips    <$> (expr left) <*> (expr $ n - left) ]

instance Arbitrary a => Arbitrary (EdgeGraph a) where
    arbitrary = arbitraryGraph

    shrink Empty       = []
    shrink (Edge _)    = [Empty]
    shrink (x :++: y)  = [Empty, x, y]
                       ++ [x' :++: y' | (x', y') <- shrink (x, y) ]
    shrink (x :>>: y)  = [Empty, x, y, x :++: y]
                       ++ [x' :>>: y' | (x', y') <- shrink (x, y) ]
    shrink (x :<>: y)  = [Empty, x, y, x :++: y]
                       ++ [x' :<>: y' | (x', y') <- shrink (x, y) ]
    shrink (x :><: y)  = [Empty, x, y, x :++: y]
                       ++ [x' :><: y' | (x', y') <- shrink (x, y) ]

-- | Generate an arbitrary 'Incidence' by building a random algebraic expression.
-- This guarantees the result is a valid flow representation.
arbitraryIncidence :: (Arbitrary a, Ord a) => Gen (Incidence a)
arbitraryIncidence = arbitraryGraph

-- | Generate an arbitrary 'AdjacencyMap'.
arbitraryAdjacencyMap :: (Arbitrary a, Ord a) => Gen (AdjacencyMap a)
arbitraryAdjacencyMap = arbitraryGraph

-- | Generate an arbitrary 'IntAdjacencyMap'.
arbitraryIntAdjacencyMap :: Gen IntAdjacencyMap
arbitraryIntAdjacencyMap = arbitraryGraph

instance (Arbitrary a, Ord a) => Arbitrary (Incidence a) where
    arbitrary = arbitraryGraph

instance (Arbitrary a, Ord a) => Arbitrary (AdjacencyMap a) where
    arbitrary = arbitraryAdjacencyMap

instance Arbitrary IntAdjacencyMap where
    arbitrary = arbitraryIntAdjacencyMap

instance Arbitrary a => Arbitrary (Fold a) where
    arbitrary = arbitraryGraph
