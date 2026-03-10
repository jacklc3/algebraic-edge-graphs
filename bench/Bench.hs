import Criterion.Main
import Data.Foldable

import EdgeGraph.Class
import EdgeGraph.Fold (Fold, deBruijn, gmap, edgeIntSet, edgeSet, nodeCount)

import qualified Data.IntSet as IntSet
import qualified Data.Set    as Set

digToInt :: [Int] -> Int
digToInt = foldl (\acc x -> acc * 10 + x) 0

-- | Count distinct edges in a Fold
el :: Ord a => Fold a -> Int
el = Set.size . edgeSet

-- | Count elements in the toList of a Fold
l :: Fold a -> Int
l = length . toList

-- | Count nodes in a Fold (via Incidence canonical form)
nc :: Ord a => Fold a -> Int
nc = nodeCount

-- | Count distinct edges in a Fold using IntSet
elInt :: Fold Int -> Int
elInt = IntSet.size . edgeIntSet

elDeBruijn :: Int -> Int
elDeBruijn n = el $ deBruijn n [0..9 :: Int]

lDeBruijn :: Int -> Int
lDeBruijn n = l $ deBruijn n [0..9 :: Int]

ncDeBruijn :: Int -> Int
ncDeBruijn n = nc $ deBruijn n [0..9 :: Int]

elIntDeBruijn :: Int -> Int
elIntDeBruijn n = elInt $ gmap digToInt $ deBruijn n [0..9 :: Int]

elClique :: Int -> Int
elClique n = el $ clique [1..n]

lClique :: Int -> Int
lClique n = l $ clique [1..n]

ncClique :: Int -> Int
ncClique n = nc $ clique [1..n]

elIntClique :: Int -> Int
elIntClique n = elInt $ clique [1..n]

main :: IO ()
main = defaultMain
  [ bgroup "elDeBruijn"
    [ bench "10^0" $ whnf elDeBruijn 0
    , bench "10^1" $ whnf elDeBruijn 1
    , bench "10^2" $ whnf elDeBruijn 2
    , bench "10^3" $ whnf elDeBruijn 3
    , bench "10^4" $ whnf elDeBruijn 4 ]
  , bgroup "lDeBruijn"
    [ bench "10^0" $ whnf lDeBruijn 0
    , bench "10^1" $ whnf lDeBruijn 1
    , bench "10^2" $ whnf lDeBruijn 2
    , bench "10^3" $ whnf lDeBruijn 3
    , bench "10^4" $ whnf lDeBruijn 4 ]
  , bgroup "ncDeBruijn"
    [ bench "10^0" $ whnf ncDeBruijn 0
    , bench "10^1" $ whnf ncDeBruijn 1
    , bench "10^2" $ whnf ncDeBruijn 2
    , bench "10^3" $ whnf ncDeBruijn 3
    , bench "10^4" $ whnf ncDeBruijn 4 ]
  , bgroup "elIntDeBruijn"
    [ bench "10^0" $ whnf elIntDeBruijn 0
    , bench "10^1" $ whnf elIntDeBruijn 1
    , bench "10^2" $ whnf elIntDeBruijn 2
    , bench "10^3" $ whnf elIntDeBruijn 3
    , bench "10^4" $ whnf elIntDeBruijn 4 ]
  , bgroup "elClique"
    [ bench "10^0" $ nf elClique 1
    , bench "10^1" $ nf elClique 10
    , bench "10^2" $ nf elClique 100
    , bench "10^3" $ nf elClique 1000]
  , bgroup "ncClique"
    [ bench "10^0" $ nf ncClique 1
    , bench "10^1" $ nf ncClique 10
    , bench "10^2" $ nf ncClique 100
    , bench "10^3" $ nf ncClique 1000]
  , bgroup "lClique"
    [ bench "10^0" $ nf lClique 1
    , bench "10^1" $ nf lClique 10
    , bench "10^2" $ nf lClique 100
    , bench "10^3" $ nf lClique 1000]
  , bgroup "elIntClique"
    [ bench "10^0" $ nf elIntClique 1
    , bench "10^1" $ nf elIntClique 10
    , bench "10^2" $ nf elIntClique 100
    , bench "10^3" $ nf elIntClique 1000] ]
