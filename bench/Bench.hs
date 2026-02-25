import Criterion.Main
import Data.Char
import Data.Foldable

import EdgeGraph.Class
import EdgeGraph.Fold (Fold, deBruijn, gmap, edgeIntSet, edgeSet,
                           nodeCount)

import qualified Data.IntSet as IntSet
import qualified Data.Set    as Set

-- | Count distinct edges in a Fold
el :: Ord a => Fold a -> Int
el = Set.size . edgeSet

-- | Count elements in the toList of a Fold
l :: Fold a -> Int
l = length . toList

-- | Count nodes in a Fold (via Incidence canonical form)
nR :: Ord a => Fold a -> Int
nR = nodeCount

-- | Count distinct edges in a Fold using IntSet
elInt :: Fold Int -> Int
elInt = IntSet.size . edgeIntSet

elDeBruijn :: Int -> Int
elDeBruijn n = el $ deBruijn n "0123456789"

lDeBruijn :: Int -> Int
lDeBruijn n = l $ deBruijn n "0123456789"

nRDeBruijn :: Int -> Int
nRDeBruijn n = nR $ deBruijn n "0123456789"

elIntDeBruijn :: Int -> Int
elIntDeBruijn n = el $ gmap fastRead $ deBruijn n "0123456789"

-- fastRead is ~3000x faster than read
fastRead :: String -> Int
fastRead = foldr (\c t -> t + ord c - ord '0') 0

fastReadInts :: Int -> Int
fastReadInts n = foldr (+) 0 $ map fastRead $ ints ++ ints
  where
    ints = mapM (const "0123456789") [1..n]

-- | Node count for clique as Incidence
nRClique :: Int -> Int
nRClique n = nR $ clique [1..n]

-- | Edge count for clique via IntSet
elIntClique :: Int -> Int
elIntClique n = elInt $ clique [1..n]

-- | Length of clique expression
lClique :: Int -> Int
lClique n = l $ clique [1..n]

main :: IO ()
main = defaultMain
    [ bgroup "elDeBruijn"
        [ bench "10^1" $ whnf elDeBruijn 1
        , bench "10^2" $ whnf elDeBruijn 2
        , bench "10^3" $ whnf elDeBruijn 3
        , bench "10^4" $ whnf elDeBruijn 4
        , bench "10^5" $ whnf elDeBruijn 5
        , bench "10^6" $ whnf elDeBruijn 6 ]
    , bgroup "lDeBruijn"
        [ bench "10^1" $ whnf lDeBruijn 1
        , bench "10^2" $ whnf lDeBruijn 2
        , bench "10^3" $ whnf lDeBruijn 3
        , bench "10^4" $ whnf lDeBruijn 4
        , bench "10^5" $ whnf lDeBruijn 5
        , bench "10^6" $ whnf lDeBruijn 6 ]
    , bgroup "nRDeBruijn"
        [ bench "10^1" $ whnf nRDeBruijn 1
        , bench "10^2" $ whnf nRDeBruijn 2
        , bench "10^3" $ whnf nRDeBruijn 3
        , bench "10^4" $ whnf nRDeBruijn 4
        , bench "10^5" $ whnf nRDeBruijn 5
        , bench "10^6" $ whnf nRDeBruijn 6 ]
    , bgroup "elIntDeBruijn"
        [ bench "10^1" $ whnf elIntDeBruijn 1
        , bench "10^2" $ whnf elIntDeBruijn 2
        , bench "10^3" $ whnf elIntDeBruijn 3
        , bench "10^4" $ whnf elIntDeBruijn 4
        , bench "10^5" $ whnf elIntDeBruijn 5
        , bench "10^6" $ whnf elIntDeBruijn 6 ]
    , bgroup "fastReadInts"
        [ bench "10^1" $ whnf fastReadInts 1
        , bench "10^2" $ whnf fastReadInts 2
        , bench "10^3" $ whnf fastReadInts 3
        , bench "10^4" $ whnf fastReadInts 4
        , bench "10^5" $ whnf fastReadInts 5
        , bench "10^6" $ whnf fastReadInts 6 ]
    , bgroup "nRClique"
        [ bench "10^0" $ nf nRClique 1
        , bench "10^1" $ nf nRClique 10
        , bench "10^2" $ nf nRClique 100
        , bench "10^3" $ nf nRClique 1000
        , bench "10^4" $ nf nRClique 10000 ]
    , bgroup "elIntClique"
        [ bench "10^0" $ nf elIntClique 1
        , bench "10^1" $ nf elIntClique 10
        , bench "10^2" $ nf elIntClique 100
        , bench "10^3" $ nf elIntClique 1000
        , bench "10^4" $ nf elIntClique 10000 ]
    , bgroup "lClique"
        [ bench "10^0" $ nf lClique 1
        , bench "10^1" $ nf lClique 10
        , bench "10^2" $ nf lClique 100
        , bench "10^3" $ nf lClique 1000
        , bench "10^4" $ nf lClique 10000 ] ]
