{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables, TypeApplications, ViewPatterns #-}
module Testable.Graph (
    -- * Test infrastructure
    test, sizeLimit,

    -- * TestableGraph class
    TestableGraph (..),

    -- * Shared test groups
    testAxiomsGroup,
    testEmptyGroup, testEdgeGroup, testOverlayGroup, testIntoGroup,
    testEdgesGroup, testIsSubgraphOfGroup, testIsEmptyGroup,
    testHasEdgeGroup, testEdgeCountGroup, testNodeCountGroup,
    testEdgeListGroup, testEdgeSetGroup, testEdgeIntSetGroup,
    testPathGroup, testCircuitGroup, testCliqueGroup,
    testBicliqueGroup, testFlowerGroup, testNodeGroup,
    testRemoveEdgeGroup, testReplaceEdgeGroup,
    testGmapGroup, testInduceGroup,
    ) where

import Prelude hiding ((<=))
import Data.List.Extra (nubOrd)
import Data.List (sort)
import System.Exit (exitFailure)
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Function

import EdgeGraph.Class
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- Test infrastructure
-- ---------------------------------------------------------------------------

test :: Testable a => String -> a -> IO ()
test str p = do
    result <- quickCheckWithResult (stdArgs { chatty = False }) p
    if isSuccess result
        then putStrLn $ "OK: " ++ str
        else do
            putStrLn $ "\nTest failure:\n    " ++ str ++ "\n"
            putStrLn $ output result
            exitFailure

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

-- ---------------------------------------------------------------------------
-- TestableGraph class
-- ---------------------------------------------------------------------------

-- | Type class for graph types that support common query and transformation
-- operations needed by the shared test groups.
class (Eq g, Show g, Arbitrary g, EdgeGraph g,
       Eq (Edge g), Ord (Edge g), Show (Edge g), Num (Edge g), Integral (Edge g),
       Arbitrary (Edge g), CoArbitrary (Edge g), Function (Edge g))
    => TestableGraph g where
    isEmpty      :: g -> Bool
    hasEdge      :: Edge g -> g -> Bool
    edgeCount    :: g -> Int
    nodeCount    :: g -> Int
    edgeList     :: g -> [Edge g]
    edgeSet      :: g -> Set.Set (Edge g)
    edgeIntSet   :: g -> IntSet.IntSet
    removeEdge   :: Edge g -> g -> g
    replaceEdge  :: Edge g -> Edge g -> g -> g
    gmap         :: (Edge g -> Edge g) -> g -> g
    induce       :: (Edge g -> Bool) -> g -> g

-- ---------------------------------------------------------------------------
-- Shared test groups
-- ---------------------------------------------------------------------------

testAxiomsGroup :: forall g. TestableGraph g => IO ()
testAxiomsGroup = do
    test "Axioms of edge graphs"      $ sizeLimit $ (axioms     :: GraphTestsuite g)
    test "Edge axioms of edge graphs"  $ sizeLimit $ (edgeAxioms @g)
    test "Theorems of edge graphs"     $ sizeLimit $ (theorems   :: GraphTestsuite g)

-- Axiom and theorem helpers (not exported)

type GraphTestsuite g = (Eq g, EdgeGraph g) => g -> g -> g -> Property

(<=) :: (Eq g, EdgeGraph g) => g -> g -> Bool
(<=) = isSubgraphOf

(//) :: Testable prop => prop -> String -> Property
p // s = label s $ counterexample ("Failed when checking '" ++ s ++ "'") p

infixl 1 //
infixl 4 <=

axioms :: GraphTestsuite g
axioms x y z = conjoin
        -- Overlay bounded semilattice
        [         x +++ y == y +++ x                              // "Overlay commutativity"
        , x +++ (y +++ z) == (x +++ y) +++ z                     // "Overlay associativity"
        ,         x +++ x == x                                    // "Overlay idempotence"
        ,     x +++ empty == x                                    // "Overlay identity"
        -- Into identity
        ,     empty >+> x == x                                    // "Left into identity"
        ,     x >+> empty == x                                    // "Right into identity"
        -- Pits identity and commutativity
        ,     empty <+> x == x                                    // "Left pits identity"
        ,     x <+> empty == x                                    // "Right pits identity"
        ,       x <+> y == y <+> x                                // "Pits commutativity"
        -- Tips identity and commutativity
        ,     empty >+< x == x                                    // "Left tips identity"
        ,     x >+< empty == x                                    // "Right tips identity"
        ,       x >+< y == y >+< x                               // "Tips commutativity"
        -- Same-operator decomposition
        , x >+> (y >+> z) == x >+> y +++ x >+> z +++ y >+> z    // "Into decomposition"
        , x <+> (y <+> z) == x <+> y +++ x <+> z +++ y <+> z    // "Pits decomposition"
        , x >+< (y >+< z) == x >+< y +++ x >+< z +++ y >+< z   // "Tips decomposition"
        -- Cross-operator decomposition: into / pits
        , x >+> (y <+> z) == x >+> y +++ x >+> z +++ y <+> z    // "Into-pits left decomposition"
        , (x >+> y) <+> z == x >+> y +++ x <+> z +++ y <+> z    // "Into-pits right decomposition"
        -- Cross-operator decomposition: into / tips
        , x >+> (y >+< z) == x >+> y +++ x >+> z +++ y >+< z    // "Into-tips left decomposition"
        , (x >+> y) >+< z == x >+> y +++ x >+< z +++ y >+< z    // "Into-tips right decomposition"
        -- Cross-operator decomposition: pits / into
        , x <+> (y >+> z) == x <+> y +++ x <+> z +++ y >+> z    // "Pits-into left decomposition"
        , (x <+> y) >+> z == x <+> y +++ x >+> z +++ y >+> z    // "Pits-into right decomposition"
        -- Cross-operator decomposition: pits / tips
        , x <+> (y >+< z) == x <+> y +++ x <+> z +++ y >+< z    // "Pits-tips left decomposition"
        , (x <+> y) >+< z == x <+> y +++ x >+< z +++ y >+< z    // "Pits-tips right decomposition"
        -- Cross-operator decomposition: tips / into
        , x >+< (y >+> z) == x >+< y +++ x >+< z +++ y >+> z    // "Tips-into left decomposition"
        , (x >+< y) >+> z == x >+< y +++ x >+> z +++ y >+> z    // "Tips-into right decomposition"
        -- Cross-operator decomposition: tips / pits
        , x >+< (y <+> z) == x >+< y +++ x >+< z +++ y <+> z    // "Tips-pits left decomposition"
        , (x >+< y) <+> z == x >+< y +++ x <+> z +++ y <+> z    // "Tips-pits right decomposition"
        ]

edgeAxioms :: forall g. (Eq g, EdgeGraph g) => Edge g -> Edge g -> Edge g -> Property
edgeAxioms a b c = conjoin
    -- Reflexive axioms
    [ edge a <+> edge a == (edge a :: g)                      // "Pits reflexivity"
    , edge a >+< edge a == (edge a :: g)                      // "Tips reflexivity"
    -- Transitive axioms (edge values are non-empty)
    , ea <+> eb +++ ea <+> ec == ea <+> (eb <+> ec)           // "Pits transitivity"
    , eb >+> ea +++ ea <+> ec == eb >+> (ea <+> ec)           // "Into-pits transitivity"
    , ea >+> eb +++ ea >+> ec == ea >+> (eb <+> ec)           // "Into-into transitivity"
    , ea >+< eb +++ ea >+> ec == (ea >+< eb) >+> ec           // "Tips-into transitivity"
    , eb >+> ea +++ ec >+> ea == (eb >+< ec) >+> ea           // "Into right transitivity"
    , ea >+< eb +++ ea >+< ec == ea >+< (eb >+< ec)          // "Tips transitivity"
    ]
  where
    ea :: g
    ea = edge a
    eb :: g
    eb = edge b
    ec :: g
    ec = edge c

theorems :: GraphTestsuite g
theorems x y z = conjoin
    -- Associativity (follows from same-operator decomposition)
    [ x >+> (y >+> z) == (x >+> y) >+> z                    // "Into associativity"
    , x <+> (y <+> z) == (x <+> y) <+> z                     // "Pits associativity"
    , x >+< (y >+< z) == (x >+< y) >+< z                    // "Tips associativity"
    -- Distributivity over overlay
    , x >+> (y +++ z) == x >+> y +++ x >+> z                 // "Left into distributivity"
    , (x +++ y) >+> z == x >+> z +++ y >+> z                 // "Right into distributivity"
    , x <+> (y +++ z) == x <+> y +++ x <+> z                 // "Pits distributivity"
    , x >+< (y +++ z) == x >+< y +++ x >+< z                // "Tips distributivity"
    -- Absorption
    , x >+> y +++ x +++ y == x >+> y                         // "Into absorption"
    , x <+> y +++ x +++ y == x <+> y                          // "Pits absorption"
    , x >+< y +++ x +++ y == x >+< y                         // "Tips absorption"
    -- Saturation
    , x >+> x == (x >+> x) >+> x                             // "Into saturation"
    , x <+> x == x <+> (x <+> x)                              // "Pits saturation"
    , x >+< x == x >+< (x >+< x)                             // "Tips saturation"
    -- Subgraph ordering
    ,         empty <= x                                      // "Lower bound"
    ,             x <= x +++ y                                // "Overlay order"
    ,     x +++ y <= x >+> y                                  // "Overlay-into order"
    ,     x +++ y <= x <+> y                                  // "Overlay-pits order"
    ,     x +++ y <= x >+< y                                  // "Overlay-tips order"
    ]

testEmptyGroup :: forall g. TestableGraph g => IO ()
testEmptyGroup = do
    putStrLn "\n============ empty ============"
    test "isEmpty     empty == True" $
          isEmpty     (empty :: g) == True
    test "edgeCount empty == 0" $
          edgeCount (empty :: g) == 0
    test "nodeCount   empty == 0" $
          nodeCount   (empty :: g) == 0

testEdgeGroup :: forall g. TestableGraph g => IO ()
testEdgeGroup = do
    putStrLn "\n============ edge ============"
    test "isEmpty     (edge x) == False" $ \(x :: Edge g) ->
          isEmpty     (edge x :: g) == False
    test "hasEdge x (edge x) == True" $ \(x :: Edge g) ->
          hasEdge x (edge x :: g) == True
    test "hasEdge 1 (edge 2) == False" $
          hasEdge 1 (edge 2 :: g) == False
    test "edgeCount (edge x) == 1" $ \(x :: Edge g) ->
          edgeCount (edge x :: g) == 1
    test "nodeCount   (edge x) == 2" $ \(x :: Edge g) ->
          nodeCount   (edge x :: g) == 2

testOverlayGroup :: forall g. TestableGraph g => IO ()
testOverlayGroup = do
    putStrLn "\n============ overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: g) y ->
          isEmpty     (overlay x y) == (isEmpty   x   && isEmpty   y)
    test "edgeCount (overlay x y) >= edgeCount x" $ sizeLimit $ \(x :: g) y ->
          edgeCount (overlay x y) >= edgeCount x
    test "edgeCount (overlay x y) <= edgeCount x + edgeCount y" $ sizeLimit $ \(x :: g) y ->
          edgeCount x + edgeCount y >= edgeCount (overlay x y)

testIntoGroup :: forall g. TestableGraph g => IO ()
testIntoGroup = do
    putStrLn "\n============ into ============"
    test "isEmpty     (into x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(x :: g) y ->
          isEmpty     (into x y) == (isEmpty   x   && isEmpty   y)

testEdgesGroup :: forall g. TestableGraph g => IO ()
testEdgesGroup = do
    putStrLn "\n============ edges ============"
    test "edges []    == empty" $
          edges []    == (empty :: g)
    test "edges [x]   == edge x" $ \(x :: Edge g) ->
          edges [x]   == (edge x :: g)

testIsSubgraphOfGroup :: forall g. TestableGraph g => IO ()
testIsSubgraphOfGroup = do
    putStrLn "\n============ isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ sizeLimit $ \(x :: g) ->
          isSubgraphOf empty         x             == True
    test "isSubgraphOf x             (overlay x y) == True" $ sizeLimit $ \(x :: g) y ->
          isSubgraphOf x             (overlay x y) == True
    test "isSubgraphOf (overlay x y) (into x y)    == True" $ sizeLimit $ \(x :: g) y ->
          isSubgraphOf (overlay x y) (into x y)    == True

testIsEmptyGroup :: forall g. TestableGraph g => IO ()
testIsEmptyGroup = do
    putStrLn "\n============ isEmpty ============"
    test "isEmpty empty                                     == True" $
          isEmpty (empty :: g)                              == True
    test "isEmpty (overlay empty empty)                     == True" $
          isEmpty (overlay empty empty :: g)                == True
    test "isEmpty (edge x)                                  == False" $ \(x :: Edge g) ->
          isEmpty (edge x :: g)                             == False
    test "isEmpty (removeEdge x $ edge x)              == True" $ \(x :: Edge g) ->
          isEmpty (removeEdge x $ edge x :: g)              == True

testHasEdgeGroup :: forall g. TestableGraph g => IO ()
testHasEdgeGroup = do
    putStrLn "\n============ hasEdge ============"
    test "hasEdge x empty            == False" $ \(x :: Edge g) ->
          hasEdge x (empty :: g)     == False
    test "hasEdge x (edge x)         == True" $ \(x :: Edge g) ->
          hasEdge x (edge x :: g)    == True

testEdgeCountGroup :: forall g. TestableGraph g => IO ()
testEdgeCountGroup = do
    putStrLn "\n============ edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount (empty :: g) == 0
    test "edgeCount (edge x)   == 1" $ \(x :: Edge g) ->
          edgeCount (edge x :: g) == 1
    test "edgeCount            == length . edgeList" $ sizeLimit $ \(x :: g) ->
          edgeCount x          == (length . edgeList) x

testNodeCountGroup :: forall g. TestableGraph g => IO ()
testNodeCountGroup = do
    putStrLn "\n============ nodeCount ============"
    test "nodeCount empty      == 0" $
          nodeCount (empty :: g) == 0
    test "nodeCount (edge x)   == 2" $ \(x :: Edge g) ->
          nodeCount (edge x :: g) == 2

testEdgeListGroup :: forall g. TestableGraph g => IO ()
testEdgeListGroup = do
    putStrLn "\n============ edgeList ============"
    test "edgeList empty      == []" $
          edgeList (empty :: g) == []
    test "edgeList (edge x)   == [x]" $ \(x :: Edge g) ->
          edgeList (edge x :: g) == [x]
    test "edgeList . edges    == nub . sort" $ \(xs :: [Edge g]) ->
          edgeList (edges xs :: g) == (nubOrd . sort) xs

testEdgeSetGroup :: forall g. TestableGraph g => IO ()
testEdgeSetGroup = do
    putStrLn "\n============ edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet (empty :: g) == Set.empty
    test "edgeSet . edge     == Set.singleton" $ \(x :: Edge g) ->
          edgeSet (edge x :: g) == Set.singleton x
    test "edgeSet . edges    == Set.fromList" $ \(xs :: [Edge g]) ->
          edgeSet (edges xs :: g) == Set.fromList xs

testEdgeIntSetGroup :: forall g. TestableGraph g => IO ()
testEdgeIntSetGroup = do
    putStrLn "\n============ edgeIntSet ============"
    test "edgeIntSet empty      == IntSet.empty" $
          edgeIntSet (empty :: g) == IntSet.empty
    test "edgeIntSet . edge     == IntSet.singleton" $ \(x :: Edge g) ->
          edgeIntSet (edge x :: g) == IntSet.singleton (fromIntegral x)

testPathGroup :: forall g. TestableGraph g => IO ()
testPathGroup = do
    putStrLn "\n============ path ============"
    test "path []    == empty" $
          path []    == (empty :: g)
    test "path [x]   == edge x" $ \(x :: Edge g) ->
          path [x]   == (edge x :: g)
    test "path [x,y] == into (edge x) (edge y)" $ \(x :: Edge g) y ->
          path [x,y] == (into (edge x) (edge y) :: g)

testCircuitGroup :: forall g. TestableGraph g => IO ()
testCircuitGroup = do
    putStrLn "\n============ circuit ============"
    test "circuit []    == empty" $
          circuit []    == (empty :: g)
    test "circuit [x]   == into (edge x) (edge x)" $ \(x :: Edge g) ->
          circuit [x]   == (into (edge x) (edge x) :: g)

testCliqueGroup :: forall g. TestableGraph g => IO ()
testCliqueGroup = do
    putStrLn "\n============ clique ============"
    test "clique []      == empty" $
          clique []      == (empty :: g)
    test "clique [x]     == edge x" $ \(x :: Edge g) ->
          clique [x]     == (edge x :: g)
    test "clique [x,y]   == into (edge x) (edge y)" $ \(x :: Edge g) y ->
          clique [x,y]   == (into (edge x) (edge y) :: g)

testBicliqueGroup :: forall g. TestableGraph g => IO ()
testBicliqueGroup = do
    putStrLn "\n============ biclique ============"
    test "biclique []  []  == empty" $
          biclique [] []   == (empty :: g)
    test "biclique [x] []  == edge x" $ \(x :: Edge g) ->
          biclique [x] []  == (edge x :: g)
    test "biclique []  [y] == edge y" $ \(y :: Edge g) ->
          biclique [] [y]  == (edge y :: g)
    test "biclique [x] [y] == into (edge x) (edge y)" $ \(x :: Edge g) y ->
          biclique [x] [y] == (into (edge x) (edge y) :: g)

testFlowerGroup :: forall g. TestableGraph g => IO ()
testFlowerGroup = do
    putStrLn "\n============ flower ============"
    test "flower []      == empty" $
          flower []      == (empty :: g)
    test "flower [x]     == into (edge x) (edge x)" $ \(x :: Edge g) ->
          flower [x]     == (into (edge x) (edge x) :: g)
    test "flower [x]     == circuit [x]" $ \(x :: Edge g) ->
          flower [x]     == (circuit [x] :: g)

testNodeGroup :: forall g. TestableGraph g => IO ()
testNodeGroup = do
    putStrLn "\n============ node ============"
    test "node []  []    == empty" $
          node [] []     == (empty :: g)
    test "node [x] []    == edge x" $ \(x :: Edge g) ->
          node [x] []    == (edge x :: g)
    test "node []  [y]   == edge y" $ \(y :: Edge g) ->
          node [] [y]    == (edge y :: g)
    test "node [x] [y]   == into (edge x) (edge y)" $ \(x :: Edge g) y ->
          node [x] [y]   == (into (edge x) (edge y) :: g)

testRemoveEdgeGroup :: forall g. TestableGraph g => IO ()
testRemoveEdgeGroup = do
    putStrLn "\n============ removeEdge ============"
    test "removeEdge x (edge x)              == empty" $ \(x :: Edge g) ->
          removeEdge x (edge x :: g)         == (empty :: g)
    test "removeEdge x . removeEdge x   == removeEdge x" $ sizeLimit $ \(x :: Edge g) (y :: g) ->
         (removeEdge x . removeEdge x) y == removeEdge x y

testReplaceEdgeGroup :: forall g. TestableGraph g => IO ()
testReplaceEdgeGroup = do
    putStrLn "\n============ replaceEdge ============"
    test "replaceEdge x x            == id" $ sizeLimit $ \(x :: Edge g) (y :: g) ->
          replaceEdge x x y          == y
    test "replaceEdge x y (edge x)   == edge y" $ \(x :: Edge g) y ->
          replaceEdge x y (edge x :: g) == (edge y :: g)

testGmapGroup :: forall g. TestableGraph g => IO ()
testGmapGroup = do
    putStrLn "\n============ gmap ============"
    test "gmap f empty      == empty" $ \(apply -> (f :: Edge g -> Edge g)) ->
          gmap f (empty :: g) == (empty :: g)
    test "gmap f (edge x)   == edge (f x)" $ \(apply -> (f :: Edge g -> Edge g)) (x :: Edge g) ->
          gmap f (edge x :: g) == (edge (f x) :: g)
    test "gmap id           == id" $ sizeLimit $ \(x :: g) ->
          gmap id x         == x
    test "gmap f . gmap g   == gmap (f . g)" $ sizeLimit $
          \(apply -> (f :: Edge g -> Edge g)) (apply -> (g :: Edge g -> Edge g)) (x :: g) ->
         (gmap f . gmap g) x == gmap (f . g) x

testInduceGroup :: forall g. TestableGraph g => IO ()
testInduceGroup = do
    putStrLn "\n============ induce ============"
    test "induce (const True)  x      == x" $ sizeLimit $ \(x :: g) ->
          induce (const True)  x      == x
    test "induce (const False) x      == empty" $ sizeLimit $ \(x :: g) ->
          induce (const False) x      == (empty :: g)
    test "induce (/= x)               == removeEdge x" $ sizeLimit $ \(x :: Edge g) (y :: g) ->
          induce (/= x) y             == removeEdge x y
