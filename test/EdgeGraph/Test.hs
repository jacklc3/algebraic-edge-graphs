{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, ScopedTypeVariables #-}
module EdgeGraph.Test (
    module Data.List,
    module Data.List.Extra,
    module Test.QuickCheck,
    module Test.QuickCheck.Function,

    GraphTestsuite, EdgeTestsuite, axioms, edgeAxioms, theorems, test,
    ) where

import Data.List (sort)
import Data.List.Extra (nubOrd)
import Prelude hiding ((<=))
import System.Exit (exitFailure)
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Function

import EdgeGraph.Class
import EdgeGraph.Test.Arbitrary ()

test :: Testable a => String -> a -> IO ()
test str p = do
    result <- quickCheckWithResult (stdArgs { chatty = False }) p
    if isSuccess result
        then putStrLn $ "OK: " ++ str
        else do
            putStrLn $ "\nTest failure:\n    " ++ str ++ "\n"
            putStrLn $ output result
            exitFailure

(<=) :: (Eq g, EdgeGraph g) => g -> g -> Bool
(<=) = isSubgraphOf

(//) :: Testable prop => prop -> String -> Property
p // s = label s $ counterexample ("Failed when checking '" ++ s ++ "'") p

infixl 1 //
infixl 4 <=

type GraphTestsuite g = (Eq g, EdgeGraph g) => g -> g -> g -> Property

type EdgeTestsuite g = (Eq g, EdgeGraph g) => Edge g -> Edge g -> Edge g -> Property

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
