{-# LANGUAGE RankNTypes #-}
module EdgeGraph.Test (
    module Data.List,
    module Data.List.Extra,
    module Test.QuickCheck,
    module Test.QuickCheck.Function,

    GraphTestsuite, axioms, theorems, test,
    ) where

import Data.List (sort)
import Data.List.Extra (nubOrd)
import Prelude hiding ((+), (*), (<=))
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

(+) :: EdgeGraph g => g -> g -> g
(+) = overlay

(*) :: EdgeGraph g => g -> g -> g
(*) = into

(<=) :: (Eq g, EdgeGraph g) => g -> g -> Bool
(<=) = isSubgraphOf

(//) :: Testable prop => prop -> String -> Property
p // s = label s $ counterexample ("Failed when checking '" ++ s ++ "'") p

infixl 1 //
infixl 4 <=
infixl 6 +
infixl 7 *

type GraphTestsuite g = (Eq g, EdgeGraph g) => g -> g -> g -> Property

axioms :: GraphTestsuite g
axioms x y z = conjoin
    [       x + y == y + x                      // "Overlay commutativity"
    , x + (y + z) == (x + y) + z                // "Overlay associativity"
    ,   empty * x == x                          // "Left into identity"
    ,   x * empty == x                          // "Right into identity"
    , x * (y * z) == (x * y) * z                // "Into associativity"
    , x * (y + z) == x * y + x * z              // "Left distributivity"
    , (x + y) * z == x * z + y * z              // "Right distributivity"
    ,   x * y * z == x * y + x * z + y * z      // "Decomposition" ]

theorems :: GraphTestsuite g
theorems x y z = conjoin
    [     x + empty == x                        // "Overlay identity"
    ,         x + x == x                        // "Overlay idempotence"
    , x + y + x * y == x * y                    // "Absorption"
    ,     x * y * z == x * y + x * z + y * z
                     + x + y + z + empty        // "Full decomposition"
    ,         x * x == x * x * x                // "Into saturation"
    ,         empty <= x                        // "Lower bound"
    ,             x <= x + y                    // "Overlay order"
    ,         x + y <= x * y                    // "Overlay-into order" ]

