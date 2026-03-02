import Test.AdjacencyMap
import Test.Fold
import Test.EdgeGraph
import Test.IntAdjacencyMap
import Test.Incidence

main :: IO ()
main = do
  testAdjacencyMap
  testFold
  testGraph
  testIntAdjacencyMap
  testIncidence
