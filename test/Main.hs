import EdgeGraph.Test.AdjacencyMap
import EdgeGraph.Test.Fold
import EdgeGraph.Test.EdgeGraph
import EdgeGraph.Test.IntAdjacencyMap
import EdgeGraph.Test.Incidence
main :: IO ()
main = do
    testAdjacencyMap
    testFold
    testGraph
    testIntAdjacencyMap
    testIncidence
