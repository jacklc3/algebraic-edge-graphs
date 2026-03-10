{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Testable.Instances () where

import Data.Foldable (toList)
import Arbitrary ()
import Testable.Graph (TestableGraph(..))
import Testable.AlgebraicGraph (TestableAlgebraicGraph(..))
import Testable.AdjacencyGraph (TestableAdjacencyGraph(..))

import qualified EdgeGraph                          as EG
import qualified EdgeGraph.Fold                     as F
import qualified EdgeGraph.AdjacencyMap             as AM
import qualified EdgeGraph.IntAdjacencyMap          as IAM
import qualified EdgeGraph.Incidence                as I
import qualified EdgeGraph.AdjacencyMap.Internal    as AMI
import qualified EdgeGraph.IntAdjacencyMap.Internal as IAMI

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- ---------------------------------------------------------------------------
-- TestableGraph instances
-- ---------------------------------------------------------------------------

instance TestableGraph (EG.EdgeGraph Int) where
  isEmpty     = EG.isEmpty
  hasEdge     = EG.hasEdge
  edgeCount   = EG.edgeCount
  nodeCount   = EG.nodeCount
  edgeList    = EG.edgeList
  edgeSet     = EG.edgeSet
  edgeIntSet  = EG.edgeIntSet
  removeEdge  = EG.removeEdge
  replaceEdge = EG.replaceEdge
  gmap        = fmap
  induce      = EG.induce

instance TestableGraph (F.Fold Int) where
  isEmpty     = F.isEmpty
  hasEdge     = F.hasEdge
  edgeCount   = F.edgeCount
  nodeCount   = F.nodeCount
  edgeList    = F.edgeList
  edgeSet     = F.edgeSet
  edgeIntSet  = F.edgeIntSet
  removeEdge  = F.removeEdge
  replaceEdge = F.replaceEdge
  gmap        = F.gmap
  induce      = F.induce

instance TestableGraph (AM.AdjacencyMap Int) where
  isEmpty     = AM.isEmpty
  hasEdge     = AM.hasEdge
  edgeCount   = AM.edgeCount
  nodeCount   = AM.nodeCount
  edgeList    = AM.edgeList
  edgeSet     = AM.edgeSet
  edgeIntSet  = AM.edgeIntSet
  removeEdge  = \x -> AM.induce (/= x)
  replaceEdge = AM.replaceEdge
  gmap        = AM.gmap
  induce      = AM.induce

instance TestableGraph IAM.IntAdjacencyMap where
  isEmpty     = IAM.isEmpty
  hasEdge     = IAM.hasEdge
  edgeCount   = IAM.edgeCount
  nodeCount   = IAM.nodeCount
  edgeList    = IAM.edgeList
  edgeSet     = Set.fromList . IAM.edgeList
  edgeIntSet  = IAM.edgeIntSet
  removeEdge  = \x -> IAM.induce (/= x)
  replaceEdge = IAM.replaceEdge
  gmap        = IAM.gmap
  induce      = IAM.induce

instance TestableGraph (I.Incidence Int) where
  isEmpty     = I.isEmpty
  hasEdge     = I.hasEdge
  edgeCount   = I.edgeCount
  nodeCount   = I.nodeCount
  edgeList    = I.edgeList
  edgeSet     = I.edgeSet
  edgeIntSet  = I.edgeIntSet
  removeEdge  = \x -> I.induce (/= x)
  replaceEdge = I.replaceEdge
  gmap        = I.gmap
  induce      = I.induce

-- ---------------------------------------------------------------------------
-- TestableAlgebraicGraph instances
-- ---------------------------------------------------------------------------

instance TestableAlgebraicGraph (EG.EdgeGraph Int) where
  size       = EG.size
  foldg      = EG.foldg
  mergeEdges = EG.mergeEdges
  splitEdge  = EG.splitEdge
  transpose  = EG.transpose
  simplify   = EG.simplify
  bind       = (>>=)
  toEdgeList = toList

instance TestableAlgebraicGraph (F.Fold Int) where
  size       = F.size
  foldg      = F.foldg
  mergeEdges = F.mergeEdges
  splitEdge  = F.splitEdge
  transpose  = F.transpose
  simplify   = F.simplify
  bind       = F.bind
  toEdgeList = toList

-- ---------------------------------------------------------------------------
-- TestableAdjacencyGraph instances
-- ---------------------------------------------------------------------------

instance TestableAdjacencyGraph (AM.AdjacencyMap Int) where
  consistent    = AMI.consistent
  postset       = AM.postset
  preset        = AM.preset
  dfsForest     = AM.dfsForest
  topSort       = AM.topSort
  isTopSort     = AM.isTopSort
  detachPit     = AM.detachPit
  detachTip     = AM.detachTip
  toIncidence   = AM.toIncidence
  fromIncidence = AM.fromIncidence
  forkSet x g   = AMI.forks (AMI.adjacencyMap g Map.! x)
  joinSet x g   = AMI.joins (AMI.adjacencyMap g Map.! x)
  predSet x g   = AMI.preds (AMI.adjacencyMap g Map.! x)
  succSet x g   = AMI.succs (AMI.adjacencyMap g Map.! x)

instance TestableAdjacencyGraph IAM.IntAdjacencyMap where
  consistent    = IAMI.consistent
  postset       = IAM.postset
  preset        = IAM.preset
  dfsForest     = IAM.dfsForest
  topSort       = IAM.topSort
  isTopSort     = IAM.isTopSort
  detachPit     = IAM.detachPit
  detachTip     = IAM.detachTip
  toIncidence   = IAM.toIncidence
  fromIncidence = IAM.fromIncidence
  forkSet x g   = IAMI.forks (IAMI.adjacencyIntMap g Map.! x)
  joinSet x g   = IAMI.joins (IAMI.adjacencyIntMap g Map.! x)
  predSet x g   = IAMI.preds (IAMI.adjacencyIntMap g Map.! x)
  succSet x g   = IAMI.succs (IAMI.adjacencyIntMap g Map.! x)
