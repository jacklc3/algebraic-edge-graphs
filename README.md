# algebraic-edge-graphs

A library for algebraic construction and manipulation of edge-indexed graphs in
Haskell. See [this paper](https://jackliellcock.com/papers/edge_graphs/paper.pdf)
for the motivation behind the library and the underlying theory.

## Installation

```
cabal install algebraic-edge-graphs
```

## Getting started

The core data type is `EdgeGraph`, built from six primitives:

```haskell
import EdgeGraph
import EdgeGraph.Class

-- A single edge labelled 1
e1 = edge 1 :: EdgeGraph Int

-- Two edges connected in sequence: 1 flows into 2
p = into (edge 1) (edge 2) :: EdgeGraph Int

-- A path through edges 1, 2, 3
p3 = path [1, 2, 3] :: EdgeGraph Int

-- Overlay places edges side by side (no connection)
disconnected = overlay (edge 1) (edge 2) :: EdgeGraph Int

-- A cycle through edges 1 and 2
c = circuit [1, 2] :: EdgeGraph Int
```

### Folding over graphs

The `EdgeGraph.Fold` module provides semiring-based path algorithms via the
Boehm-Berarducci encoding:

```haskell
import EdgeGraph
import qualified EdgeGraph.Fold as F

-- Convert to Fold representation
g = into (edge 1) (edge 2) :: EdgeGraph Int
f = toFold g

-- Shortest paths using edge labels as weights
sp = F.shortestPaths id f

-- Widest (bottleneck) paths
wp = F.widestPaths id f

-- Reachability, cycle detection
r  = F.reachable f
ac = F.isAcyclic f
```

### Adjacency map representation

For algorithms like DFS, topological sort, and strongly connected components,
convert to `AdjacencyMap`:

```haskell
import EdgeGraph
import qualified EdgeGraph.AdjacencyMap as AM

g = path [1, 2, 3] :: EdgeGraph Int
m = AM.fromEdgeGraph g

AM.topSort m   -- Just [1, 2, 3]
AM.dfsForest m -- depth-first search forest
AM.scc m       -- strongly connected components
```

## Key modules

| Module | Description |
|---|---|
| `EdgeGraph` | Core data type and graph operations |
| `EdgeGraph.Class` | Type class for polymorphic graph construction |
| `EdgeGraph.Fold` | Boehm-Berarducci encoding and semiring path algorithms |
| `EdgeGraph.AdjacencyMap` | Adjacency map with DFS, topological sort, SCC |
| `EdgeGraph.IntAdjacencyMap` | `Int`-specialised adjacency map |
| `EdgeGraph.Incidence` | Incidence representation for node-level structure |
| `EdgeGraph.HigherKinded.Class` | Higher-kinded type class for graph construction |

## Acknowledgements

This project was originally forked from Andrey Mokhov's
[algebraic-graphs](https://github.com/snowleopard/alga) library,
the theory of which was provided in
[this paper](https://dl.acm.org/doi/10.1145/3122955.3122956).
The codebase has changed substantially,
but the original work provided the foundation for this project.
