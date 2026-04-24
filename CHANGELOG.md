# Changelog

## 0.1.1 — 2026-04-24

* Added reference to the theory in [this paper](https://jackliellcock.com/papers/edge_graphs/paper.pdf).
* Fixed broken Haddock links throughout the documentation.
* Tightened whitespace and alignment of formula blocks in module documentation for consistent rendering on Hackage.

## 0.1.0 — 2026-03-10

* Initial release.
* Core algebraic edge graph data type (`EdgeGraph`) with six primitives:
  `empty`, `edge`, `overlay`, `into`, `pits`, `tips`.
* Boehm-Berarducci encoding (`EdgeGraph.Fold`) with semiring path algorithms:
  `shortestPaths`, `widestPaths`, `reachable`, `isReachable`, `isAcyclic`.
* Adjacency map representations (`AdjacencyMap`, `IntAdjacencyMap`) with
  DFS, topological sort, and strongly connected components.
* Incidence representation (`Incidence`) for node-level graph structure.
* Type classes for polymorphic graph construction (`EdgeGraph.Class`,
  `EdgeGraph.HigherKinded.Class`).
