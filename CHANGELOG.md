# Changelog

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
