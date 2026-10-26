// src/solvers/solve_bottleneck.h
// Pure C++ Bottleneck Assignment Problem (BAP) solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve Bottleneck Assignment Problem using binary search + Hopcroft-Karp
// Minimizes the MAXIMUM edge cost in a perfect matching (minimax objective)
//
// Algorithm:
// 1. Collect all unique finite costs and sort them
// 2. Binary search on threshold T
// 3. For each T, check if perfect matching exists using edges with cost <= T
// 4. Use Hopcroft-Karp as the bipartite matching subroutine
//
// Complexity: O(E√V log(unique costs)) where E = edges, V = vertices
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, maximize the minimum edge cost (maximin objective)
//
// Returns:
//   LapResult with 0-based assignment and bottleneck value as total_cost
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
LapResult solve_bottleneck(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
