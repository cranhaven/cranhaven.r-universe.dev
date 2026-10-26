// src/solvers/solve_lapmod.h
// Pure C++ LAPMOD solver (Sparse JV variant) - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve sparse LAP using LAPMOD (Sparse JV variant with CSR format)
// Optimized for matrices with >50% NA/Inf entries and n>100
//
// This implementation uses Compressed Sparse Row (CSR) format to exploit sparsity.
// The algorithm performs shortest augmenting path iterations using Dijkstra's algorithm.
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists (any row has no finite costs)
//   DimensionException if nrow > ncol
LapResult solve_lapmod(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
