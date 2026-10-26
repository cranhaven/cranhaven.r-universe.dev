// src/solvers/solve_hk01.h
// Pure C++ Hopcroft-Karp solver for 0/1 costs - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Hopcroft-Karp for binary {0,1} or uniform cost matrices
// Optimized for cases where all allowed edges have equal cost or costs are {0,1}
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
//   LapException if cost matrix is not uniform or binary {0,1}
LapResult solve_hk01(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
