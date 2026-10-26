// src/solvers/solve_ssp.h
// Pure C++ Successive Shortest Path (SSP) LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Successive Shortest Path (min-cost flow with Dijkstra)
// Handles rectangular matrices where nrow <= ncol (auto-transposes if needed)
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
LapResult solve_ssp(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
