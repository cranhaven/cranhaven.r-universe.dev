// src/solvers/solve_csflow.h
// Pure C++ Cost-Scaling Flow LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Cost-Scaling Flow (min-cost max-flow with Dijkstra)
// Handles rectangular matrices where nrow <= ncol
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
LapResult solve_csflow(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
