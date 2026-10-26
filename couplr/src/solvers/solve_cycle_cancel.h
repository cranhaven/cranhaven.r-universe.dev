// src/solvers/solve_cycle_cancel.h
// Pure C++ Cycle Canceling LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using cycle canceling with Karp's minimum mean cycle algorithm
// Finds initial feasible flow, then iteratively cancels negative cost cycles
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
//   DimensionException if matrix is empty
//   ConvergenceException if algorithm fails to converge
LapResult solve_cycle_cancel(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
