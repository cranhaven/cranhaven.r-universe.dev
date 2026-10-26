// src/solvers/solve_bruteforce.h
// Pure C++ brute-force LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using exhaustive search (ONLY for very small problems)
// Enumerates all possible matchings and chooses the best
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
//   DimensionException if nrow > ncol or n > 8
//
// Note: Computational complexity is O(m!/(m-n)! * n), practical limit is n <= 8
LapResult solve_bruteforce(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
