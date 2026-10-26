// src/solvers/solve_csa.h
// Cost-scaling assignment entry point - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include "solve_auction.h"

namespace lap {

// Solve LAP with Goldberg and Kennedy's cost-scaling assignment algorithm
// CSA-Q (Mathematical Programming 71, 1995). Each refine divides epsilon by
// 10, clears the matching and discharges the active rows from a stack with
// the double-push operation on implicit row prices; the fourth-best heuristic
// lets a row find its two cheapest arcs among three it kept at its last scan.
// The phases run on the costs as supplied, and the final epsilon-optimal
// assignment is repaired to an optimal one.
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   stats (optional): receives the double-push and full row scan counts
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
LapResult solve_csa(const CostMatrix& cost, bool maximize = false,
                    EpsilonScalingStats* stats = nullptr);

}  // namespace lap
