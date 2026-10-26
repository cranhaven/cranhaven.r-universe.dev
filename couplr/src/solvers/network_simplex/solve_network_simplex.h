// src/solvers/network_simplex/solve_network_simplex.h
// Pure C++ Network Simplex LAP solver - NO Rcpp dependencies
#pragma once

#include "../../core/lap_types.h"

namespace lap {

// Solve LAP using Network Simplex algorithm
// Based on LEMON library and Király-Kovács (2012)
//
// Network Simplex maintains a spanning tree of the flow network and performs
// pivots to improve the solution. For assignment problems, the network has:
//   - Source node (supplies n units)
//   - n row nodes (transshipment)
//   - m column nodes (transshipment)
//   - Sink node (demands n units)
//
// The algorithm iteratively finds entering arcs with negative reduced cost
// and performs basis exchanges until optimality.
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//
// Returns:
//   LapResult with 0-based assignment and total cost (using original costs).
//   status is "optimal" when pricing reached the simplex optimality condition,
//   and "iteration_limit" when the pivot cap ended the loop first, in which case
//   the assignment is feasible but its optimality is unproven.
//
// Throws:
//   InfeasibleException if no assignment covers every row
//   DimensionException if nrow > ncol
//   ConvergenceException if the final basis leaves a row unmatched even though
//     a matching covering every row was found before the pivot loop
LapResult solve_network_simplex(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
