// src/solvers/solve_jv_duals.h
// Pure C++ Jonker-Volgenant solver with dual variables - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include "../core/lap_lazy_types.h"

namespace lap {

// Solve LAP using Jonker-Volgenant and return dual variables
// Handles rectangular matrices where nrow <= ncol
//
// For an optimal assignment, the duals satisfy complementary slackness:
//   u[i] + v[j] <= c[i,j] for all (i,j) (minimization)
//   u[i] + v[j] >= c[i,j] for all (i,j) (maximization)
//   u[i] + v[j] = c[i,j] for assigned pairs
//
// Parameters:
//   cost: Cost matrix (row-major, with mask for forbidden edges)
//   maximize: If true, find maximum weight matching (costs negated internally)
//   warm_start: Run the LAPJV pre-stages (column reduction and augmenting row
//     reduction) before the shortest-path phase. Off, the same core is the
//     classic Hungarian method in shortest-augmenting-path form, which is what
//     solve_hungarian() asks for.
//
// Returns:
//   DualResult with:
//     - solution: LapResult with 0-based assignment and total cost (using original costs)
//     - u: row potentials (0-based indexing)
//     - v: column potentials (0-based indexing)
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
DualResult solve_jv_duals(const CostMatrix& cost, bool maximize = false,
                          bool warm_start = true);

// The same solve over a cost source that computes its cells on demand, so a
// lazy problem can be certified without materializing the matrix the
// certificate is a statement about. The duals come back in the orientation of
// the specification and on the original (unnegated) cost scale, which is what
// lap::certify_assignment() reads.
//
// `maximize` is not a parameter here: a LazyCostMatrix is negated at
// construction, and it reports that through is_negated().
DualResult solve_jv_duals(const LazyCostMatrix& cost);

}  // namespace lap
