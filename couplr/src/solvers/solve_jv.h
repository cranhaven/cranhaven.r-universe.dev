// src/solvers/solve_jv.h
// Pure C++ Jonker-Volgenant LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"
#include "../core/lap_lazy_types.h"

namespace lap {

// Solve LAP using Jonker-Volgenant (Hungarian-style with potentials)
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
LapResult solve_jv(const CostMatrix& cost, bool maximize = false);

// Lazy cost-source overload: computes distances on demand from the
// underlying feature data instead of a materialized dense matrix, trading
// compute for RAM. `maximize` is already baked into the source's internal
// negate flag at construction, so there is no separate parameter here (a
// second maximize argument would invite a "which one wins" ambiguity).
//
// Throws:
//   InfeasibleException if no valid matching exists
//   DimensionException if nrow > ncol
LapResult solve_jv(const LazyCostMatrix& cost);

}  // namespace lap
