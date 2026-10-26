// src/solvers/solve_ramshaw_tarjan.h
// Pure C++ Ramshaw-Tarjan LAP solver - NO Rcpp dependencies
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve LAP using Ramshaw-Tarjan algorithm (optimized for rectangular problems)
// Reference: Ramshaw & Tarjan (2012) "On minimum-cost assignments in
//            unbalanced bipartite graphs"
//
// Key insight: Instead of padding rectangular matrices to square, uses a modified
// Hungarian algorithm that maintains n row potentials and only tracks "active"
// column potentials. This avoids O(m) dummy operations when m >> n.
//
// Complexity: O(nm + n² log n) for m >> n
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
//
// Note: Automatically transposes if nrow > ncol to ensure rows <= cols
LapResult solve_ramshaw_tarjan(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
