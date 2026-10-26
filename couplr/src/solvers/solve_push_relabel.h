// src/solvers/solve_push_relabel.h
// Pure C++ Goldberg-Tarjan cost-scaling push-relabel LAP solver.
// NO Rcpp dependencies.
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Solve the assignment problem by cost-scaling push-relabel.
//
// Goldberg & Tarjan (1990), "Finding minimum-cost circulations by successive
// approximation", Mathematics of Operations Research 15(3). The min-cost flow
// is reached through a sequence of eps-optimal flows: each phase divides eps,
// saturates the residual arcs the smaller eps no longer allows, and restores a
// feasible flow by pushing excess along admissible arcs and relabelling a node
// that has excess and no admissible arc out of it.
//
// The two operations are the algorithm, not a paraphrase of one: a push moves
// flow on an arc whose reduced cost the current eps admits, and a relabel
// raises a node's price by the least amount that admits one.
//
// eps-optimality means every residual arc has reduced cost >= -eps. With
// integer costs, a flow that is eps-optimal for eps < 1/n is optimal, which is
// what ends the scaling. Real-valued costs are scaled to integers first, so the
// same bound applies to the scaled problem.
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
LapResult solve_push_relabel(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
