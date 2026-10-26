// src/solvers/sap_dense/sap_dense_solve.h
// Pure C++ successive shortest path LAP solver, dense scan - NO Rcpp dependencies
#pragma once

#include "../../core/lap_types.h"

namespace lap {

// Solve LAP by successive shortest paths.
//
// Each augmentation runs Dijkstra on reduced costs from one unassigned row and
// reweights the potentials by Johnson's rule. The priority queue is a linear
// scan over the columns rather than a heap, which suits a dense cost matrix
// where every column is a candidate at every step.
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
//
// Complexity: O(n * m^2). Each of the n augmentations finalizes at most m
// columns and scans m columns to select each one.
LapResult solve_sap_dense(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
