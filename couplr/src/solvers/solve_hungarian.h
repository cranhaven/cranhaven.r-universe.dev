// src/solvers/solve_hungarian.h
// Pure C++ classic Hungarian (shortest-augmenting-path) LAP solver.
// O(n^3) on dense n x m matrices with n <= m. Handles forbidden edges.
// NO Rcpp dependencies.
#pragma once

#include "../core/lap_types.h"

namespace lap {

// Classic Hungarian algorithm in shortest-augmenting-path form (Kuhn-Munkres
// with row/column potentials). Same algorithmic core as solve_jv, but without
// the LAPJV pre-stages (column reduction + augmenting-row reduction).
LapResult solve_hungarian(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
