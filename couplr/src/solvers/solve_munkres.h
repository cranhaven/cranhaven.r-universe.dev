// src/solvers/solve_munkres.h
// Pure C++ Munkres (matrix-form Kuhn-Munkres) LAP solver - NO Rcpp dependencies.
// O(n^4) textbook implementation using star/prime/cover bookkeeping.
// Kept for pedagogical value and as a reference implementation. For
// production-speed assignment, use solve_hungarian (O(n^3)) or solve_jv.
#pragma once

#include "../core/lap_types.h"

namespace lap {

LapResult solve_munkres(const CostMatrix& cost, bool maximize = false);

}  // namespace lap
