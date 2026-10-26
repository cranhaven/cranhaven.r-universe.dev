// src/solvers/solve_bottleneck_rcpp.cpp
// Rcpp wrapper for the Bottleneck Assignment Problem solver - delegates to the
// pure C++ implementation. The algorithm lives in solve_bottleneck.cpp
// (lap::solve_bottleneck), the single source of truth exercised by cpp_tests,
// so the shipped path and the tested path are identical.
//
// Unlike the sum-assignment solvers, bottleneck reports the minimax objective
// (the maximum chosen edge) as total_cost, so this adapter preserves
// result.total_cost instead of recomputing an edge sum.

#include <Rcpp.h>
#include "solve_bottleneck.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List solve_bottleneck_impl(NumericMatrix cost, bool maximize) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_bottleneck(cm, maximize);

    const int n = static_cast<int>(result.assignment.size());
    IntegerVector match(n);
    for (int i = 0; i < n; ++i) {
      match[i] = (result.assignment[i] >= 0) ? (result.assignment[i] + 1) : 0;
    }
    return make_result(match, result.total_cost);  // total_cost = bottleneck value
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}
