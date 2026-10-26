// src/solvers/solve_cycle_cancel_rcpp.cpp
// Rcpp wrapper for the cycle-canceling solver - delegates to the pure C++
// implementation. The algorithm lives in solve_cycle_cancel.cpp
// (lap::solve_cycle_cancel), the single source of truth exercised by cpp_tests,
// so the shipped path and the tested path are identical.
// [[Rcpp::plugins(cpp17)]]

#include <Rcpp.h>
#include "solve_cycle_cancel.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List solve_cycle_cancel_impl(NumericMatrix cost, bool maximize) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_cycle_cancel(cm, maximize);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}
