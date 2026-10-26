// src/solvers/solve_hk01_rcpp.cpp
// Rcpp wrapper for the hk01 solver - delegates to the pure C++ implementation.
// The algorithm lives in solve_hk01.cpp (lap::solve_hk01), the single source of
// truth exercised by cpp_tests, so the shipped path and the tested path are
// identical. lap::solve_hk01 falls back to lap::solve_csflow for {0,1} inputs
// whose zero-cost subgraph has no perfect matching, mirroring what this wrapper
// used to do via lap_solve_csflow.
// [[Rcpp::plugins(cpp17)]]

#include <Rcpp.h>
#include "solve_hk01.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List solve_hk01_impl(NumericMatrix cost, bool maximize) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_hk01(cm, maximize);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}
