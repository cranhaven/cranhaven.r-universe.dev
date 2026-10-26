// src/solvers/solve_csflow_rcpp.cpp
// Rcpp wrapper for the CSFlow solver - delegates to the pure C++ implementation.
// The algorithm lives in solve_csflow.cpp (lap::solve_csflow), the single source
// of truth exercised by cpp_tests, so the shipped path and the tested path are
// identical.

#include <Rcpp.h>
#include "solve_csflow.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List solve_csflow_impl(NumericMatrix cost, bool maximize) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_csflow(cm, maximize);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}
