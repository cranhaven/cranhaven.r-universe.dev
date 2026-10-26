// src/solvers/solve_csa_rcpp.cpp
// Rcpp wrapper for the CSA solver - calls the pure C++ implementation.
// The algorithm itself lives in solve_csa.cpp (lap::solve_csa), which is the
// single source of truth exercised by cpp_tests. This adapter only bridges
// Rcpp <-> pure types so the shipped path and the tested path are identical.

#include <Rcpp.h>
#include "solve_csa.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List solve_csa_impl(NumericMatrix cost, bool maximize) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_csa(cm, maximize);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }

  return Rcpp::List();  // unreachable
}
