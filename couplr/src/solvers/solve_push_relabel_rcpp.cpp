// src/solvers/solve_push_relabel_rcpp.cpp
// Rcpp wrapper for the push-relabel solver - delegates to the pure C++
// implementation. The algorithm lives in solve_push_relabel.cpp
// (lap::solve_push_relabel), the single source of truth exercised by cpp_tests,
// so the shipped path and the tested path are identical.

#include <Rcpp.h>
#include "solve_push_relabel.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List solve_push_relabel_impl(NumericMatrix cost, bool maximize) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_push_relabel(cm, maximize);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}
