// src/solvers/solve_auction_rcpp.cpp
// Rcpp wrappers for the auction solvers - delegate to the pure C++
// implementations. The algorithms live in solve_auction.cpp (lap::solve_auction,
// lap::solve_auction_gs, lap::solve_auction_scaled, lap::solve_auction_scaled_params),
// the single source of truth exercised by cpp_tests, so the shipped path and the
// tested path are identical.

#include <Rcpp.h>
#include <cmath>
#include "solve_auction.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

Rcpp::List solve_auction_impl(NumericMatrix cost, bool maximize, double eps_in) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    double eps = (std::isfinite(eps_in) && eps_in > 0.0) ? eps_in : -1.0;
    lap::LapResult result = lap::solve_auction(cm, maximize, eps);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}

Rcpp::List solve_auction_gauss_seidel_impl(NumericMatrix cost, bool maximize, double eps_in) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    double eps = (std::isfinite(eps_in) && eps_in > 0.0) ? eps_in : -1.0;
    long long bids = 0;
    lap::LapResult result = lap::solve_auction_gs(cm, maximize, eps, &bids);
    Rcpp::List out = lap_result_to_rcpp(result, cost);
    out["bids"] = static_cast<double>(bids);
    return out;
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}

Rcpp::List solve_auction_scaled_impl(NumericMatrix cost, bool maximize,
                                     double initial_epsilon_factor,
                                     double alpha,
                                     double final_epsilon) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_auction_scaled_params(
        cm, maximize, initial_epsilon_factor, alpha, final_epsilon);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}

Rcpp::List solve_auction_scaled_impl(NumericMatrix cost, bool maximize,
                                     std::string schedule) {
  try {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::LapResult result = lap::solve_auction_scaled(cm, maximize, schedule);
    return lap_result_to_rcpp(result, cost);
  } catch (const lap::LapException& e) {
    Rcpp::stop(e.what());
  }
  return Rcpp::List();  // unreachable
}
