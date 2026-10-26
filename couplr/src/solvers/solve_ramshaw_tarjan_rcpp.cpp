// src/solvers/solve_ramshaw_tarjan.cpp
// Rcpp wrapper for Ramshaw-Tarjan solver - calls pure C++ implementation

#include <Rcpp.h>
#include "solve_ramshaw_tarjan.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

// Rcpp-exported wrapper
Rcpp::List solve_ramshaw_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize) {
    try {
        // Convert to pure C++ types
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);

        // Call pure C++ solver
        lap::LapResult result = lap::solve_ramshaw_tarjan(cm, maximize);

        // Convert back to Rcpp
        return lap_result_to_rcpp(result, cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::List();
}
