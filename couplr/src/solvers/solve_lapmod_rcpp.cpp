// src/solvers/solve_lapmod.cpp
// Rcpp wrapper for LAPMOD solver - calls pure C++ implementation

#include <Rcpp.h>
#include "solve_lapmod.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

// Rcpp-exported wrapper
Rcpp::List solve_lapmod_impl(Rcpp::NumericMatrix cost, bool maximize) {
    try {
        // Convert to pure C++ types
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);

        // Call pure C++ solver
        lap::LapResult result = lap::solve_lapmod(cm, maximize);

        // Convert back to Rcpp
        return lap_result_to_rcpp(result, cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::List();
}
