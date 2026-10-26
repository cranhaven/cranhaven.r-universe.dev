// src/solvers/sap_dense/sap_dense_solve_rcpp.cpp
// Rcpp wrapper for the dense-scan successive shortest path solver

#include <Rcpp.h>
#include "sap_dense_solve.h"
#include "../../core/lap_error.h"
#include "../../core/lap_utils_rcpp.h"

// Rcpp-exported wrapper
Rcpp::List sap_dense_solve_impl(Rcpp::NumericMatrix cost, bool maximize = false) {
    try {
        // Convert to pure C++ types
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);

        // Call pure C++ solver
        lap::LapResult result = lap::solve_sap_dense(cm, maximize);

        // Convert back to Rcpp
        return lap_result_to_rcpp(result, cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::List();
}
