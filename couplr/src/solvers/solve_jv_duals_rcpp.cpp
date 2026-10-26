// src/solvers/solve_jv_duals.cpp
// Rcpp wrapper for JV duals solver - calls pure C++ implementation

#include <Rcpp.h>
#include "solve_jv_duals.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

// Helper: Convert lap::DualResult to Rcpp::List (with 1-based indexing for assignment)
static Rcpp::List dual_result_to_rcpp(const lap::DualResult& result,
                                       const Rcpp::NumericMatrix& original_cost) {
    const int n = static_cast<int>(result.solution.assignment.size());

    // Convert 0-based to 1-based for assignment
    Rcpp::IntegerVector match(n);
    for (int i = 0; i < n; ++i) {
        match[i] = (result.solution.assignment[i] >= 0) ? (result.solution.assignment[i] + 1) : 0;
    }

    // Use compute_total_cost for consistency with other solvers
    double total = compute_total_cost(original_cost, match);

    // Dual variables stay 0-based
    Rcpp::NumericVector u(result.u.begin(), result.u.end());
    Rcpp::NumericVector v(result.v.begin(), result.v.end());

    return Rcpp::List::create(
        Rcpp::Named("match") = match,
        Rcpp::Named("total_cost") = total,
        Rcpp::Named("u") = u,
        Rcpp::Named("v") = v
    );
}

// The Jonker-Volgenant core with or without its pre-stages. Both are the same
// shortest-augmenting-path solve and both return the potentials it computes.
static Rcpp::List solve_jv_core_rcpp(Rcpp::NumericMatrix cost, bool maximize,
                                     bool warm_start) {
    try {
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
        lap::DualResult result = lap::solve_jv_duals(cm, maximize, warm_start);
        return dual_result_to_rcpp(result, cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}

Rcpp::List solve_jv_duals_impl(Rcpp::NumericMatrix cost, bool maximize) {
    return solve_jv_core_rcpp(cost, maximize, /*warm_start=*/true);
}

Rcpp::List solve_hungarian_impl(Rcpp::NumericMatrix cost, bool maximize) {
    return solve_jv_core_rcpp(cost, maximize, /*warm_start=*/false);
}
