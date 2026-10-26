// src/solvers/solve_munkres_rcpp.cpp
// Rcpp wrapper for the Munkres (O(n^4)) solver.

#include <Rcpp.h>
#include "solve_munkres.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

Rcpp::List solve_munkres_impl(Rcpp::NumericMatrix cost, bool maximize) {
    try {
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
        lap::LapResult result = lap::solve_munkres(cm, maximize);

        const int n = static_cast<int>(result.assignment.size());
        Rcpp::IntegerVector match(n);
        for (int i = 0; i < n; ++i) {
            match[i] = (result.assignment[i] >= 0) ? (result.assignment[i] + 1) : 0;
        }

        double total = compute_total_cost(cost, match);
        return make_result(match, total);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
