// src/solvers/solve_jv_duals_lazy_rcpp.cpp
// Rcpp wrapper for the lazy (on-demand) JV duals solver, returning the u/v the
// dense wrapper returns so verify_assignment() reads the same shape on either
// cost source. The specification names a built-in metric or carries a user's
// distance function.

#include <Rcpp.h>
#include "solve_jv_duals_impl.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

#include <variant>

Rcpp::List solve_jv_duals_lazy_impl(Rcpp::NumericMatrix left_mat,
                                    Rcpp::NumericMatrix right_mat,
                                    SEXP metric,
                                    Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                    double max_distance, Rcpp::List calipers,
                                    bool maximize) {
    try {
        const LazySource source = rcpp_lazy_source(left_mat, right_mat, metric, inv_cov,
                                                   max_distance, calipers, maximize);

        // The total is computed from the original (unnegated) distances inside
        // the solve; there is no materialized matrix to recompute it from, which
        // is the point of the lazy path.
        const lap::DualResult result = std::visit([](const auto& cm) {
            return lap::solve_jv_duals_prepared(cm);
        }, source);

        const int n = static_cast<int>(result.solution.assignment.size());
        Rcpp::IntegerVector match(n);
        for (int i = 0; i < n; ++i) {
            match[i] = (result.solution.assignment[i] >= 0)
                           ? (result.solution.assignment[i] + 1)
                           : 0;
        }

        return Rcpp::List::create(
            Rcpp::Named("match") = match,
            Rcpp::Named("total_cost") = result.solution.total_cost,
            Rcpp::Named("u") = Rcpp::NumericVector(result.u.begin(), result.u.end()),
            Rcpp::Named("v") = Rcpp::NumericVector(result.v.begin(), result.v.end())
        );

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
