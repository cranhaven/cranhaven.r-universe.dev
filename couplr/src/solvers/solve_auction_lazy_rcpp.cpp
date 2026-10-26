// src/solvers/solve_auction_lazy_rcpp.cpp
// Rcpp wrapper for the lazy (on-demand) Auction solver. The specification names
// a built-in metric or carries a user's distance function, and the same bidding
// core runs over whichever cost source that is.

#include <Rcpp.h>
#include "solve_auction_core.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

#include <variant>

Rcpp::List solve_auction_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                   SEXP metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                   double max_distance, Rcpp::List calipers,
                                   bool maximize,
                                   Rcpp::Nullable<double> eps) {
    try {
        const LazySource source = rcpp_lazy_source(left_mat, right_mat, metric, inv_cov,
                                                   max_distance, calipers, maximize);

        const double eps_in = eps.isNotNull() ? Rcpp::as<double>(eps.get())
                                              : std::numeric_limits<double>::quiet_NaN();
        const lap::LapResult result = std::visit([eps_in](const auto& cm) {
            return lap::auction_detail::auction_core_lazy(
                cm, lap::auction_detail::terminal_epsilon_options(eps_in));
        }, source);

        std::vector<int> match(result.assignment.size());
        for (size_t i = 0; i < result.assignment.size(); ++i) {
            match[i] = (result.assignment[i] >= 0) ? (result.assignment[i] + 1) : 0;
        }
        return make_result(match, result.total_cost);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
