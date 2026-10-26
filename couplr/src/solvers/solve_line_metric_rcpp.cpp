// src/solvers/solve_line_metric.cpp
// Rcpp wrapper for line_metric solver - calls pure C++ implementation

#include <Rcpp.h>
#include "solve_line_metric.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

// Helper: Convert lap::LapResult to Rcpp::List (with 1-based indexing)
// For line_metric, we need to compute the cost using the original x/y vectors
static Rcpp::List lap_result_to_rcpp(const lap::LapResult& result,
                                      const Rcpp::NumericVector& x,
                                      const Rcpp::NumericVector& y,
                                      const std::string& cost_type,
                                      bool maximize) {
    const int n = static_cast<int>(result.assignment.size());

    // Convert 0-based to 1-based
    Rcpp::IntegerVector match(n);
    for (int i = 0; i < n; ++i) {
        match[i] = (result.assignment[i] >= 0) ? (result.assignment[i] + 1) : 0;
    }

    // Recompute total cost using original vectors for verification
    // (line_metric doesn't use a cost matrix, so we compute directly)
    double total = 0.0;
    for (int i = 0; i < n; ++i) {
        if (result.assignment[i] >= 0) {
            double diff = x[i] - y[result.assignment[i]];
            double abs_diff = std::abs(diff);

            // Match cost type from pure implementation
            std::string lower_cost = cost_type;
            std::transform(lower_cost.begin(), lower_cost.end(), lower_cost.begin(),
                         [](unsigned char c) { return std::tolower(c); });

            if (lower_cost == "l2" || lower_cost == "sq" ||
                lower_cost == "squared" || lower_cost == "quadratic") {
                total += abs_diff * abs_diff;
            } else {
                total += abs_diff;
            }
        }
    }

    // total is the actual matched cost sum; for maximize this is already the
    // maximum achievable (the assignment is the anti-monotone one), so it is
    // returned as-is rather than negated.
    (void)maximize;
    return make_result(match, total);
}

// Rcpp-exported wrapper (called by router/exposed via rcpp_interface.cpp)
Rcpp::List solve_line_metric_impl(const Rcpp::NumericVector& x,
                                  const Rcpp::NumericVector& y,
                                  const std::string& cost,
                                  bool maximize) {
    try {
        // Convert to std::vector
        std::vector<double> x_vec(x.begin(), x.end());
        std::vector<double> y_vec(y.begin(), y.end());

        // Call pure C++ solver
        lap::LapResult result = lap::solve_line_metric(x_vec, y_vec, cost, maximize);

        // Convert back to Rcpp
        return lap_result_to_rcpp(result, x, y, cost, maximize);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::List();
}

// Public interface (called from rcpp_interface.cpp)
Rcpp::List lap_solve_line_metric(const Rcpp::NumericVector& x,
                                 const Rcpp::NumericVector& y,
                                 const std::string& cost,
                                 bool maximize) {
    return solve_line_metric_impl(x, y, cost, maximize);
}
