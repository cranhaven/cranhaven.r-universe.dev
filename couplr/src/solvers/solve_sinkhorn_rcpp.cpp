// src/solvers/solve_sinkhorn.cpp
// Rcpp wrapper for Sinkhorn solver - calls pure C++ implementation

#include <Rcpp.h>
#include "solve_sinkhorn.h"
#include "../core/lap_error.h"
#include "../core/lap_utils_rcpp.h"

using namespace Rcpp;

// Helper: Convert std::vector<double> to Rcpp::NumericVector
static Rcpp::NumericVector vec_to_rcpp(const std::vector<double>& vec) {
    return Rcpp::NumericVector(vec.begin(), vec.end());
}

// Helper: Convert 2D vector to Rcpp::NumericMatrix
static Rcpp::NumericMatrix matrix_to_rcpp(const std::vector<std::vector<double>>& mat) {
    if (mat.empty()) {
        return Rcpp::NumericMatrix(0, 0);
    }
    const int n = static_cast<int>(mat.size());
    const int m = static_cast<int>(mat[0].size());

    Rcpp::NumericMatrix result(n, m);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            result(i, j) = mat[i][j];
        }
    }
    return result;
}

// Rcpp-exported wrapper for Sinkhorn solver
Rcpp::List solve_sinkhorn_impl(
    Rcpp::NumericMatrix cost,
    double lambda,
    double tol,
    int max_iter,
    Rcpp::Nullable<Rcpp::NumericVector> r_weights,
    Rcpp::Nullable<Rcpp::NumericVector> c_weights
) {
    try {
        // Convert to pure C++ types
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);

        // Convert marginals
        std::vector<double> r_vec, c_vec;

        if (r_weights.isNotNull()) {
            Rcpp::NumericVector rw(r_weights);
            r_vec = std::vector<double>(rw.begin(), rw.end());
        }

        if (c_weights.isNotNull()) {
            Rcpp::NumericVector cw(c_weights);
            c_vec = std::vector<double>(cw.begin(), cw.end());
        }

        // Call pure C++ solver
        lap::SinkhornResult result = lap::solve_sinkhorn(cm, lambda, tol, max_iter, r_vec, c_vec);

        // Convert back to Rcpp
        return List::create(
            _["transport_plan"] = matrix_to_rcpp(result.transport_matrix),
            _["cost"] = result.total_cost,
            _["u"] = vec_to_rcpp(result.u),
            _["v"] = vec_to_rcpp(result.v),
            _["converged"] = result.converged,
            _["iterations"] = result.iterations,
            _["lambda"] = result.lambda
        );

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::List();
}

// Convenience function to round soft assignment to hard assignment
Rcpp::IntegerVector sinkhorn_round_impl(Rcpp::NumericMatrix P) {
    try {
        const int n = P.nrow();
        const int m = P.ncol();

        if (n == 0) {
            return Rcpp::IntegerVector(0);
        }

        // Convert to std::vector<std::vector<double>>
        std::vector<std::vector<double>> mat(n, std::vector<double>(m));
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                mat[i][j] = P(i, j);
            }
        }

        // Call pure C++ rounding
        std::vector<int> match = lap::sinkhorn_round(mat);

        // Convert to 1-based for R
        Rcpp::IntegerVector result(n);
        for (int i = 0; i < n; ++i) {
            result[i] = (match[i] >= 0) ? (match[i] + 1) : 0;
        }

        return result;

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    // Should never reach here
    return Rcpp::IntegerVector();
}
