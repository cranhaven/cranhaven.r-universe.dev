// Partial correlation with sample / ridge / OAS covariance estimators
// Thiago de Paula Oliveira
// partial_correlation.cpp
#include <RcppArmadillo.h>
#include <limits>
#include <cmath>
#include <algorithm>
#include "matrixCorr_detail.h"

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

// functions we use here:
using namespace matrixCorr_detail;
using matrixCorr_detail::linalg::crossprod_no_copy;
using matrixCorr_detail::linalg::subtract_n_outer_mu;
using matrixCorr_detail::linalg::make_pd_inplace;
using matrixCorr_detail::cov_shrinkage::oas_shrink;

// Partial correlation matrix with sample / ridge / OAS covariance
// @param X_ Numeric double matrix (n x p). No NAs.
// @param method One of "sample", "ridge", "oas". Default "oas" (recommended for p >> n).
// @param lambda Ridge penalty for "ridge" method (added to diagonal). Ignored otherwise.
// @param return_cov_precision If TRUE, return covariance and precision matrices.
// @return A list with elements: \code{pcor}, and optionally \code{cov}, \code{precision},
//         \code{method}, \code{lambda}, \code{rho} (for OAS).
// @export
// [[Rcpp::export]]
 Rcpp::List partial_correlation_cpp(SEXP X_,
                                    const std::string method = "oas",
                                    const double lambda = 1e-3,
                                    const bool return_cov_precision = true) {
   if (!Rf_isReal(X_) || !Rf_isMatrix(X_))
     Rcpp::stop("Numeric double matrix required.");

   const arma::uword n = static_cast<arma::uword>(Rf_nrows(X_));
   const arma::uword p = static_cast<arma::uword>(Rf_ncols(X_));
   if (n < 2 || p < 2) Rcpp::stop("Need >= 2 rows and >= 2 columns.");

   // No-copy
   arma::mat X(REAL(X_), n, p, /*copy_aux_mem*/ false, /*strict*/ true);

   // Column means (1 x p)
   arma::rowvec mu = arma::sum(X, 0) / static_cast<double>(n);

   // X'X, then subtract n * mu * mu' (no centred copy), giving centred cross-product.
   arma::mat XtX = crossprod_no_copy(X);
   subtract_n_outer_mu(XtX, mu, static_cast<double>(n));

   // Two scalings of covariance (MLE and unbiased)
   arma::mat cov_mle      = XtX / static_cast<double>(n);
   arma::mat cov_unbiased = XtX / static_cast<double>(n - 1);

   // Choose estimator for Sigma
   arma::mat Sigma;
   double rho = NA_REAL;         // OAS shrinkage weight (if used)
   if (method == "sample") {
     Sigma = std::move(cov_unbiased);
   } else if (method == "ridge") {
     Sigma = std::move(cov_unbiased);
     if (lambda < 0.0) Rcpp::stop("lambda must be non-negative.");
     if (lambda > 0.0) Sigma.diag() += lambda;
   } else if (method == "oas") {
     Sigma = oas_shrink(cov_mle, static_cast<double>(n), rho);
   } else {
     Rcpp::stop("Unknown method: '%s' (use 'sample', 'ridge', or 'oas').", method.c_str());
   }

   // Ensure positive definiteness (adds minimal jitter if needed)
   double jitter = 0.0;
   make_pd_inplace(Sigma, jitter);

   // Precision matrix: Theta = inv(Sigma)
   arma::mat Theta;
   bool inv_ok = arma::inv_sympd(Theta, Sigma); // SPD path
   if (!inv_ok) {
     // Fallback via solve; add tiny jitter if still unstable
     Sigma.diag() += 1e-12;
     inv_ok = arma::solve(Theta, Sigma, arma::eye<arma::mat>(p, p));
     if (!inv_ok) Rcpp::stop("Failed to invert covariance (even after jitter).");
   }

   // pcor_ij = -Theta_ij / sqrt(Theta_ii * Theta_jj)
   arma::vec d = Theta.diag();
   if (d.min() <= 0.0 || !d.is_finite()) {
     Rcpp::stop("Precision diagonal must be positive and finite.");
   }
   arma::vec inv_sqrt = 1.0 / arma::sqrt(d);

   arma::mat pcor(p, p, arma::fill::ones);
#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
   for (arma::sword j = 0; j < static_cast<arma::sword>(p); ++j) {
     const arma::uword uj = static_cast<arma::uword>(j);
     const double sj = inv_sqrt[uj];
     for (arma::uword i = 0; i < uj; ++i) {
       const double si = inv_sqrt[i];
       double val = -Theta(i, uj) * (si * sj);
       pcor(i, uj) = val;
       pcor(uj, i) = val;
     }
     pcor(uj, uj) = 1.0;
   }

   if (return_cov_precision) {
     Rcpp::List out = Rcpp::List::create(
       Rcpp::Named("pcor")      = pcor,
       Rcpp::Named("cov")       = Sigma,
       Rcpp::Named("precision") = Theta,
       Rcpp::Named("method")    = method,
       Rcpp::Named("lambda")    = (method == "ridge" ? lambda : NA_REAL),
       Rcpp::Named("rho")       = (method == "oas"   ? rho    : NA_REAL),
       Rcpp::Named("jitter")    = jitter
     );
     return out;
   } else {
     return Rcpp::List::create(Rcpp::Named("pcor") = pcor);
   }
 }
