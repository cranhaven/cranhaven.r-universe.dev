// Thiago de Paula Oliveira
// src-bicor.cpp
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#include <limits>     // for std::numeric_limits
#include <cmath>      // for std::floor, std::ceil
#include <algorithm>  // for std::max
#ifdef _OPENMP
#include <omp.h>
#endif

// only what we use from matrixCorr_detail
#include "matrixCorr_detail.h"
using namespace matrixCorr_detail;
using matrixCorr_detail::standardise_bicor::standardise_bicor_column;
using matrixCorr_detail::standardise_bicor::standardise_bicor_column_weighted;

using namespace Rcpp;
using namespace arma;

// Biweight mid-correlation matrix
//
// Computes the biweight mid-correlation for all column pairs of X.
// No missing/Inf values are allowed in X for this fast variant.
// Set `pearson_fallback=1` (individual) to obtain the hybrid correlation when
// MAD==0 in a column.
// X Numeric matrix (rows = observations, cols = variables).
// c_const Tuning constant multiplying raw MAD (default 9.0).
// maxPOutliers Maximum proportion of low *and* high outliers to permit (>0 to cap).
// If < 1, columns are side-rescaled so that these quantiles (if beyond |u>=1) map to |u|=1.
// pearson_fallback 0 = never (returns NA for MAD==0); 1 = individual fallback when needed;
// 2 = force Pearson for all columns (i.e., ordinary Pearson).
// n_threads Number of OpenMP threads (>=1).
// Symmetric p x p matrix of correlations. Diagonals are 1 where defined, NA if
// the column is degenerate.
// [[Rcpp::export]]
arma::mat bicor_matrix_cpp(const arma::mat &X,
                           const double c_const = 9.0,
                           const double maxPOutliers = 1.0,
                           const int pearson_fallback = 1,
                           const int n_threads = 1) {
  const std::size_t n = X.n_rows, p = X.n_cols;
  if (p == 0 || n < 2) stop("X must have >=2 rows and >=1 column.");
  if (!X.is_finite()) stop("X contains NA/NaN/Inf; please handle missingness upstream.");

  // Standardised columns
  arma::mat Z(n, p, fill::zeros);
  std::vector<bool> col_valid(p, false);

  // Standardise each column in parallel
#ifdef _OPENMP
  omp_set_num_threads(std::max(1, n_threads));
#pragma omp parallel for schedule(static)
#endif
  for (std::ptrdiff_t j = 0; j < static_cast<std::ptrdiff_t>(p); ++j) {
    bool ok = false;
    arma::vec zcol(n, fill::zeros);
    standardise_bicor_column(X.col(j), zcol, pearson_fallback, c_const, maxPOutliers, ok);
    Z.col(j) = zcol;
    col_valid[static_cast<std::size_t>(j)] = ok;
  }

  // Correlation matrix R = Z'Z
  arma::mat R(p, p, fill::zeros);

#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (std::ptrdiff_t j = 0; j < static_cast<std::ptrdiff_t>(p); ++j) {
    for (std::size_t k = static_cast<std::size_t>(j); k < p; ++k) {
      double val = arma::dot(Z.col(j), Z.col(k));
      // Clamp to [-1, 1] if finite
      if (std::isfinite(val)) {
        if (val > 1.0) val = 1.0;
        else if (val < -1.0) val = -1.0;
      }
      R(j, k) = val;
      if (k != static_cast<std::size_t>(j)) R(k, j) = val;
    }
  }

  // Set diagonals
  R.diag().ones();
  return R;
}

// bicor for two vectors (hybrid if one side falls back to Pearson).
// [[Rcpp::export]]
double bicor_vec_cpp(const arma::vec &x, const arma::vec &y,
                     const double c_const = 9.0,
                     const double maxPOutliers = 1.0,
                     const int pearson_fallback = 1) {
  if (x.n_elem != y.n_elem) stop("x and y must have the same length.");
  if (!x.is_finite() || !y.is_finite()) stop("x or y contains NA/NaN/Inf.");

  arma::vec zx(x.n_elem, fill::zeros), zy(y.n_elem, fill::zeros);
  bool okx=false, oky=false;
  standardise_bicor_column(x, zx, pearson_fallback, c_const, maxPOutliers, okx);
  standardise_bicor_column(y, zy, pearson_fallback, c_const, maxPOutliers, oky);

  double val = arma::dot(zx, zy);
  if (std::isfinite(val)) {
    if (val > 1.0) val = 1.0;
    else if (val < -1.0) val = -1.0;
  }
  // If either side is degenerate, return NA
  if (!okx || !oky) return std::numeric_limits<double>::quiet_NaN();
  return val;
}

// [[Rcpp::export]]
arma::mat bicor_matrix_pairwise_cpp(const arma::mat &X,
                                    const double c_const = 9.0,
                                    const double maxPOutliers = 1.0,
                                    const int pearson_fallback = 1,
                                    const int min_n = 5,
                                    const int n_threads = 1) {
  const std::size_t n = X.n_rows, p = X.n_cols;
  if (p == 0 || n < 2) stop("X must have >=2 rows and >=1 column.");

  // initialise with NaN (constructor can't take 'datum::nan')
  arma::mat R(p, p, arma::fill::none);
  R.fill(arma::datum::nan);

#ifdef _OPENMP
  omp_set_num_threads(std::max(1, n_threads));
#pragma omp parallel for schedule(dynamic)
#endif
  for (std::ptrdiff_t j = 0; j < static_cast<std::ptrdiff_t>(p); ++j) {
    for (std::size_t k = static_cast<std::size_t>(j); k < p; ++k) {

      // rows where both columns are finite
      std::vector<arma::uword> idx; idx.reserve(n);
      for (arma::uword i = 0; i < n; ++i) {
        const double a = X(i, j), b = X(i, k);
        if (std::isfinite(a) && std::isfinite(b)) idx.push_back(i);
      }
      if (idx.size() < static_cast<std::size_t>(min_n)) continue;

      arma::uvec I = arma::conv_to<arma::uvec>::from(idx);

      // materialise columns, then subselect with '.elem()'
      arma::vec colj = X.col(j);
      arma::vec colk = X.col(k);
      arma::vec xj = colj.elem(I);
      arma::vec xk = colk.elem(I);

      arma::vec zj(xj.n_elem, arma::fill::zeros), zk(xk.n_elem, arma::fill::zeros);
      bool okj=false, okk=false;
      standardise_bicor_column(xj, zj, pearson_fallback, c_const, maxPOutliers, okj);
      standardise_bicor_column(xk, zk, pearson_fallback, c_const, maxPOutliers, okk);

      double val = arma::datum::nan;
      if (okj && okk) {
        val = arma::dot(zj, zk);
        if (std::isfinite(val)) {
          if (val > 1.0) val = 1.0;
          else if (val < -1.0) val = -1.0;
        }
      }
      R(j,k) = val;
      if (k != static_cast<std::size_t>(j)) R(k,j) = val;
    }
  }

  // diagonals are defined; set to 1
  for (std::size_t j = 0; j < p; ++j) R(j,j) = 1.0;
  return R;
}


// [[Rcpp::export]]
arma::mat bicor_matrix_weighted_cpp(const arma::mat &X,
                                    const arma::vec &w,
                                    const double c_const = 9.0,
                                    const double maxPOutliers = 1.0,
                                    const int pearson_fallback = 1,
                                    const int n_threads = 1) {
  const std::size_t n = X.n_rows, p = X.n_cols;
  if (p == 0 || n < 2) stop("X must have >=2 rows and >=1 column.");
  if (w.n_elem != n) stop("Length of weights `w` must match nrow(X).");
  if (!X.is_finite()) stop("X contains NA/NaN/Inf; use pairwise kernel or clean data.");
  if (!w.is_finite() || arma::any(w < 0)) stop("Weights must be finite and non-negative.");

  arma::mat Z(n, p, arma::fill::zeros);
  std::vector<bool> col_valid(p, false);

#ifdef _OPENMP
  omp_set_num_threads(std::max(1, n_threads));
#pragma omp parallel for schedule(static)
#endif
  for (std::ptrdiff_t j = 0; j < static_cast<std::ptrdiff_t>(p); ++j) {
    bool ok = false;
    arma::vec zcol(n, arma::fill::zeros);
    standardise_bicor_column_weighted(X.col(j), w, zcol, pearson_fallback, c_const, maxPOutliers, ok);
    Z.col(j) = zcol;
    col_valid[static_cast<std::size_t>(j)] = ok;
  }

  arma::mat R(p, p, arma::fill::zeros);
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (std::ptrdiff_t j = 0; j < static_cast<std::ptrdiff_t>(p); ++j) {
    for (std::size_t k = static_cast<std::size_t>(j); k < p; ++k) {
      double val = arma::dot(Z.col(j), Z.col(k));
      if (std::isfinite(val)) {
        if (val > 1.0) val = 1.0;
        else if (val < -1.0) val = -1.0;
      }
      R(j, k) = val;
      if (k != static_cast<std::size_t>(j)) R(k, j) = val;
    }
  }
  R.diag().ones();
  return R;
}

// [[Rcpp::export]]
arma::mat bicor_matrix_weighted_pairwise_cpp(const arma::mat &X,
                                             const arma::vec &w,
                                             const double c_const = 9.0,
                                             const double maxPOutliers = 1.0,
                                             const int pearson_fallback = 1,
                                             const int min_n = 5,
                                             const int n_threads = 1) {
  const std::size_t n = X.n_rows, p = X.n_cols;
  if (p == 0 || n < 2) stop("X must have >=2 rows and >=1 column.");
  if (w.n_elem != n)   stop("Length of weights `w` must match nrow(X).");
  if (!w.is_finite() || arma::any(w < 0)) stop("Weights must be finite and non-negative.");

  arma::mat R(p, p, arma::fill::none);
  R.fill(arma::datum::nan);

#ifdef _OPENMP
  omp_set_num_threads(std::max(1, n_threads));
#pragma omp parallel for schedule(dynamic)
#endif
  for (std::ptrdiff_t j = 0; j < static_cast<std::ptrdiff_t>(p); ++j) {
    for (std::size_t k = static_cast<std::size_t>(j); k < p; ++k) {

      std::vector<arma::uword> idx; idx.reserve(n);
      for (arma::uword i = 0; i < n; ++i) {
        const double a = X(i, j), b = X(i, k);
        if (std::isfinite(a) && std::isfinite(b)) idx.push_back(i);
      }
      if (idx.size() < static_cast<std::size_t>(min_n)) continue;

      arma::uvec I = arma::conv_to<arma::uvec>::from(idx);

      arma::vec colj = X.col(j);
      arma::vec colk = X.col(k);
      arma::vec xj = colj.elem(I);
      arma::vec xk = colk.elem(I);
      arma::vec ww = w.elem(I);

      arma::vec zj(xj.n_elem, arma::fill::zeros), zk(xk.n_elem, arma::fill::zeros);
      bool okj=false, okk=false;
      standardise_bicor_column_weighted(xj, ww, zj, pearson_fallback, c_const, maxPOutliers, okj);
      standardise_bicor_column_weighted(xk, ww, zk, pearson_fallback, c_const, maxPOutliers, okk);

      double val = arma::datum::nan;
      if (okj && okk) {
        val = arma::dot(zj, zk);
        if (std::isfinite(val)) {
          if (val > 1.0) val = 1.0;
          else if (val < -1.0) val = -1.0;
        }
      }
      R(j,k) = val;
      if (k != static_cast<std::size_t>(j)) R(k,j) = val;
    }
  }

  for (std::size_t j = 0; j < p; ++j) R(j,j) = 1.0;
  return R;
}
