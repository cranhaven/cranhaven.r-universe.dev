// Thiago de Paula Oliveira
// pearson.cpp — in-place, portable rank-1 update, triangular normalisation
#include <RcppArmadillo.h>
#include <limits>
#include <cmath>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

// X is n x p (double, no NA). Returns p x p Pearson correlation matrix.
// [[Rcpp::export]]
arma::mat pearson_matrix_cpp(SEXP X_) {
  if (!Rf_isReal(X_) || !Rf_isMatrix(X_))
    Rcpp::stop("Numeric double matrix required.");

  const arma::uword n = Rf_nrows(X_);
  const arma::uword p = Rf_ncols(X_);
  if (n < 2 || p < 2) Rcpp::stop("Need >= 2 rows and >= 2 columns.");

  // No-copy
  arma::mat X(REAL(X_), n, p, /*copy_aux_mem*/ false, /*strict*/ true);

  // Column means (1 x p)
  arma::rowvec mu = arma::sum(X, 0) / static_cast<double>(n);

  // XtX := X'X via SYRK (upper triangle); fallback to GEMM
  arma::mat XtX(p, p);
#if defined(ARMA_USE_BLAS)
{
  XtX.zeros();
  const arma::blas_int N = static_cast<arma::blas_int>(p);
  const arma::blas_int K = static_cast<arma::blas_int>(n);
  const double alpha = 1.0, beta = 0.0;
  const char uplo = 'U', trans = 'T';
  arma::blas::syrk<double>(&uplo, &trans, &N, &K,
                           &alpha, X.memptr(), &K,
                           &beta,  XtX.memptr(), &N);
}
#else
XtX = X.t() * X;
#endif

// XtX := XtX - n * mu * mu'
const double scale = -static_cast<double>(n);
#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
for (arma::sword j = 0; j < static_cast<arma::sword>(p); ++j) {
  const arma::uword uj = static_cast<arma::uword>(j);
  const double muj = mu[uj];
  const double fj  = scale * muj;            // = -n * mu_j
  for (arma::uword i = 0; i <= uj; ++i) {
    XtX(i, uj) += fj * mu[i];                // -= n * mu_i * mu_j
  }
}
XtX = arma::symmatu(XtX);                    // mirror to full symmetric

// Convert to covariance in place
XtX /= static_cast<double>(n - 1);

// Standard deviations; clamp tiny negatives
arma::vec s = arma::sqrt(
  arma::clamp(XtX.diag(), 0.0, std::numeric_limits<double>::infinity())
);

// Build correlation in place on upper triangle
arma::vec inv_s = 1.0 / s;
#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
for (arma::sword j = 0; j < static_cast<arma::sword>(p); ++j) {
  const arma::uword uj = static_cast<arma::uword>(j);
  const double sj = inv_s[uj];
  if (!std::isfinite(sj) || sj == 0.0) continue;
  for (arma::uword i = 0; i <= uj; ++i) {
    const double si = inv_s[i];
    if (!std::isfinite(si) || si == 0.0) continue;
    // contiguous writes within column 'uj'
    XtX(i, uj) *= (si * sj);
  }
}
XtX = arma::symmatu(XtX);

// Diagonal to 1; zero-variance rows/cols as NA
arma::uvec zero = arma::find(s == 0.0);
XtX.diag().ones();
for (arma::uword k = 0; k < zero.n_elem; ++k) {
  const arma::uword j = zero[k];
  XtX.row(j).fill(arma::datum::nan);
  XtX.col(j).fill(arma::datum::nan);
  XtX(j, j) = arma::datum::nan;
}

return XtX;  // correlation matrix
}
