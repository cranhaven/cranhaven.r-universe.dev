// Thiago de Paula Oliveira

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]
#include <RcppArmadillo.h>
#include <cmath>
#ifdef _OPENMP
#include <omp.h>
#endif

// Pairwise unbiased distance correlation (U-statistic)
// Székely, Rizzo & Bakirov, 2007
// [[Rcpp::export]]
double ustat_dcor(const arma::vec& x, const arma::vec& y) {
  const int n = x.n_elem;
  if (n < 4) Rcpp::stop("Sample size must be at least 4 for unbiased dCor");

  arma::mat Dx(n, n, arma::fill::zeros);
  arma::mat Dy(n, n, arma::fill::zeros);

  // pairwise distances
  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      const double dx = std::abs(x[i] - x[j]);
      const double dy = std::abs(y[i] - y[j]);
      Dx(i, j) = Dx(j, i) = dx;
      Dy(i, j) = Dy(j, i) = dy;
    }
  }

  // row sums and grand sums
  const arma::vec Rx = arma::sum(Dx, 1);
  const arma::vec Ry = arma::sum(Dy, 1);
  const double Sx = arma::accu(Dx);
  const double Sy = arma::accu(Dy);

  // unbiased inner product
  double XY = 0.0, X2 = 0.0, Y2 = 0.0;

  const double inv_nm2 = 1.0 / ( (double)(n - 2) );
  const double add_x = Sx / ( (double)( (n - 1) * (n - 2) ) );
  const double add_y = Sy / ( (double)( (n - 1) * (n - 2) ) );

  for (int i = 0; i < n; ++i) {
    const double rxi = Rx[i];
    const double ryi = Ry[i];
    for (int j = i + 1; j < n; ++j) {
      // U-centering for the (i,j) off-diagonal element
      const double ax = Dx(i, j) - (rxi + Rx[j]) * inv_nm2 + add_x;
      const double ay = Dy(i, j) - (ryi + Ry[j]) * inv_nm2 + add_y;

      XY += ax * ay;
      X2 += ax * ax;
      Y2 += ay * ay;
    }
  }

  // unbiased scaling 2 / (n * (n - 3))
  const double scale = 2.0 / ( (double) n * (double)(n - 3) );
  XY *= scale;
  X2 *= scale;
  Y2 *= scale;

  if (X2 <= 0.0 || Y2 <= 0.0) return NA_REAL;

  const double denom = std::sqrt(X2 * Y2);
  double dcor = XY / denom;

  // numerical clamp into [0, 1]
  if (!std::isfinite(dcor)) return NA_REAL;
  if (dcor < 0.0) dcor = 0.0;
  if (dcor > 1.0) dcor = 1.0;

  return dcor;
}

// Full matrix of unbiased distance correlations
// [[Rcpp::export]]
arma::mat ustat_dcor_matrix_cpp(const arma::mat& X) {
  const int p = X.n_cols;
  arma::mat R(p, p, arma::fill::eye);

  // Parallelize over upper-triangular column pairs
#ifdef _OPENMP
#pragma omp parallel for schedule(dynamic)
#endif
  for (int j = 1; j < p; ++j) {
    for (int i = 0; i < j; ++i) {
      const arma::vec xi = X.col(i);
      const arma::vec xj = X.col(j);
      const double d = ustat_dcor(xi, xj);
      R(i, j) = d;
      R(j, i) = d;
    }
  }
  return R;
}
