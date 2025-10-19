// Thiago de Paula Oliveira
#include <RcppArmadillo.h>
#include <cmath>
#include "matrixCorr_detail.h"
using namespace Rcpp;

// only what we use from matrixCorr_detail
using namespace matrixCorr_detail;
using matrixCorr_detail::moments::col_means_vars_pop;
using matrixCorr_detail::moments::cov_xy_pop_manual;
using matrixCorr_detail::moments::cov_xy_pop_arma;
using matrixCorr_detail::ccc_bits::ccc_from_stats_via_r;
using matrixCorr_detail::norm1::qnorm01;
using matrixCorr_detail::ccc_se::se_delta;
using matrixCorr_detail::fisherz::ci_from_z;
using matrixCorr_detail::symm::put;

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat ccc_cpp(const arma::mat& X) {
  const int n = X.n_rows;
  const int p = X.n_cols;
  arma::mat result(p, p, arma::fill::eye);

  arma::vec means(p), vars(p);
  // Precompute means and population variances (identical scaling)
  col_means_vars_pop(X, means, vars);

#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
  for (int i = 0; i < p; ++i) {
    for (int j = i + 1; j < p; ++j) {
      const arma::vec x = X.col(i);
      const arma::vec y = X.col(j);

      const double mean_x = means[i];
      const double mean_y = means[j];
      const double var_x  = vars[i];
      const double var_y  = vars[j];

      // population covariance via manual loop
      const double cov_xy = cov_xy_pop_manual(x, y, mean_x, mean_y);

      // CCC with identical algebraic order (r -> sxy -> p)
      const double pij = ccc_from_stats_via_r(mean_x, mean_y, var_x, var_y, cov_xy);

      put(result, i, j, pij);
    }
  }

  return result;
}

// [[Rcpp::export]]
List ccc_with_ci_cpp(const arma::mat& X, double conf_level = 0.95) {
  const int n = X.n_rows;
  const int p = X.n_cols;

  arma::mat est(p, p, arma::fill::eye);
  arma::mat lwr(p, p, arma::fill::zeros);
  arma::mat upr(p, p, arma::fill::zeros);

  arma::vec means(p), vars(p);
  // Precompute means and population variances
  col_means_vars_pop(X, means, vars);

  const double alpha  = 1.0 - conf_level;
  const double zcrit  = qnorm01(1.0 - alpha / 2.0);

#ifdef _OPENMP
#pragma omp parallel for schedule(static)
#endif
  for (int i = 0; i < p; ++i) {
    for (int j = i + 1; j < p; ++j) {
      const arma::vec x = X.col(i);
      const arma::vec y = X.col(j);

      const double mean_x = means[i], mean_y = means[j];
      const double var_x  = vars[i],  var_y  = vars[j];

      // population covariance via arma::cov scaled to population
      const double cov_xy = cov_xy_pop_arma(x, y);

      // CCC estimate
      const double p_val = ccc_from_stats_via_r(mean_x, mean_y, var_x, var_y, cov_xy);

      // components for delta-method SE
      const double r   = cov_xy / std::sqrt(var_x * var_y);
      const double u   = (mean_y - mean_x) / std::pow(var_x * var_y, 0.25);
      const double sep = se_delta(r, p_val, u, n);                 // SE on p
      const double set = sep / (1.0 - p_val * p_val);              // SE on Fisher z

      double lci = 0.0, uci = 0.0;
      ci_from_z(p_val, set, zcrit, lci, uci);                      // Fisher-z CI

      put(est, i, j, p_val);
      put(lwr, i, j, lci);
      put(upr, i, j, uci);
    }
  }

  return List::create(
    Named("est")    = est,
    Named("lwr.ci") = lwr,
    Named("upr.ci") = upr
  );
}

// [[Rcpp::export]]
int openmp_threads() {
  int n = 1;
#ifdef _OPENMP
  n = omp_get_max_threads();
#endif
  return n;
}
