// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "ar_coef.h"
using namespace Rcpp;

// arma::vec ar_coef(const arma::vec, const arma::vec, const double, const int);
// arma::vec ar_coef(arma::vec, arma::vec, double, int);

// [[Rcpp::export]]
double sfarima_rss(const arma::vec theta,
                   const arma::mat R_mat,
                   const List model_order)
{
  int nar1 = as<NumericVector>(model_order["ar"])(0);
  int nar2 = as<NumericVector>(model_order["ar"])(1);
  int nma1 = as<NumericVector>(model_order["ma"])(0);
  int nma2 = as<NumericVector>(model_order["ma"])(1);
  int nar12 = nar1 + nar2;
  int nma12 = nma1 + nma2;
  
  int n1 = R_mat.n_rows;
  int n2 = R_mat.n_cols;
  int Kx = std::min(50, n1);
  int Kt = std::min(50, n2);
  
  double d1 = theta(0);
  double d2 = theta(1);
  arma::colvec phi1, psi1, phi2, psi2;
  if (nar1 > 0) {
    phi1 = theta.subvec(2, nar1 + 1);
  } else {
    phi1.zeros(1);
  }
  if (nar2 > 0) {
    phi2 = theta.subvec(nar1 + 2, nar12 + 1);
  } else {
    phi2.zeros(1);
  }
  if (nma1 > 0) {
    psi1 = theta.subvec(nar12 + 2, nar12 + nma1 + 1);
  } else {
    psi1.zeros(1);
  }
  
  if (nma2 > 0) {
    psi2 = theta.subvec(nar12 + nma1 + 2, nar12 + nma12 + 1);
  } else {
    psi2.zeros(1);
  }
  
  arma::rowvec bk1 = ar_coef(phi1, psi1, d1, Kx).t();
  arma::rowvec bk2 = ar_coef(phi2, psi2, d2, Kt).t();

  // calculating the RSS
  arma::mat e_est(n1, n2);
  arma::mat xi_est(n2, n1);
  // 1st stage
  arma::mat tR_mat = R_mat.t();
  for (int j = 0; j < n2; j++) {
    if (j < Kt) {
      xi_est.row(j) = bk2.subvec(0, j) * reverse(tR_mat.rows(0, j));
    } else {
      xi_est.row(j) = bk2 * reverse(tR_mat.rows((j - Kt), j));
    }
  }
  //2nd stage
  arma::mat txi_est = xi_est.t();
  for (int i = 0; i < n1; i++) {
    if (i < Kx) {
      e_est.row(i) = bk1.subvec(0, i) * reverse(txi_est.rows(0, i));
    } else {
      e_est.row(i) = bk1 * reverse(txi_est.rows((i - Kx), i));
    }
  }

  double rss = arma::accu(arma::pow(e_est, 2));
  return rss;
}
