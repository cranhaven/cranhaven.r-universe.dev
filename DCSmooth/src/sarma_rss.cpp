// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "ar_coef.h"
using namespace Rcpp;

// arma::vec ar_coef(const arma::vec, const arma::vec, const double, const int);
// arma::vec ar_coef(arma::vec, arma::vec, double, int);

// //[[Rcpp::export]]
// NumericVector ARMA_to_AR(const NumericVector phi,
//                          const NumericVector psi,
//                          const int K)
// {
//   Rcpp::Function toAR("ARMAtoMA");
//   
//   return toAR(Named("ar", -phi), Named("ma", -psi), Named("lag.max") = K);
// }

//[[Rcpp::export]]
NumericVector ARMA_to_AR(const arma::vec phi,
                     const arma::vec psi,
                     const int K)
{
  Rcpp::Function toAR("ARMAtoMA");
  
  Rcpp::NumericVector AR_value = toAR(Named("ar", -psi), Named("ma", -phi),
                                      Named("lag.max") = K);
  AR_value.push_front(1.0);

  return AR_value;
}

// [[Rcpp::export]]
double sarma_rss(const arma::vec theta,
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

  arma::colvec phi1, psi1, phi2, psi2;
  if (nar1 > 0) {
    phi1 = theta.subvec(0, nar1 - 1);
  }
  else {
    phi1.zeros(1);
  }
  if (nar2 > 0) {
    phi2 = theta.subvec(nar1, nar12 - 1);
  }
  else {
    phi2.zeros(1);
  }
  if (nma1 > 0) {
    psi1 = theta.subvec(nar12, nar12 + nma1 - 1);
  }
  else {
    psi1.zeros(1);
  }

  if (nma2 > 0) {
    psi2 = theta.subvec(nar12 + nma1, nar12 + nma12 - 1);
  }
  else {
    psi2.zeros(1);
  }

  arma::vec bk1 = ar_coef(phi1, psi1, 0, Kx);
  arma::vec bk2 = ar_coef(phi2, psi2, 0, Kt);
  
  // arma::vec bk1 = ARMA_to_AR(phi1, psi1, Kx);
  // arma::vec bk2 = ARMA_to_AR(phi2, psi2, Kt);

  // calculating the RSS
  arma::mat e_est(n1, n2);
  arma::mat xi_est(n2, n1);
  // 1st stage
  arma::mat tR_mat = R_mat.t();
  for (int j = 0; j < n2; j++) {
    if (j < Kt) {
      xi_est.row(j) = bk2.subvec(0, j).t() * reverse(tR_mat.rows(0, j));
    } else {
      xi_est.row(j) = bk2.t() * reverse(tR_mat.rows((j - Kt), j));
    }
  }
  
  //2nd stage
  arma::mat txi_est = xi_est.t();
  for (int i = 0; i < n1; i++) {
    if (i < Kx) {
      e_est.row(i) = bk1.subvec(0, i).t() * reverse(txi_est.rows(0, i));
    }
    else {
      e_est.row(i) = bk1.t() * reverse(txi_est.rows((i - Kx), i));
    }
  }

  double rss = arma::accu(arma::pow(e_est, 2));
  return rss;
}
