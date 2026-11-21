#include <RcppArmadillo.h>
#include "loglik.h"
#include "utils.h"

#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

using namespace Rcpp;

arma::colvec loglik_j(arma::mat& W, arma::mat& X, arma::colvec& y,
                      arma::colvec gamma, double kappa_j_1, double kappa_j,
                      arma::colvec beta_j, double sigma_j, double rho_j,
                      int boundary) {
  int n_elem = y.size();
  arma::colvec res(n_elem), low(n_elem), high(n_elem), part1(n_elem), part2(n_elem), part3(n_elem);

  res = y - X * beta_j;
  low = kappa_j_1 - W * gamma;
  high = kappa_j - W * gamma;

  part1 = 1.0 / sigma_j * arma::normpdf(res / sigma_j);

  if (boundary == 1)
    part2.ones();
  else
    part2 = arma::normcdf((sigma_j * high - rho_j * res) / (sigma_j * sqrt(1.0 - pow(rho_j, 2))));

  if (boundary == -1)
    part3.zeros();
  else
    part3 = arma::normcdf((sigma_j * low - rho_j * res) / (sigma_j * sqrt(1.0 - pow(rho_j, 2))));

  return log(part1) + log(part2 - part3);
}


// [[Rcpp::export]]
arma::colvec loglik_cpp(NumericVector& theta, arma::field<arma::mat>& W,
                        arma::field<arma::mat>& X, arma::field<arma::colvec>& Y,
                        arma::colvec& weights, int nReg, int nThreads) {

#ifdef _OPENMP
  omp_set_num_threads(nThreads);
#endif

  int boundary;
  int min_z = 1;
  int max_z = nReg;
  List theta_list;
  Theta theta_j;
  arma::field<arma::colvec> ll_j(nReg);
  arma::colvec ll;

  theta_list = opsr_prepare_coefs(theta, nReg);
  // theta_list is an Rcpp::List which is badly handled in openmp
  // Theta is a typedef which contains only arma or base types
  Theta* theta_array = make_theta_array(theta_list);

#ifdef _OPENMP
#pragma omp parallel for private(theta_j, boundary)
#endif
  for (int j = 0; j < nReg; j++) {
    theta_j = theta_array[j];
    boundary = (j + 1 == min_z) ? -1 : (j + 1 == max_z) ? 1 : 0;  // j + 1!

    ll_j[j] = loglik_j(W[j], X[j], Y[j], theta_j.gamma, theta_j.kappa_j_1,
                       theta_j.kappa_j, theta_j.beta_j, theta_j.sigma_j,
                       theta_j.rho_j, boundary);
  }

  for (int j = 0; j < nReg; j++) {
    ll = arma::join_cols(ll, ll_j[j]);
  }

  // deallocate!
  free_theta_array(theta_array);

  // element-wise multiplication
  return ll % weights;
}

