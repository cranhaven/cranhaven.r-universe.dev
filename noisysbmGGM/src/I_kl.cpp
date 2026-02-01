#include <RcppArmadillo.h>
#include <string>
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

namespace mjd{

void I_fast_same(const arma::mat& Z_matrix, const arma::umat& ind_all, arma::vec& mask, const arma::vec& Avec, int k) {
  int n = ind_all.n_rows;
  for (int i = 0; i < n; ++i) {
    int j = ind_all(i, 0)-1;
    int l = ind_all(i, 1)-1;
    double val = Z_matrix(k-1, j) * Z_matrix(k-1, l);
    mask(i) += val * Avec(i);
  }
}

void I_fast_diff(const arma::mat& Z_matrix, const arma::umat& ind_all, arma::vec& mask, const arma::vec& Avec, int k, int l) {
  int n = ind_all.n_rows;
  for (int i = 0; i < n; ++i) {
    int j = ind_all(i, 0)-1;
    int m = ind_all(i, 1)-1;
    double val1 = Z_matrix(k-1, j) * Z_matrix(l-1, m);
    double val2 = Z_matrix(k-1, m) * Z_matrix(l-1, j);
    mask(i) += (val1 + val2) * Avec(i);
  }
}
}

// [[Rcpp::export]]

Rcpp::List I_fast(const arma::umat& ind_all, const arma::vec& Avec, const arma::mat& Z_matrix, int k, int l) {
  int n = ind_all.n_rows;
  arma::vec mask(n, arma::fill::zeros);

  if (k == l) {
    mjd::I_fast_same(Z_matrix, ind_all, mask, Avec, k);
  } else {
    mjd::I_fast_diff(Z_matrix, ind_all, mask, Avec, k, l);
  }
  int n_kl=arma::accu(mask);

  return Rcpp::List::create(Rcpp::Named("I_kl") = mask,
                            Rcpp::Named("n_kl") = n_kl);
}


