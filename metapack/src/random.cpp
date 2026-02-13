#include <stdio.h>
#include <cmath>
#include <Rmath.h>
#include <RcppArmadillo.h>
#include <Rdefines.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "random.h"
// [[Rcpp::depends(RcppArmadillo, RcppProgress)]]

using namespace RNG;

arma::mat RNG::rwish(arma::mat S, double v) {
  int p = S.n_rows;
  arma::mat R = arma::chol(S);
  arma::mat A(p,p, arma::fill::zeros);
  for (int i = 0; i < p-1; ++i) {
    for (int j = i+1; j < p; ++j) {
      A(j,i) = arma::randn<double>();
    }
  }
  
  for (int i = 0; i < p; ++i) {
    A(i,i) = std::sqrt(::Rf_rchisq(v-static_cast<double>(i)));
  }
  return R.t() * A * A.t() * R;
}

arma::mat RNG::rwish(arma::mat &S, double v) {
  int p = S.n_rows;
  arma::mat R = arma::chol(S);
  arma::mat A(p,p, arma::fill::zeros);
  for (int i = 0; i < p-1; ++i) {
    for (int j = i+1; j < p; ++j) {
      A(j,i) = arma::randn<double>();
    }
  }
  
  for (int i = 0; i < p; ++i) {
    A(i,i) = std::sqrt(::Rf_rchisq(v-static_cast<double>(i)));
  }
  return R.t() * A * A.t() * R;
}

arma::mat RNG::riwish(arma::mat S, double v) {
  arma::mat Sinv = arma::inv_sympd(S);
  int p = S.n_rows;
  arma::mat R = arma::chol(Sinv);
  arma::mat A(p, p, arma::fill::zeros);
  for (int i = 0; i < p-1; ++i) {
    for (int j = i+1; j < p; ++j) {
      A(j,i) = arma::randn<double>();
    }
  }
  
  for (int i = 0; i < p; ++i) {
    A(i,i) = std::sqrt(::Rf_rchisq(v-static_cast<double>(i)));
  }
  arma::mat out = R.t() * A * A.t() * R;
  return arma::inv_sympd(out);
}

arma::mat RNG::riwish(arma::mat &S, double v) {
  arma::mat Sinv = arma::inv_sympd(S);
  int p = S.n_rows;
  arma::mat R = arma::chol(Sinv);
  arma::mat A(p, p, arma::fill::zeros);
  for (int i = 0; i < p-1; ++i) {
    for (int j = i+1; j < p; ++j) {
      A(j,i) = arma::randn<double>();
    }
  }
  
  for (int i = 0; i < p; ++i) {
    A(i,i) = std::sqrt(::Rf_rchisq(v-static_cast<double>(i)));
  }
  arma::mat out = R.t() * A * A.t() * R;
  return arma::inv_sympd(out);
}
