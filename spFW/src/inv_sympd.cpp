
#ifndef INV_SYMPD_CPP_
#define INV_SYMPD_CPP_

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::export]]
arma::mat inv_sympd(arma::mat & X) {
  arma::mat Y;
  Y = arma::inv_sympd(X);
  return( Y );
}


#endif
