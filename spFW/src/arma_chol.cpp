
#ifndef ARMA_CHOL_CPP_
#define  ARMA_CHOL_CPP_

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::export]]
arma::mat arma_chol(arma::mat & X) {
  arma::mat Y;
  Y = arma::chol(X,"lower");
  return( Y );
}


#endif
