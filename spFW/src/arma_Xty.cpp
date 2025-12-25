
#ifndef ARMA_XTY_CPP_
#define ARMA_XTY_CPP_

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::export]]

arma::mat arma_Xty(arma::mat & X, arma::vec & y){
  arma::vec Xty;
  Xty = X.t() * y;
  return( Xty );
}


#endif
