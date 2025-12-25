
#ifndef ARMA_XY_CPP_
#define ARMA_XY_CPP_

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::export]]

arma::mat arma_Xy(arma::mat & X, arma::vec & y){
  arma::vec Xy;
  Xy = X * y;
  return( Xy );
}



#endif
