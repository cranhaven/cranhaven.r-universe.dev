// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector eigenValuesRcpp(arma::mat matrix)
{
  
  arma::vec eigenValues;
  
  eig_sym(eigenValues, matrix);
  
  return as<NumericVector>(wrap(eigenValues));
  
}

NumericVector eigenValuesRcppConversion(NumericMatrix matrix) 
{
  return eigenValuesRcpp(as<arma::mat>(matrix));
}
