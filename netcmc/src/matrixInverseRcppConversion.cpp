// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericMatrix matrixInverseRcpp(arma::mat matrix) 
{
  return as<NumericMatrix>(wrap(inv(matrix)));
}

NumericMatrix matrixInverseRcppConversion(NumericMatrix matrix) 
{
  return matrixInverseRcpp(as<arma::mat>(matrix));
}

