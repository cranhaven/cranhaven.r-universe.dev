// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericMatrix choleskyDecompositionRcpp(arma::mat matrix)
{
  
  return as<NumericMatrix>(wrap(chol(matrix)));
  
}

NumericMatrix choleskyDecompositionRcppConversion(NumericMatrix matrix) 
{
  return choleskyDecompositionRcpp(as<arma::mat>(matrix));
}
