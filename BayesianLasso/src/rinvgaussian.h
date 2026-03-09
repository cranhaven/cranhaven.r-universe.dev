 
#ifndef RINVGAUSSIAN_H
#define RINVGAUSSIAN_H
 
#include <RcppArmadillo.h>
 
arma::vec rinvgaussian_c(arma::vec vmu, arma::vec vlambda);
arma::vec rrinvgauss(arma::vec vmu, arma::vec vlambda);
arma::vec rinvgaussian_slice_c(arma::vec vx, arma::vec vmu, arma::vec vlambda); 
  
#endif
