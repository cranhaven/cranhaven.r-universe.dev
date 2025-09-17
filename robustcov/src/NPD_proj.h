#ifndef NPD_PROJ_H
#define NPD_PROJ_H
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;
mat ProjPattern(mat X, const mat X0, const vec un);
mat ProjPSD(const mat R, const int n, const float eigenTol);
arma::mat nearPPSD(arma::mat X, const float eigenTol, const float convTol, 
             const float psdTol, const int maxit);
#endif
