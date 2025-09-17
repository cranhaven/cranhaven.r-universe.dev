#ifndef UTILS_H
#define UTILS_H

#define ARMA_NO_DEBUG

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
using namespace std;
// [[Rcpp::depends(RcppArmadillo)]]

arma::vec MAD_cpp(arma::mat data);
arma::vec rank_cpp(const vec& x);

#endif
