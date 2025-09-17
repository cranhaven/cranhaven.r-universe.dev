#ifndef FASTCORKENDALL_H
#define FASTCORKENDALL_H

#define ARMA_NO_DEBUG

#include <RcppArmadillo.h>
//#include "utils.h"
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;
using namespace std;

// functions to be used within C++
double fastCorKendall(const arma::vec& x, const arma::vec& y, const arma::uword & n);

#endif
