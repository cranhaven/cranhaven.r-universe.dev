#ifndef PDIST_H
#define PDIST_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector pdist_rcpp(NumericVector q, NumericMatrix par, std::string dist);

#endif
