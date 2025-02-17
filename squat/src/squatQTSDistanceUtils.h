#ifndef SQUATQTSDISTANCEUTILS_H
#define SQUATQTSDISTANCEUTILS_H

#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix GetCostMatrix(
    const Rcpp::DataFrame &qts1,
    const Rcpp::DataFrame &qts2
);

#endif /* SQUATQTSDISTANCEUTILS_H */
