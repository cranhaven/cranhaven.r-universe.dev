#ifndef KMA_H
#define KMA_H

#include <Rcpp.h>

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
double GeodesicQuaternionDistance(
    const Rcpp::NumericMatrix &M1,
    const Rcpp::NumericMatrix &M2,
    const unsigned int index1,
    const unsigned int index2
);

// [[Rcpp::export]]
Rcpp::NumericMatrix RegularizeGrid(
    const Rcpp::NumericVector &grid,
    const Rcpp::NumericMatrix &values,
    const double gridLowerBound,
    const double gridUpperBound,
    const unsigned int numberOfPoints
);

// [[Rcpp::export]]
Rcpp::NumericMatrix GetGeodesicMean(const Rcpp::NumericMatrix &values);

#endif /* KMA_H */
