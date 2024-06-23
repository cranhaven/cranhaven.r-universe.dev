#ifndef AUTOCART_SPATIALMETHODS_H
#define AUTOCART_SPATIALMETHODS_H

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

double euclidDistance(double x1, double y1, double x2, double y2);
double moranI(NumericVector response, NumericMatrix weights);
double moranIVariance(NumericVector response, NumericMatrix weights);
double gearyC(NumericVector response, NumericMatrix weights);
NumericMatrix getInvWeights(NumericMatrix locations, bool islonglat, int power);

// Convex hull algorithms
bool compareNumericVector(NumericVector v1, NumericVector v2);
List jarvisConvexHull(NumericMatrix locations);
double getAreaOfConvexHull(List convexHull);

// Parallelized algorithms
double moranIParallel(NumericVector response, NumericMatrix weights);

#endif
