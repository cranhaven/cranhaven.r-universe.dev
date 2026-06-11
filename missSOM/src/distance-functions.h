#ifndef DISTANCE_H
#define DISTANCE_H

#include <R.h>
#include <Rcpp.h>
#include <cfloat>

#include "missSOM_types.h"

#define EPS 1e-8 /* relative test of equality of distances */

Rcpp::XPtr<DistanceFunctionPtr> CreateStdDistancePointer(int type);
Rcpp::XPtr<DistanceFunctionPtr> CreateNonNaNDistanceFunctionXPtr(int type);

Rcpp::ExpressionVector CreateStdDistancePointers(const Rcpp::IntegerVector &types);

std::vector<DistanceFunctionPtr> GetDistanceFunctions(const Rcpp::ExpressionVector &distanceFunctionXPtrs);

inline DistanceFunctionPtr AsDistanceFunctionPtr(const Rcpp::XPtr<DistanceFunctionPtr> &xptr) {
  return (*xptr);
}

void FindBestMatchingUnit(
  double *object,
  double *codes,
  int numCodes,
  int totalVars,
  const std::vector<DistanceFunctionPtr> &distanceFunctions,
  int &index,
  double &distance);
  
// [[Rcpp::export]]
Rcpp::NumericVector ObjectDistances(Rcpp::NumericMatrix data,
            Rcpp::ExpressionVector distanceFunctions);
            
double EuclideanDistance(double *dataVector, double *codeVector, int n);
double SumOfSquaresDistance(double *dataVector, double *codeVector, int n);
double TanimotoDistance(double *dataVector, double *codeVector, int n);
double ManhattanDistance(double *dataVector, double *codeVector, int n);


#endif
