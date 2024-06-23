#ifndef AUTOCART_SPLITTINGMETHODS_H
#define AUTOCART_SPLITTINGMETHODS_H

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include "AutoTree.h"

using namespace Rcpp;
using namespace arma;

// Helper
NumericMatrix getWeightsMatrix(NumericMatrix locations, int distpower, bool islonglat, double spatialBandwidth, SpatialWeights::Type spatialWeightsType, bool useParallelCalculations);
NumericMatrix getDefaultWeightsMatrix(NumericMatrix locations, int distpower, bool islonglat, double spatialBandwidth, bool useParallelCalculations);
NumericMatrix getGaussianWeightsMatrix(NumericMatrix locations, bool islonglat, double spatialBandwidth, bool useParallelCalculations);

// Continuous
NumericVector continuousGoodnessByVariance(NumericVector response, NumericVector x_vector, NumericVector wt, int minbucket, bool useParallelCalculations);
NumericVector continuousGoodnessByAutocorrelation(NumericVector response, NumericVector x_vector, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericVector wt, int minbucket, int distpower, bool islonglat, bool useGearyC, bool saddlepointApproximation, double spatialBandwidth, SpatialWeights::Type spatialWeightsType, bool useParallelCalculations);
NumericVector continuousGoodnessBySize(NumericVector x_vector, NumericMatrix locations, NumericMatrix distanceMatrix, NumericVector wt, int minbucket, bool islonglat, bool useParallelCalculations);

// Categorical
NumericVector categoricalGoodnessByVariance(NumericVector response, IntegerVector x_vector, NumericVector wt, int minbucket, bool useParallelCalculations);
NumericVector categoricalGoodnessByAutocorrelation(NumericVector response, IntegerVector x_vector, NumericMatrix locations, NumericMatrix spatialWeightsMatrix, NumericVector wt, int minbucket, int distpower, bool islonglat, bool useGearyC, bool saddlepointApproximation, double spatialBandwidth, SpatialWeights::Type spatialWeightsType, bool useParallelCalculations);
NumericVector categoricalGoodnessBySize(IntegerVector x_vector, NumericMatrix locations, NumericMatrix distanceMatrix, NumericVector wt, int minbucket, bool islonglat, bool useParallelCalculations);


// Testing ground beta functions that were unused
NumericVector continuousGoodnessBySizeConvexHull(NumericMatrix locations, int minbucket);
// NumericVector continuousGoodnessByDensity(NumericMatrix nodeLocations, NumericMatrix globalLocations, int minbucket);
// NumericVector continuousGoodnessBySeparation(NumericMatrix locations, NumericMatrix distanceMatrix, int minbucket, bool islonglat);
// NumericVector continuousGoodnessBySeparation(const arma::mat locations, int n, int minbucket);
// NumericVector continuousGoodnessBySeparationOld(NumericMatrix locations, NumericMatrix distanceMatrix, int minbucket, bool islonglat);


#endif
