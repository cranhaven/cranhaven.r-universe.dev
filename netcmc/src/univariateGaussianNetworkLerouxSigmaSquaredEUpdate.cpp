#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

double univariateGaussianNetworkLerouxSigmaSquaredEUpdate(NumericMatrix standardizedX,
                                                          NumericVector y,
                                                          NumericMatrix spatialAssignment,
                                                          NumericMatrix W,
                                                          NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                          NumericMatrix WInTripletForm,
                                                          const int numberOfRowsInX,
                                                          const int numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                          const int numberOfRowsInWTripletForm,
                                                          NumericVector beta,
                                                          NumericVector spatialRandomEffects,
                                                          NumericVector uRandomEffects,
                                                          double a3,
                                                          double b3)
{
  
  NumericVector XBeta = matrixVectorMultiplicationRcpp(standardizedX, beta);
  
  NumericVector resultantVByspatialRandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInSpatialAssignmentMatrixTripletForm; z++) {
    resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] = resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] + spatialAssignmentMatrixInTripletForm(z, 2) * spatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 1)];
  }
  
  NumericVector resultantWByURandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
    resultantWByURandomEffects[WInTripletForm(z, 0)] = resultantWByURandomEffects[WInTripletForm(z, 0)] + WInTripletForm(z, 2) * uRandomEffects[WInTripletForm(z, 1)];
  }
  
  NumericVector errorRandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInX; z++) {
    errorRandomEffects[z] = y[z] - XBeta[z] - resultantVByspatialRandomEffects[z] - resultantWByURandomEffects[z];
  }
  
  double errorRandomEffectsSquared = vectorTransposeVectorMultiplicationRcpp(errorRandomEffects, errorRandomEffects);
  
  double sigmaSquaredE = 1 / rgamma(1, (a3 + 0.5 * numberOfRowsInX), 1 / (b3 + 0.5 * errorRandomEffectsSquared))[0];
  
  return sigmaSquaredE;
  
}
