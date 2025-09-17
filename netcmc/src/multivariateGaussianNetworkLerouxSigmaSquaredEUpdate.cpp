#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector multivariateGaussianNetworkLerouxSigmaSquaredEUpdate(NumericMatrix standardizedX,
                                                                   const int numberOfResponses,
                                                                   NumericVector y,
                                                                   NumericMatrix spatialAssignment,
                                                                   NumericMatrix W,
                                                                   NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                                   NumericMatrix WInTripletForm,
                                                                   const int numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                   const int numberOfRowsInWTripletForm,
                                                                   NumericVector beta,
                                                                   NumericVector spatialRandomEffects,
                                                                   NumericVector uRandomEffects,
                                                                   double a3,
                                                                   double b3)
{
  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInSpatialAssignmentMatrix = spatialRandomEffects.size() / numberOfResponses;
  int numberOfColumnsInW = uRandomEffects.size() / numberOfResponses;
  NumericVector sigmaSquaredE(numberOfResponses);
  
  for(int j = 0; j < numberOfResponses; j++) {
    
    sigmaSquaredE[j] = univariateGaussianNetworkLerouxSigmaSquaredEUpdate(standardizedX,
                                                                          y,
                                                                          spatialAssignment,
                                                                          W,
                                                                          spatialAssignmentMatrixInTripletForm,
                                                                          WInTripletForm,
                                                                          numberOfRowsInX,
                                                                          numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                          numberOfRowsInWTripletForm,
                                                                          getSubvector(beta, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1),
                                                                          getSubvector(spatialRandomEffects, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                                          getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1),
                                                                          a3,
                                                                          b3);
    
  }
  
  return sigmaSquaredE;
      
}
