#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector univariateGaussianNetworkLerouxURandomEffectsUpdate(NumericMatrix standardizedX,
                                                                   NumericMatrix W,
                                                                   NumericVector y,
                                                                   const int numberOfRowsInX,
                                                                   NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                                   NumericMatrix WInTripletForm,
                                                                   const int numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                   const int numberOfRowsInWTripletForm,
                                                                   NumericVector beta,
                                                                   NumericVector spatialRandomEffects,
                                                                   NumericVector uRandomEffects,
                                                                   double sigmaSquaredU,
                                                                   double sigmaSquaredE, 
                                                                   bool centerURandomEffects)
{
  
  NumericVector XBeta = matrixVectorMultiplicationRcpp(standardizedX, beta);
  
  NumericVector resultantVByspatialRandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInSpatialAssignmentMatrixTripletForm; z++) {
    resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] = resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] + spatialAssignmentMatrixInTripletForm(z, 2) * spatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 1)];
  }
  
  for(int i = 0; i < uRandomEffects.size(); i++) {
    
    NumericVector resultantShedWByURandomEffects(numberOfRowsInX);
    for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
      if(i != WInTripletForm(z, 1)){
        resultantShedWByURandomEffects[WInTripletForm(z, 0)] = resultantShedWByURandomEffects[WInTripletForm(z, 0)] + WInTripletForm(z, 2) * uRandomEffects[WInTripletForm(z, 1)];
      }
    }
    
    NumericVector differenceYXBetaPhiReShedURE(numberOfRowsInX);
    for(int z = 0; z < numberOfRowsInX; z++) {
      differenceYXBetaPhiReShedURE[z] = y[z] - XBeta[z] - resultantVByspatialRandomEffects[z] - resultantShedWByURandomEffects[z];
    }
    
    double l = 1 / ( (1/sigmaSquaredU) + (vectorTransposeVectorMultiplicationRcpp(W(_, i), W(_, i))/sigmaSquaredE) );
    
    double h = l * (1/sigmaSquaredE) * vectorTransposeVectorMultiplicationRcpp(differenceYXBetaPhiReShedURE, W(_, i));
    
    uRandomEffects[i] = rnorm(1, h, sqrt(l))[0];
    
  }
  
  if(centerURandomEffects){
    uRandomEffects = getMeanCenteredRandomEffects(uRandomEffects);
  }

  return uRandomEffects;
  
}
