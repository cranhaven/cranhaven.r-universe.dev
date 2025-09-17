#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

NumericVector univariateGaussianNetworkLerouxBetaUpdate(NumericMatrix standardizedX,
                                                        NumericVector y,
                                                        const int numberOfRowsInX,
                                                        const int numberOfColumnsInX,
                                                        NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                        NumericMatrix WInTripletForm,
                                                        const int numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                        const int numberOfRowsInWTripletForm,
                                                        NumericVector beta,
                                                        NumericVector spatialRandomEffects,
                                                        NumericVector uRandomEffects,
                                                        double sigmaSquaredE,
                                                        double covarianceBetaPrior)
{

  
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
    errorRandomEffects[z] = y[z] - resultantVByspatialRandomEffects[z] - resultantWByURandomEffects[z];
  }
  
  NumericMatrix XXTranspose(numberOfColumnsInX, numberOfColumnsInX);
  NumericMatrix XXTransposeLoop(numberOfColumnsInX, numberOfColumnsInX);
  for(int z = 0; z < numberOfRowsInX; z++) {
    NumericVector XVector(numberOfColumnsInX);
    for(int j = 0; j < numberOfColumnsInX; j++) {
      XVector[j] = standardizedX(z, j);
    }
    NumericMatrix xVectorVectorTranspose = vectorVectorTransposeMultiplicationRcpp(XVector, XVector);
    XXTransposeLoop = XXTranspose;
    XXTranspose = matrixMatrixAdditionRcpp(XXTransposeLoop, xVectorVectorTranspose);
  }
  
  NumericMatrix XXTransposesigmaSquaredE = doubleMatrixMultiplicationRcpp(1/sigmaSquaredE, XXTranspose);

  NumericVector vectorOfRepeatedOnes(numberOfColumnsInX, 1.0);
  NumericMatrix identityMatrix = getDiagonalMatrix(vectorOfRepeatedOnes);
  NumericMatrix identityMatrixBetaPriorInverse = doubleMatrixMultiplicationRcpp(1 / covarianceBetaPrior, identityMatrix);
  
  NumericMatrix dMatrixInverse = matrixMatrixAdditionRcpp(XXTransposesigmaSquaredE, identityMatrixBetaPriorInverse);
  NumericMatrix dMatrix = matrixInverseRcppConversion(dMatrixInverse);
  
  NumericMatrix XVectorErrorSigmaSquaredE(numberOfColumnsInX);
  for(int z = 0; z < numberOfRowsInX; z++) {
    NumericVector XVector(numberOfColumnsInX);
    for(int j = 0; j < numberOfColumnsInX; j++) {
      XVector[j] = standardizedX(z, j);
    }
    NumericVector XVectorErrorSigmaSquaredEHold = doubleVectorMultiplicationRcpp(errorRandomEffects[z]/sigmaSquaredE, XVector);
    NumericVector XVectorErrorSigmaSquaredELoop = XVectorErrorSigmaSquaredE;
    for(int j = 0; j < numberOfColumnsInX; j++) {
      XVectorErrorSigmaSquaredE[j] = XVectorErrorSigmaSquaredELoop[j] + XVectorErrorSigmaSquaredEHold[j];
    }
  }
  
  NumericVector betaTilde = matrixVectorMultiplicationRcpp(dMatrix, XVectorErrorSigmaSquaredE);
  
  NumericVector randomStandardNormalDraws(numberOfColumnsInX);
  for(int j = 0; j < numberOfColumnsInX; j++) {
    randomStandardNormalDraws[j] = rnorm(1, 0.0, 1.0)[0];
  }
  
  NumericMatrix choleskyDecomposition = choleskyDecompositionRcppConversion(dMatrix);
  NumericMatrix choleskyDecompositionTranspose = transpose(choleskyDecomposition);
  NumericVector choleskyDecompositionComponent = matrixVectorMultiplicationRcpp(choleskyDecompositionTranspose, randomStandardNormalDraws);
  
  for(int j = 0; j < numberOfColumnsInX; j++) {
    beta[j] = betaTilde[j] + choleskyDecompositionComponent[j];
  } 
  
  return beta;
  
}
