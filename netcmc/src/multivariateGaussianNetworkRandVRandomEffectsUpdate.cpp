#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector multivariateGaussianNetworkRandVRandomEffectsUpdate(NumericMatrix standardizedX,
                                                                    NumericVector y,
                                                                    NumericMatrix V,
                                                                    NumericMatrix W,
                                                                    const int numberOfResponses,
                                                                    NumericMatrix WInTripletForm,
                                                                    const int numberOfRowsInWTripletForm,
                                                                    NumericVector beta,
                                                                    NumericVector vRandomEffects,
                                                                    NumericVector uRandomEffects,
                                                                    NumericMatrix varianceCovarianceV,
                                                                    NumericVector sigmaSquaredE,
                                                                    bool centerVRandomEffects)
{
  
  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInV = V.cols();
  int numberOfColumnsInW = W.cols();

  NumericVector XBeta(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector betaRthResponse = getSubvector(beta, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1);
    NumericVector XBetaRthResponse = matrixVectorMultiplicationRcpp(standardizedX, betaRthResponse);
    for(int z = 0; z < numberOfRowsInX; z++) {
      XBeta[z + (j * numberOfRowsInX)] = XBetaRthResponse[z];
    }
  }

  NumericVector resultantWByURandomEffects(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector uRandomEffectsRthResponse = getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1);
    for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
      resultantWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] = resultantWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] + WInTripletForm(z, 2) * uRandomEffectsRthResponse[WInTripletForm(z, 1)];
    }
  }
  
  NumericVector errorRandomEffects(numberOfRowsInX * numberOfResponses);
  for(int z = 0; z < (numberOfRowsInX * numberOfResponses); z++) {
    errorRandomEffects[z] = y[z] - XBeta[z] - resultantWByURandomEffects[z];
  }
  
  NumericMatrix varianceCovarianceTilde = getDiagonalMatrix(sigmaSquaredE);
  NumericMatrix varianceCovarianceVInverse = matrixInverseRcppConversion(varianceCovarianceV);

  for(int i = 0; i < numberOfColumnsInV; i++) {
    
    NumericVector indecies = getNonZeroEntries(V(_, i));
    
    double hk = indecies.size();
    
    NumericMatrix sumKthVSquaredVarianceCovarianceTilde = doubleMatrixMultiplicationRcpp(hk, varianceCovarianceTilde);
    
    NumericMatrix BInverse = matrixMatrixAdditionRcpp(sumKthVSquaredVarianceCovarianceTilde, varianceCovarianceVInverse);
    NumericMatrix B = matrixInverseRcppConversion(BInverse);
    
    NumericVector ck(numberOfResponses);
    for(int j = 0; j < numberOfResponses; j++) {
      NumericVector errorRandomEffectsRthResponse = getSubvector(errorRandomEffects, j * numberOfRowsInX, ((j + 1) * numberOfRowsInX) - 1);
      ck[j] = getSumVector(getSubvectorIndecies(errorRandomEffectsRthResponse, indecies));
    }
    
    NumericVector ckTransposeVarianceCovarianceTilde = matrixVectorMultiplicationRcpp(transpose(varianceCovarianceTilde), ck);
    NumericVector kthVRandomEffectsTilde = matrixVectorMultiplicationRcpp(transpose(B), ckTransposeVarianceCovarianceTilde);
    
    NumericVector randomStandardNormalDraws(numberOfResponses);
    for(int j = 0; j < numberOfResponses; j++) {
      randomStandardNormalDraws[j] = rnorm(1, 0.0, 1.0)[0];
    }
    
    NumericMatrix choleskyDecomposition = choleskyDecompositionRcppConversion(B);
    NumericMatrix choleskyDecompositionTranspose = transpose(choleskyDecomposition);
    NumericVector choleskyDecompositionComponent = matrixVectorMultiplicationRcpp(choleskyDecompositionTranspose, randomStandardNormalDraws);
    
    for(int j = 0; j < numberOfResponses; j++) {
      vRandomEffects[i + (j * numberOfColumnsInV)] = kthVRandomEffectsTilde[j] + choleskyDecompositionComponent[j];
    } 
    
  }

  NumericVector allCenteredRandomEffects(vRandomEffects.size());
  for(int j = 0; j < numberOfResponses; j++){
    NumericVector centeredRandomEffects = getMeanCenteredRandomEffects(getSubvector(vRandomEffects, j * numberOfColumnsInV, ((j + 1) * numberOfColumnsInV) - 1));
    for(int z = 0; z < numberOfColumnsInW; z++){
      allCenteredRandomEffects[(j * numberOfColumnsInV) + z] = centeredRandomEffects[z];
    }
  }

  if(centerVRandomEffects){
    return allCenteredRandomEffects;
  } else {
    return vRandomEffects;
  }

}
