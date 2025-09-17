#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector multivariateGaussianNetworkLerouxURandomEffectsUpdate(NumericMatrix standardizedX,
                                                                    NumericVector y,
                                                                    NumericMatrix W,
                                                                    const int numberOfResponses,
                                                                    NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                                    NumericMatrix WInTripletForm,
                                                                    const int numberOfRowsInSpatialAssignmentMatrixInTripletForm,
                                                                    const int numberOfRowsInWTripletForm,
                                                                    NumericVector beta,
                                                                    NumericVector spatialRandomEffects,
                                                                    NumericVector uRandomEffects,
                                                                    NumericMatrix varianceCovarianceU,
                                                                    NumericVector sigmaSquaredE,
                                                                    bool centerURandomEffects)
{
  
  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInV = spatialRandomEffects.size() / numberOfResponses;
  int numberOfColumnsInW = W.cols();

  NumericVector XBeta(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector betaRthResponse = getSubvector(beta, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1);
    NumericVector XBetaRthResponse = matrixVectorMultiplicationRcpp(standardizedX, betaRthResponse);
    for(int z = 0; z < numberOfRowsInX; z++) {
      XBeta[z + (j * numberOfRowsInX)] = XBetaRthResponse[z];
    }
  }

  NumericVector resultantVByVRandomEffects(numberOfRowsInX * numberOfResponses);
  for(int j = 0; j < numberOfResponses; j++) {
    NumericVector vRandomEffectsRthResponse = getSubvector(spatialRandomEffects, j * numberOfColumnsInV, ((j + 1) * numberOfColumnsInV) - 1);
    for(int z = 0; z < numberOfRowsInSpatialAssignmentMatrixInTripletForm; z++) {
      resultantVByVRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0) + (j * numberOfRowsInX)] = resultantVByVRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0) + (j * numberOfRowsInX)] + spatialAssignmentMatrixInTripletForm(z, 2) * vRandomEffectsRthResponse[spatialAssignmentMatrixInTripletForm(z, 1)];
    }
  }
  
  NumericMatrix varianceCovarianceTilde = getDiagonalMatrix(sigmaSquaredE);
  NumericMatrix varianceCovarianceUInverse = matrixInverseRcppConversion(varianceCovarianceU);

  for(int i = 0; i < numberOfColumnsInW; i++) {

    NumericVector resultantShedWByURandomEffects(numberOfRowsInX * numberOfResponses);
    for(int j = 0; j < numberOfResponses; j++) {
      NumericVector uRandomEffectsRthResponse = getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1);
      for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
        if(i != WInTripletForm(z, 1)){
          resultantShedWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] = resultantShedWByURandomEffects[WInTripletForm(z, 0) + (j * numberOfRowsInX)] + WInTripletForm(z, 2) * uRandomEffectsRthResponse[WInTripletForm(z, 1)];
        }
      }
    }
    
    NumericVector indecies = getNonZeroEntries(W(_, i));

    double sumKthWSquared = vectorTransposeVectorMultiplicationRcpp(W(_, i), W(_, i));
    
    NumericVector errorRandomEffects(numberOfRowsInX * numberOfResponses);
    for(int z = 0; z < (numberOfRowsInX * numberOfResponses); z++) {
      errorRandomEffects[z] = y[z] - XBeta[z] - resultantVByVRandomEffects[z] - resultantShedWByURandomEffects[z];
    }
    
    NumericVector mk(numberOfResponses);
    for(int j = 0; j < numberOfResponses; j++) {
      NumericVector errorRandomEffectsRthResponse = getSubvector(errorRandomEffects, j * numberOfRowsInX, ((j + 1) * numberOfRowsInX) - 1);
      mk[j] = vectorTransposeVectorMultiplicationRcpp(errorRandomEffectsRthResponse, W(_, i));
    }
    
    NumericMatrix sumKthWSquaredVarianceCovarianceTilde = doubleMatrixMultiplicationRcpp(sumKthWSquared, varianceCovarianceTilde);
    
    NumericMatrix AInverse = matrixMatrixAdditionRcpp(sumKthWSquaredVarianceCovarianceTilde, varianceCovarianceUInverse);
    NumericMatrix A = matrixInverseRcppConversion(AInverse);
    
    NumericVector mkTransposeVarianceCovarianceTilde = matrixVectorMultiplicationRcpp(transpose(varianceCovarianceTilde), mk);
    NumericVector kthURandomEffectsTilde = matrixVectorMultiplicationRcpp(transpose(A), mkTransposeVarianceCovarianceTilde);
    
    NumericVector randomStandardNormalDraws(numberOfResponses);
    for(int j = 0; j < numberOfResponses; j++) {
      randomStandardNormalDraws[j] = rnorm(1, 0.0, 1.0)[0];
    }
    
    NumericMatrix choleskyDecomposition = choleskyDecompositionRcppConversion(A);
    NumericMatrix choleskyDecompositionTranspose = transpose(choleskyDecomposition);
    NumericVector choleskyDecompositionComponent = matrixVectorMultiplicationRcpp(choleskyDecompositionTranspose, randomStandardNormalDraws);
    
    for(int j = 0; j < numberOfResponses; j++) {
      uRandomEffects[i + (j * numberOfColumnsInW)] = kthURandomEffectsTilde[j] + choleskyDecompositionComponent[j];
    } 
    
  }

  NumericVector allCenteredRandomEffects(uRandomEffects.size());
  for(int j = 0; j < numberOfResponses; j++){
    NumericVector centeredRandomEffects = getMeanCenteredRandomEffects(getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1));
    for(int z = 0; z < numberOfColumnsInW; z++){
      allCenteredRandomEffects[(j * numberOfColumnsInW) + z] = centeredRandomEffects[z];
    }
  }

  if(centerURandomEffects){
    return allCenteredRandomEffects;
  } else {
    return uRandomEffects;
  }

}
