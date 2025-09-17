#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List univariatePoissonNetworkLerouxRhoUpdate(NumericMatrix squareSpatialNeighbourhoodMatrix,
                                             NumericVector spatialRandomEffects,
                                             double spatialTauSquared,
                                             double spatialRho,
                                             double spatialRhoTuningParameters,
                                             double spatialRhoAcceptanceRate,
                                             double numberOfAcceptedSpatialRhoDraws,
                                             double numberOfAllAcceptedSpatialRhoDraws,
                                             NumericMatrix QSpatialMatrixComponent1,
                                             NumericVector QSpatialMatrixComponent1EigenValues,
                                             int currentNumberOfIterations)
{
  
  int updateIndex = 100;
  
  int dimensionOfSpatialMatrix = squareSpatialNeighbourhoodMatrix.cols();
  NumericVector vectorOfOnes(dimensionOfSpatialMatrix, 1.0);
  
  double spatialRhoStar = -1.0;
  
  while(spatialRhoStar < 0 || spatialRhoStar > 1){
    spatialRhoStar = rnorm(1, spatialRho, spatialRhoTuningParameters)[0];
  }
  
  double logDeterminentQSpatialMatrix = 0.0;
  double logDeterminentQSpatialMatrixStar = 0.0;
  
  for(int i = 0; i < QSpatialMatrixComponent1EigenValues.size(); i++){
    logDeterminentQSpatialMatrix += log(spatialRho * QSpatialMatrixComponent1EigenValues[i] + (1 - spatialRho));
    logDeterminentQSpatialMatrixStar += log(spatialRhoStar * QSpatialMatrixComponent1EigenValues[i] + (1 - spatialRhoStar));
  }
  
  NumericMatrix QSpatialMatrixStar = matrixMatrixAdditionRcpp(doubleMatrixMultiplicationRcpp(spatialRhoStar, QSpatialMatrixComponent1),
                                                              doubleMatrixMultiplicationRcpp((1 - spatialRhoStar),  getDiagonalMatrix(vectorOfOnes)));
  
  NumericMatrix QSpatialMatrix = matrixMatrixAdditionRcpp(doubleMatrixMultiplicationRcpp(spatialRho, QSpatialMatrixComponent1),
                                                          doubleMatrixMultiplicationRcpp((1 - spatialRho),  getDiagonalMatrix(vectorOfOnes)));
  
  double spatialRandomEffectsBySpatialQStarBySpatialRandomEffectsMultiplication = vectorTransposeVectorMultiplicationRcpp(spatialRandomEffects, matrixVectorMultiplicationRcpp(QSpatialMatrixStar, spatialRandomEffects));
  
  double spatialRandomEffectsBySpatialQBySpatialRandomEffectsMultiplication = vectorTransposeVectorMultiplicationRcpp(spatialRandomEffects, matrixVectorMultiplicationRcpp(QSpatialMatrix, spatialRandomEffects));
  
  double logSpatialRhoStarLikelihood = 0.5 * logDeterminentQSpatialMatrixStar
    - (0.5 / spatialTauSquared) * spatialRandomEffectsBySpatialQStarBySpatialRandomEffectsMultiplication
    + log(R::pnorm((1 - spatialRho) / spatialRhoTuningParameters, 0.0, 1.0, TRUE, FALSE) - 1 + R::pnorm(spatialRho / spatialRhoTuningParameters, 0.0, 1.0, TRUE, FALSE)); 
    
  double logSpatialRhoCurrentLikelihood = 0.5 * logDeterminentQSpatialMatrix
    - (0.5 / spatialTauSquared) * spatialRandomEffectsBySpatialQBySpatialRandomEffectsMultiplication
    + log(R::pnorm((1 - spatialRhoStar) / spatialRhoTuningParameters, 0.0, 1.0, TRUE, FALSE) - 1 + R::pnorm(spatialRhoStar / spatialRhoTuningParameters, 0.0, 1.0, TRUE, FALSE)); 
    
  double logA = logSpatialRhoStarLikelihood - logSpatialRhoCurrentLikelihood;
  
  double U = runif(1)[0];
  
  if(log(U) < logA){
    spatialRho = spatialRhoStar;
    numberOfAcceptedSpatialRhoDraws++;
    numberOfAllAcceptedSpatialRhoDraws++;
  }
  
  spatialRhoAcceptanceRate = numberOfAllAcceptedSpatialRhoDraws / currentNumberOfIterations;
  
  if(currentNumberOfIterations % updateIndex == 0){
    double kthSpatialRhoAcceptanceRate = numberOfAcceptedSpatialRhoDraws / updateIndex;
    if(kthSpatialRhoAcceptanceRate > 0.5){
      spatialRhoTuningParameters = 1.1 * spatialRhoTuningParameters;
    }
    if(kthSpatialRhoAcceptanceRate < 0.4){
      spatialRhoTuningParameters = 0.9 * spatialRhoTuningParameters;
    }
    if(spatialRhoTuningParameters > 0.3){
      spatialRhoTuningParameters = 0.3;
    }
    numberOfAcceptedSpatialRhoDraws = 0;
  }
  
  List output(5);
  
  output[0] = spatialRho;
  output[1] = spatialRhoTuningParameters;
  output[2] = spatialRhoAcceptanceRate;
  output[3] = numberOfAcceptedSpatialRhoDraws;
  output[4] = numberOfAllAcceptedSpatialRhoDraws;
  
  return output;
      
}
