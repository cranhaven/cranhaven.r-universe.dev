#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

double univariatePoissonNetworkLerouxTauSquaredUpdate(NumericMatrix squareSpatialNeighbourhoodMatrix,
                                                      NumericVector spatialRandomEffects,
                                                      double spatialTauSquared,
                                                      double spatialRho,
                                                      double a1,
                                                      double b1,
                                                      NumericMatrix QSpatialMatrixComponent1)
{
  int numberOfSpatialAreas = squareSpatialNeighbourhoodMatrix.cols();
  NumericVector vectorOfOnes(numberOfSpatialAreas, 1.0);
  
  NumericMatrix QSpatialMatrix = matrixMatrixAdditionRcpp(doubleMatrixMultiplicationRcpp(spatialRho, QSpatialMatrixComponent1),
                                                          doubleMatrixMultiplicationRcpp((1 - spatialRho),  getDiagonalMatrix(vectorOfOnes)));
  
  double spatialRandomEffectsBySpatialQBySpatialRandomEffectsMultiplication = vectorTransposeVectorMultiplicationRcpp(spatialRandomEffects, matrixVectorMultiplicationRcpp(QSpatialMatrix, spatialRandomEffects));
  
  spatialTauSquared = 1 / rgamma(1, (a1 + 0.5 * numberOfSpatialAreas), 1 / (b1 + 0.5 * spatialRandomEffectsBySpatialQBySpatialRandomEffectsMultiplication))[0];

  
  return spatialTauSquared;
      
}
