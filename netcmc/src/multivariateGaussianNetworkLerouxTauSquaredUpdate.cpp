#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector multivariateGaussianNetworkLerouxTauSquaredUpdate(NumericMatrix squareSpatialNeighbourhoodMatrix,
                                                                 NumericVector spatialRandomEffects,
                                                                 const int numberOfResponses,
                                                                 NumericVector spatialTauSquared,
                                                                 NumericVector spatialRho,
                                                                 double a1,
                                                                 double b1,
                                                                 NumericMatrix QSpatialMatrixComponent1)
{
  int numberOfColumnsInSpatialAssignmentMatrix = spatialRandomEffects.size() / numberOfResponses;
  
  for(int j = 0; j < numberOfResponses; j++) {
    
    spatialTauSquared[j] = univariateGaussianNetworkLerouxTauSquaredUpdate(squareSpatialNeighbourhoodMatrix,
                                                                            getSubvector(spatialRandomEffects, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                                            spatialTauSquared[j],
                                                                            spatialRho[j],
                                                                            a1,
                                                                            b1,
                                                                            QSpatialMatrixComponent1);
    
  }
  
  return spatialTauSquared;
      
}
