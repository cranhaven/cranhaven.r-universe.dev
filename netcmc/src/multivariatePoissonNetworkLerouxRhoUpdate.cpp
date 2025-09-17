#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List multivariatePoissonNetworkLerouxRhoUpdate(NumericMatrix squareSpatialNeighbourhoodMatrix,
                                               NumericVector spatialRandomEffects,
                                               const int numberOfResponses,
                                               NumericVector spatialTauSquared,
                                               NumericVector spatialRho,
                                               NumericVector spatialRhoTuningParameters,
                                               NumericVector spatialRhoAcceptanceRate,
                                               NumericVector numberOfAcceptedSpatialRhoDraws,
                                               NumericVector numberOfAllAcceptedSpatialRhoDraws,
                                               NumericMatrix QSpatialMatrixComponent1,
                                               NumericVector QSpatialMatrixComponent1EigenValues,
                                               int currentNumberOfIterations)
{
  
  int numberOfColumnsInSpatialAssignmentMatrix = spatialRandomEffects.size() / numberOfResponses;

  for(int j = 0; j < numberOfResponses; j++) {
    
    List output = univariatePoissonNetworkLerouxRhoUpdate(squareSpatialNeighbourhoodMatrix,
                                                          getSubvector(spatialRandomEffects, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                          spatialTauSquared[j],
                                                          spatialRho[j],
                                                          spatialRhoTuningParameters[j],
                                                          spatialRhoAcceptanceRate[j],
                                                          numberOfAcceptedSpatialRhoDraws[j],
                                                          numberOfAllAcceptedSpatialRhoDraws[j],
                                                          QSpatialMatrixComponent1,
                                                          QSpatialMatrixComponent1EigenValues,
                                                          currentNumberOfIterations);
    
    spatialRho[j] = output[0];
    spatialRhoTuningParameters[j] = output[1];
    spatialRhoAcceptanceRate[j] = output[2];
    numberOfAcceptedSpatialRhoDraws[j] = output[3];
    numberOfAllAcceptedSpatialRhoDraws[j] = output[4];
    
  }
  
  List output(5);
  output[0] = spatialRho;
  output[1] = spatialRhoTuningParameters;
  output[2] = spatialRhoAcceptanceRate;
  output[3] = numberOfAcceptedSpatialRhoDraws;
  output[4] = numberOfAllAcceptedSpatialRhoDraws;
  return output;
      
}
