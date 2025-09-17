#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List multivariatePoissonNetworkLerouxSpatialRandomEffectsUpdate(NumericMatrix standardizedX,
                                                                const int numberOfResponses,
                                                                NumericMatrix spatialAssignment,
                                                                NumericMatrix squareSpatialNeighbourhoodMatrix,
                                                                NumericVector y,
                                                                const int numberOfSpatialAreas,
                                                                NumericMatrix squareSpatialNeighbourhoodMatrixInTripletForm,
                                                                NumericMatrix WInTripletForm,
                                                                const int numberOfRowsInSquareSpatialNeighbourhoodMatrix,
                                                                const int numberOfRowsInWTripletForm,
                                                                NumericVector beta,
                                                                NumericVector spatialRandomEffects,
                                                                NumericVector uRandomEffects,
                                                                NumericVector spatialTauSquared,
                                                                NumericVector spatialRho,
                                                                NumericVector spatialRandomEffectsTuningParameters,
                                                                NumericVector spatialRandomEffectsAcceptanceRate,
                                                                NumericVector numberOfAcceptedSpatialRandomEffectsDraws,
                                                                NumericVector numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                                int currentNumberOfIterations,
                                                                bool centerSpatialRandomEffects)
{
  
  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInSpatialAssignmentMatrix = spatialRandomEffects.size() / numberOfResponses;
  int numberOfColumnsInW = uRandomEffects.size() / numberOfResponses;
  
  for(int j = 0; j < numberOfResponses; j++) {
    
    List output = univariatePoissonNetworkLerouxSpatialRandomEffectsUpdate(standardizedX,
                                                                           spatialAssignment,
                                                                           squareSpatialNeighbourhoodMatrix,
                                                                           getSubvector(y, j * numberOfRowsInX, ((j + 1) * numberOfRowsInX) - 1),
                                                                           numberOfRowsInX,
                                                                           numberOfColumnsInX,
                                                                           numberOfSpatialAreas,
                                                                           squareSpatialNeighbourhoodMatrixInTripletForm,
                                                                           WInTripletForm,
                                                                           numberOfRowsInSquareSpatialNeighbourhoodMatrix,
                                                                           numberOfRowsInWTripletForm,
                                                                           getSubvector(beta, j * numberOfColumnsInX, ((j + 1) * numberOfColumnsInX) - 1),
                                                                           getSubvector(spatialRandomEffects, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                                           getSubvector(uRandomEffects, j * numberOfColumnsInW, ((j + 1) * numberOfColumnsInW) - 1),
                                                                           spatialTauSquared[j],
                                                                           spatialRho[j],
                                                                           getSubvector(spatialRandomEffectsTuningParameters, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                                           getSubvector(spatialRandomEffectsAcceptanceRate, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                                           getSubvector(numberOfAcceptedSpatialRandomEffectsDraws, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                                           getSubvector(numberOfAllAcceptedSpatialRandomEffectsDraws, j * numberOfColumnsInSpatialAssignmentMatrix, ((j + 1) * numberOfColumnsInSpatialAssignmentMatrix) - 1),
                                                                           currentNumberOfIterations,
                                                                           centerSpatialRandomEffects);
    
    NumericVector rthSpatialRandomEffects = output[0];
    NumericVector rthSpatialRandomEffectsTuningParameters = output[1];
    NumericVector rthSpatialRandomEffectsAcceptanceRate = output[2];
    NumericVector rthNumberOfAcceptedSpatialRandomEffectsDraws = output[3];
    NumericVector rthNumberOfAllAcceptedSpatialRandomEffectsDraws = output[4];
    
    for(int i = 0; i < numberOfColumnsInSpatialAssignmentMatrix; i++){
      spatialRandomEffects[i + (j * numberOfColumnsInSpatialAssignmentMatrix)] = rthSpatialRandomEffects[i];
      spatialRandomEffectsTuningParameters[i + (j * numberOfColumnsInSpatialAssignmentMatrix)] = rthSpatialRandomEffectsTuningParameters[i];
      spatialRandomEffectsAcceptanceRate[i + (j * numberOfColumnsInSpatialAssignmentMatrix)] = rthSpatialRandomEffectsAcceptanceRate[i];
      numberOfAcceptedSpatialRandomEffectsDraws[i + (j * numberOfColumnsInSpatialAssignmentMatrix)] = rthNumberOfAcceptedSpatialRandomEffectsDraws[i];
      numberOfAllAcceptedSpatialRandomEffectsDraws[i + (j * numberOfColumnsInSpatialAssignmentMatrix)] = rthNumberOfAllAcceptedSpatialRandomEffectsDraws[i];
    }
    
  }
  
  List output(5);
  output[0] = spatialRandomEffects;
  output[1] = spatialRandomEffectsTuningParameters;
  output[2] = spatialRandomEffectsAcceptanceRate;
  output[3] = numberOfAcceptedSpatialRandomEffectsDraws;
  output[4] = numberOfAllAcceptedSpatialRandomEffectsDraws;
  return output;
  
}
