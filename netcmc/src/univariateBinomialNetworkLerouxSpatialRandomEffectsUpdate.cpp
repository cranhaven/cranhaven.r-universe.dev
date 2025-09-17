#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List univariateBinomialNetworkLerouxSpatialRandomEffectsUpdate(NumericMatrix standardizedX,
                                                                NumericMatrix spatialAssignment,
                                                                NumericMatrix squareSpatialNeighbourhoodMatrix,
                                                                NumericVector trials,
                                                                NumericVector y,
                                                                const int numberOfRowsInX,
                                                                const int numberOfColumnsInX,
                                                                const int numberOfSpatialAreas,
                                                                NumericMatrix squareSpatialNeighbourhoodMatrixInTripletForm,
                                                                NumericMatrix WInTripletForm,
                                                                const int numberOfRowsInSquareSpatialNeighbourhoodMatrix,
                                                                const int numberOfRowsInWTripletForm,
                                                                NumericVector beta,
                                                                NumericVector spatialRandomEffects,
                                                                NumericVector uRandomEffects,
                                                                double spatialTauSquared,
                                                                double spatialRho,
                                                                NumericVector spatialRandomEffectsTuningParameters,
                                                                NumericVector spatialRandomEffectsAcceptanceRate,
                                                                NumericVector numberOfAcceptedSpatialRandomEffectsDraws,
                                                                NumericVector numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                                int currentNumberOfIterations,
                                                                bool centerSpatialRandomEffects)
{
  
  int updateIndex = 100;
  
  NumericVector XBeta = matrixVectorMultiplicationRcpp(standardizedX, beta);
  
  NumericVector resultantWByURandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
    resultantWByURandomEffects[WInTripletForm(z, 0)] = resultantWByURandomEffects[WInTripletForm(z, 0)] + WInTripletForm(z, 2) * uRandomEffects[WInTripletForm(z, 1)];
  }
  
  for(int i = 0; i < spatialRandomEffects.size(); i++) {
    
    double spatialRandomEffectsStar = rnorm(1, spatialRandomEffects[i], spatialRandomEffectsTuningParameters[i])[0];
    
    double U = runif(1)[0];
    
    double logSpatialRandomEffectsStarLikelihood;
    double logSpatialRandomEffectsCurrentLikelihood;
    
    NumericVector spatialAssignmentStarRepeatedVector = doubleVectorMultiplicationRcpp(spatialRandomEffectsStar, spatialAssignment(_, i));
    NumericVector spatialAssignmentRepeatedVector = doubleVectorMultiplicationRcpp(spatialRandomEffects[i], spatialAssignment(_, i));
    
    NumericVector indecies = getNonZeroEntries(spatialAssignment(_, i));
    
    double offdiagonalSpatialRandomEffectsBySpatialQBySpatialRandomEffects = 0.0;
    
    for(int j = 0; j < squareSpatialNeighbourhoodMatrixInTripletForm.rows(); j++){
      if(squareSpatialNeighbourhoodMatrixInTripletForm(j,0) == i && squareSpatialNeighbourhoodMatrixInTripletForm(j,0) != squareSpatialNeighbourhoodMatrixInTripletForm(j,1)){
        offdiagonalSpatialRandomEffectsBySpatialQBySpatialRandomEffects += - spatialRho * squareSpatialNeighbourhoodMatrixInTripletForm(j,2) * spatialRandomEffects[(squareSpatialNeighbourhoodMatrixInTripletForm(j,1))];
      }
    }
    
    for(int j = 0; j < squareSpatialNeighbourhoodMatrixInTripletForm.rows(); j++){
      if(squareSpatialNeighbourhoodMatrixInTripletForm(j,1) == i && squareSpatialNeighbourhoodMatrixInTripletForm(j,0) != squareSpatialNeighbourhoodMatrixInTripletForm(j,1)){
        offdiagonalSpatialRandomEffectsBySpatialQBySpatialRandomEffects += - spatialRho * squareSpatialNeighbourhoodMatrixInTripletForm(j,2) * spatialRandomEffects[(squareSpatialNeighbourhoodMatrixInTripletForm(j,0))];
      }
    }
    
    logSpatialRandomEffectsStarLikelihood = - (0.5 / spatialTauSquared) * ((spatialRandomEffectsStar * (spatialRho * getSumVector(squareSpatialNeighbourhoodMatrix(_, i)) + 1 - spatialRho) * spatialRandomEffectsStar) + (spatialRandomEffectsStar * offdiagonalSpatialRandomEffectsBySpatialQBySpatialRandomEffects))
      + spatialRandomEffectsStar * vectorTransposeVectorMultiplicationRcpp(y, spatialAssignment(_, i))
      - getSumLogExpIndecies(trials, XBeta, resultantWByURandomEffects, spatialAssignmentStarRepeatedVector, indecies);
      
    logSpatialRandomEffectsCurrentLikelihood = - (0.5 / spatialTauSquared) * ((spatialRandomEffects[i] * (spatialRho * getSumVector(squareSpatialNeighbourhoodMatrix(_, i)) + 1 - spatialRho) * spatialRandomEffects[i]) + (spatialRandomEffects[i] * offdiagonalSpatialRandomEffectsBySpatialQBySpatialRandomEffects))
      + spatialRandomEffects[i] * vectorTransposeVectorMultiplicationRcpp(y, spatialAssignment(_, i))
      - getSumLogExpIndecies(trials, XBeta, resultantWByURandomEffects, spatialAssignmentRepeatedVector, indecies);
      
    double logA = logSpatialRandomEffectsStarLikelihood - logSpatialRandomEffectsCurrentLikelihood;
    
    if(log(U) < logA){
      spatialRandomEffects[i] = spatialRandomEffectsStar;
      numberOfAcceptedSpatialRandomEffectsDraws[i] += 1;
      numberOfAllAcceptedSpatialRandomEffectsDraws[i] += 1;
    }
    
    spatialRandomEffectsAcceptanceRate[i] = numberOfAllAcceptedSpatialRandomEffectsDraws[i] / currentNumberOfIterations;
    
    if(currentNumberOfIterations % updateIndex == 0){
      double kthSpatialRandomEffectsAcceptanceRate = numberOfAcceptedSpatialRandomEffectsDraws[i] / updateIndex;
      if(kthSpatialRandomEffectsAcceptanceRate > 0.5){
        spatialRandomEffectsTuningParameters[i] = 1.1 * spatialRandomEffectsTuningParameters[i];
      }
      if(kthSpatialRandomEffectsAcceptanceRate < 0.3){
        spatialRandomEffectsTuningParameters[i] = 0.9 * spatialRandomEffectsTuningParameters[i];
      }
      numberOfAcceptedSpatialRandomEffectsDraws[i] = 0;
    }
  }
  
  List output(5);
  if(centerSpatialRandomEffects){
    output[0] = getMeanCenteredRandomEffects(spatialRandomEffects);
  } else {
    output[0] = spatialRandomEffects;
  }
  output[1] = spatialRandomEffectsTuningParameters;
  output[2] = spatialRandomEffectsAcceptanceRate;
  output[3] = numberOfAcceptedSpatialRandomEffectsDraws;
  output[4] = numberOfAllAcceptedSpatialRandomEffectsDraws;
  return output;
  
}
