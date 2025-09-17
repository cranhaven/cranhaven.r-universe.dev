#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List univariatePoissonNetworkLerouxURandomEffectsUpdate(NumericMatrix standardizedX,
                                                        NumericMatrix W,
                                                        NumericVector y,
                                                        const int numberOfRowsInX,
                                                        const int numberOfColumnsInX,
                                                        NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                        NumericMatrix WInTripletForm,
                                                        const int numberOfRowsInSpatialAssignmentMatrixInTripletForm,
                                                        const int numberOfRowsInWTripletForm,
                                                        NumericVector beta,
                                                        NumericVector spatialRandomEffects,
                                                        NumericVector uRandomEffects,
                                                        double sigmaSquaredU,
                                                        NumericVector uRandomEffectsTuningParameters,
                                                        NumericVector uRandomEffectsAcceptanceRate,
                                                        NumericVector numberOfAcceptedUREDraws,
                                                        NumericVector numberOfAllAcceptedUREDraws,
                                                        int currentNumberOfIterations, 
                                                        bool centerURandomEffects)
{
  
  NumericVector XBeta = matrixVectorMultiplicationRcpp(standardizedX, beta);
  
  NumericVector resultantVByspatialRandomEffects(numberOfRowsInX);
  for(int z = 0; z < numberOfRowsInSpatialAssignmentMatrixInTripletForm; z++) {
    resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] = resultantVByspatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 0)] + spatialAssignmentMatrixInTripletForm(z, 2) * spatialRandomEffects[spatialAssignmentMatrixInTripletForm(z, 1)];
  }
  
  for(int i = 0; i < uRandomEffects.size(); i++) {
    
    double uRandomEffectStar = rnorm(1, uRandomEffects[i], uRandomEffectsTuningParameters[i])[0];
    
    double U = runif(1)[0];
    
    NumericVector resultantShedWByURandomEffects(numberOfRowsInX);
    for(int z = 0; z < numberOfRowsInWTripletForm; z++) {
      if(i != WInTripletForm(z, 1)){
        resultantShedWByURandomEffects[WInTripletForm(z, 0)] = resultantShedWByURandomEffects[WInTripletForm(z, 0)] + WInTripletForm(z, 2) * uRandomEffects[WInTripletForm(z, 1)];
      }
    }
    
    double logUStarLikelihood;
    double logUCurrentLikelihood;
    
    NumericVector uRandomEffectStarRepeatedVector = doubleVectorMultiplicationRcpp(uRandomEffectStar, W(_, i));
    NumericVector uRandomEffectsRepeatedVector = doubleVectorMultiplicationRcpp(uRandomEffects[i], W(_, i));
    NumericVector resultantVByVAndWByURandomEffects = resultantVByspatialRandomEffects + resultantShedWByURandomEffects;
    
    NumericVector indecies = getNonZeroEntries(W(_, i));
    
    logUStarLikelihood = - (0.5 / sigmaSquaredU) * uRandomEffectStar * uRandomEffectStar
      + uRandomEffectStar * vectorTransposeVectorMultiplicationRcpp(y, W(_, i))
      - getSumExpNetworkLerouxIndecies(XBeta, resultantVByVAndWByURandomEffects, uRandomEffectStarRepeatedVector, indecies);
      
    logUCurrentLikelihood = - (0.5 / sigmaSquaredU) * uRandomEffects[i] * uRandomEffects[i]
    + uRandomEffects[i] * vectorTransposeVectorMultiplicationRcpp(y, W(_, i))
      - getSumExpNetworkLerouxIndecies(XBeta, resultantVByVAndWByURandomEffects, uRandomEffectsRepeatedVector, indecies);
      
    // compute the log acceptance probability
    double logA = logUStarLikelihood - logUCurrentLikelihood;
    
    if(log(U) < logA){
      uRandomEffects[i] = uRandomEffectStar;
      numberOfAcceptedUREDraws[i]++;
      numberOfAllAcceptedUREDraws[i]++;
    }
    
    uRandomEffectsAcceptanceRate[i] = numberOfAllAcceptedUREDraws[i] / currentNumberOfIterations;
    
    if(currentNumberOfIterations % 100 == 0){
      if(uRandomEffectsAcceptanceRate[i] > 0.5){
        uRandomEffectsTuningParameters[i] = 1.1 * uRandomEffectsTuningParameters[i];
      }
      if(uRandomEffectsAcceptanceRate[i] < 0.3){
        uRandomEffectsTuningParameters[i] = 0.9 * uRandomEffectsTuningParameters[i];
      }
      numberOfAcceptedUREDraws[i] = 0;
    }
  }
  
  List output(5);
  if(centerURandomEffects){
    output[0] = getMeanCenteredRandomEffects(uRandomEffects);
  } else {
    output[0] = uRandomEffects;
  }
  output[1] = uRandomEffectsTuningParameters;
  output[2] = uRandomEffectsAcceptanceRate;
  output[3] = numberOfAcceptedUREDraws;
  output[4] = numberOfAllAcceptedUREDraws;
  return output;
  
}
