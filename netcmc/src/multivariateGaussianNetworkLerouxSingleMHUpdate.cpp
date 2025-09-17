#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List multivariateGaussianNetworkLerouxSingleMHUpdate(NumericMatrix standardizedX,
                                                   NumericVector y,
                                                   const int numberOfResponses,
                                                   NumericMatrix squareSpatialNeighbourhoodMatrix,
                                                   NumericMatrix spatialAssignment,
                                                   NumericMatrix W,
                                                   const int numberOfSpatialAreas,
                                                   NumericMatrix squareSpatialNeighbourhoodMatrixInTripletForm,
                                                   NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                   NumericMatrix WInTripletForm,
                                                   NumericVector beta,
                                                   NumericVector spatialRandomEffects,
                                                   NumericVector uRandomEffects,
                                                   NumericVector spatialTauSquared,
                                                   NumericVector spatialRho,
                                                   NumericMatrix varianceCovarianceU,
                                                   NumericVector sigmaSquaredE,
                                                   double covarianceBetaPrior,
                                                   NumericVector spatialRandomEffectsTuningParameters,
                                                   NumericVector spatialRandomEffectsAcceptanceRate,
                                                   NumericVector numberOfAcceptedSpatialRandomEffectsDraws,
                                                   NumericVector numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                   NumericVector spatialRhoTuningParameters,
                                                   NumericVector spatialRhoAcceptanceRate,
                                                   NumericVector numberOfAcceptedSpatialRhoDraws,
                                                   NumericVector numberOfAllAcceptedSpatialRhoDraws,
                                                   NumericMatrix QSpatialMatrixComponent1,
                                                   NumericVector QSpatialMatrixComponent1EigenValues,
                                                   double a1,
                                                   double b1,
                                                   double xi,
                                                   NumericMatrix omega,
                                                   double a3,
                                                   double b3,
                                                   int currentNumberOfIterations,
                                                   bool betaFixed,
                                                   bool spatialRandomEffectsFixed,
                                                   bool uRandomEffectsFixed,
                                                   bool spatialTauSquaredFixed,
                                                   bool spatialRhoFixed,
                                                   bool varianceCovarianceUFixed,
                                                   bool sigmaSquaredEFixed,
                                                   NumericVector trueBetaValues,
                                                   NumericVector trueSpatialRandomEffectsValues,
                                                   NumericVector trueURandomEffectsValues,
                                                   NumericVector trueSpatialTauSquaredValues,
                                                   NumericVector trueSpatialRhoValues,
                                                   NumericMatrix trueVarianceCovarianceUValues,
                                                   NumericVector trueSigmaSquaredEValues,
                                                   bool centerSpatialRandomEffects,
                                                   bool centerURandomEffects)
  
{
  
  int numberOfRowsInSpatialAssignmentMatrixTripletForm = spatialAssignmentMatrixInTripletForm.rows();
  int numberOfRowsInWTripletForm = WInTripletForm.rows();
  
  if(betaFixed){
    
    beta = trueBetaValues;
    
  } else {
    
    beta = multivariateGaussianNetworkLerouxBetaUpdate(standardizedX,
                                                       y,
                                                       numberOfResponses,
                                                       spatialAssignmentMatrixInTripletForm,
                                                       WInTripletForm,
                                                       numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                       numberOfRowsInWTripletForm,
                                                       beta,
                                                       spatialRandomEffects,
                                                       uRandomEffects,
                                                       sigmaSquaredE,
                                                       covarianceBetaPrior);
    
  }
  
  if(spatialRandomEffectsFixed){
    
    spatialRandomEffects = trueSpatialRandomEffectsValues;
    
  } else {
    
    List output = multivariateGaussianNetworkLerouxSpatialRandomEffectsMHUpdate(standardizedX,
                                                                                 numberOfResponses,
                                                                                 spatialAssignment,
                                                                                 squareSpatialNeighbourhoodMatrix,
                                                                                 y,
                                                                                 squareSpatialNeighbourhoodMatrixInTripletForm,
                                                                                 WInTripletForm,
                                                                                 numberOfRowsInWTripletForm,
                                                                                 beta,
                                                                                 spatialRandomEffects,
                                                                                 uRandomEffects,
                                                                                 spatialTauSquared,
                                                                                 spatialRho,
                                                                                 spatialRandomEffectsTuningParameters,
                                                                                 spatialRandomEffectsAcceptanceRate,
                                                                                 numberOfAcceptedSpatialRandomEffectsDraws,
                                                                                 numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                                                 sigmaSquaredE,
                                                                                 currentNumberOfIterations,
                                                                                 centerSpatialRandomEffects);
    
    spatialRandomEffects = output[0];
    spatialRandomEffectsTuningParameters = output[1];
    spatialRandomEffectsAcceptanceRate = output[2];
    numberOfAcceptedSpatialRandomEffectsDraws = output[3];
    numberOfAllAcceptedSpatialRandomEffectsDraws = output[4];
    
  }
  
  if(uRandomEffectsFixed){
    
    uRandomEffects = trueURandomEffectsValues;
    
  } else {
    
    uRandomEffects = multivariateGaussianNetworkLerouxURandomEffectsUpdate(standardizedX,
                                                                            y,
                                                                            W,
                                                                            numberOfResponses,
                                                                            spatialAssignmentMatrixInTripletForm,
                                                                            WInTripletForm,
                                                                            numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                            numberOfRowsInWTripletForm,
                                                                            beta,
                                                                            spatialRandomEffects,
                                                                            uRandomEffects,
                                                                            varianceCovarianceU,
                                                                            sigmaSquaredE,
                                                                            centerURandomEffects);
    
  }
  
  if(spatialTauSquaredFixed){
    
    spatialTauSquared = trueSpatialTauSquaredValues;
    
  } else {
    
    spatialTauSquared = multivariateGaussianNetworkLerouxTauSquaredUpdate(squareSpatialNeighbourhoodMatrix,
                                                                          spatialRandomEffects,
                                                                          numberOfResponses,
                                                                          spatialTauSquared,
                                                                          spatialRho,
                                                                          a1,
                                                                          b1,
                                                                          QSpatialMatrixComponent1);
    
  }
  
  if(spatialRhoFixed){
    
    spatialRho = trueSpatialRhoValues;
    
  } else {
    
    List output = multivariateGaussianNetworkLerouxRhoUpdate(squareSpatialNeighbourhoodMatrix,
                                                             spatialRandomEffects,
                                                             numberOfResponses,
                                                             spatialTauSquared,
                                                             spatialRho,
                                                             spatialRhoTuningParameters,
                                                             spatialRhoAcceptanceRate,
                                                             numberOfAcceptedSpatialRhoDraws,
                                                             numberOfAllAcceptedSpatialRhoDraws,
                                                             QSpatialMatrixComponent1,
                                                             QSpatialMatrixComponent1EigenValues,
                                                             currentNumberOfIterations);
    
    spatialRho = output[0];
    spatialRhoTuningParameters = output[1];
    spatialRhoAcceptanceRate = output[2];
    numberOfAcceptedSpatialRhoDraws = output[3];
    numberOfAllAcceptedSpatialRhoDraws = output[4];
    
  }
  
  if(varianceCovarianceUFixed){
    
    varianceCovarianceU = trueVarianceCovarianceUValues;
    
  } else {
    
    varianceCovarianceU = multivariateGaussianNetworkLerouxVarianceCovarianceUUpdate(uRandomEffects,
                                                                                     numberOfResponses,
                                                                                     xi,
                                                                                     omega);
  }
  
  if(sigmaSquaredEFixed){
    
    sigmaSquaredE = trueSigmaSquaredEValues;
    
  } else {
    
    sigmaSquaredE = multivariateGaussianNetworkLerouxSigmaSquaredEUpdate(standardizedX,
                                                                         numberOfResponses,
                                                                         y,
                                                                         spatialAssignment,
                                                                         W,
                                                                         spatialAssignmentMatrixInTripletForm,
                                                                         WInTripletForm,
                                                                         numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                         numberOfRowsInWTripletForm,
                                                                         beta,
                                                                         spatialRandomEffects,
                                                                         uRandomEffects,
                                                                         a3,
                                                                         b3);
  }
  
  List fittedValuesAndLikelihood = getMultivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration(standardizedX,
                                                                                                                     y,
                                                                                                                     spatialAssignment,
                                                                                                                     W,
                                                                                                                     beta,
                                                                                                                     spatialRandomEffects,
                                                                                                                     uRandomEffects,
                                                                                                                     sigmaSquaredE,
                                                                                                                     numberOfResponses);
  
  NumericVector fittedValues = fittedValuesAndLikelihood[0];
  NumericVector logLikelihoods = fittedValuesAndLikelihood[1];
  
  
  
  List output(17);
  
  output[0] = beta;
  
  output[1] = spatialRandomEffects;
  output[2] = spatialRandomEffectsTuningParameters;
  output[3] = spatialRandomEffectsAcceptanceRate;
  output[4] = numberOfAcceptedSpatialRandomEffectsDraws;
  output[5] = numberOfAllAcceptedSpatialRandomEffectsDraws;
  
  output[6] = uRandomEffects;
  
  output[7] = spatialTauSquared;
  
  output[8] = spatialRho;
  output[9] = spatialRhoTuningParameters;
  output[10] = spatialRhoAcceptanceRate;
  output[11] = numberOfAcceptedSpatialRhoDraws;
  output[12] = numberOfAllAcceptedSpatialRhoDraws;
  
  output[13] = varianceCovarianceU;
  
  output[14] = sigmaSquaredE;
  
  output[15] = fittedValues;
  output[16] = logLikelihoods;
  
  return output;
  
}
