#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List univariateGaussianNetworkLerouxSingleMHUpdate(NumericMatrix standardizedX,
                                                 NumericVector y,
                                                 NumericMatrix squareSpatialNeighbourhoodMatrix,
                                                 NumericMatrix spatialAssignment,
                                                 NumericMatrix W,
                                                 NumericMatrix squareSpatialNeighbourhoodMatrixInTripletForm,
                                                 NumericMatrix spatialAssignmentMatrixInTripletForm,
                                                 NumericMatrix WInTripletForm,
                                                 NumericVector beta,
                                                 NumericVector spatialRandomEffects,
                                                 NumericVector uRandomEffects,
                                                 double spatialTauSquared,
                                                 double spatialRho,
                                                 double sigmaSquaredU,
                                                 double sigmaSquaredE,
                                                 double covarianceBetaPrior,
                                                 NumericVector spatialRandomEffectsTuningParameters,
                                                 NumericVector spatialRandomEffectsAcceptanceRate,
                                                 NumericVector numberOfAcceptedSpatialRandomEffectsDraws,
                                                 NumericVector numberOfAllAcceptedSpatialRandomEffectsDraws,
                                                 double spatialRhoTuningParameters,
                                                 double spatialRhoAcceptanceRate,
                                                 int numberOfAcceptedSpatialRhoDraws,
                                                 int numberOfAllAcceptedSpatialRhoDraws,
                                                 NumericMatrix QSpatialMatrixComponent1,
                                                 NumericVector QSpatialMatrixComponent1EigenValues,
                                                 double a1,
                                                 double b1,
                                                 double a2,
                                                 double b2,
                                                 double a3,
                                                 double b3,
                                                 int currentNumberOfIterations,
                                                 bool betaFixed,
                                                 bool spatialRandomEffectsFixed,
                                                 bool uRandomEffectsFixed,
                                                 bool spatialTauSquaredFixed,
                                                 bool spatialRhoFixed,
                                                 bool sigmaSquaredUFixed,
                                                 bool sigmaSquaredEFixed,
                                                 NumericVector trueBetaValues,
                                                 NumericVector trueSpatialRandomEffectsValues,
                                                 NumericVector trueURandomEffectsValues,
                                                 double trueSpatialTauSquaredValues,
                                                 double trueSpatialRhoValues,
                                                 double trueSigmaSquaredUValues,
                                                 double trueSigmaSquaredEValues,
                                                 bool centerSpatialRandomEffects,
                                                 bool centerURandomEffects)
  
{
  
  int numberOfRowsInX = standardizedX.rows();
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfRowsInSpatialAssignmentMatrixTripletForm = spatialAssignmentMatrixInTripletForm.rows();
  int numberOfRowsInWTripletForm = WInTripletForm.rows();
  
  if(betaFixed){
    
    beta = trueBetaValues;
    
  } else {
    
    NumericVector output = univariateGaussianNetworkLerouxBetaUpdate(standardizedX,
                                                                     y,
                                                                     numberOfRowsInX,
                                                                     numberOfColumnsInX,
                                                                     spatialAssignmentMatrixInTripletForm,
                                                                     WInTripletForm,
                                                                     numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                     numberOfRowsInWTripletForm,
                                                                     beta,
                                                                     spatialRandomEffects,
                                                                     uRandomEffects,
                                                                     sigmaSquaredE,
                                                                     covarianceBetaPrior);
    
    beta = output;
    
  }
  
  if(spatialRandomEffectsFixed){
    
    spatialRandomEffects = trueSpatialRandomEffectsValues;
    
  } else {
    
    List output = univariateGaussianNetworkLerouxSpatialRandomEffectsMHUpdate(standardizedX,
                                                                              spatialAssignment,
                                                                              squareSpatialNeighbourhoodMatrix,
                                                                              y,
                                                                              numberOfRowsInX,
                                                                              numberOfColumnsInX,
                                                                              squareSpatialNeighbourhoodMatrixInTripletForm,
                                                                              WInTripletForm,
                                                                              numberOfRowsInWTripletForm,
                                                                              beta,
                                                                              spatialRandomEffects,
                                                                              uRandomEffects,
                                                                              spatialTauSquared,
                                                                              spatialRho,
                                                                              sigmaSquaredE,
                                                                              spatialRandomEffectsTuningParameters,
                                                                              spatialRandomEffectsAcceptanceRate,
                                                                              numberOfAcceptedSpatialRandomEffectsDraws,
                                                                              numberOfAllAcceptedSpatialRandomEffectsDraws,
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
    
    NumericVector output = univariateGaussianNetworkLerouxURandomEffectsUpdate(standardizedX,
                                                                               W,
                                                                               y,
                                                                               numberOfRowsInX,
                                                                               spatialAssignmentMatrixInTripletForm,
                                                                               WInTripletForm,
                                                                               numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                               numberOfRowsInWTripletForm,
                                                                               beta,
                                                                               spatialRandomEffects,
                                                                               uRandomEffects,
                                                                               sigmaSquaredU,
                                                                               sigmaSquaredE, 
                                                                               centerURandomEffects);
    
    uRandomEffects = output;
    
  }
  
  if(spatialTauSquaredFixed){
    
    spatialTauSquared = trueSpatialTauSquaredValues;
    
  } else {
    
    double output = univariateGaussianNetworkLerouxTauSquaredUpdate(squareSpatialNeighbourhoodMatrix,
                                                                    spatialRandomEffects,
                                                                    spatialTauSquared,
                                                                    spatialRho,
                                                                    a1,
                                                                    b1,
                                                                    QSpatialMatrixComponent1);
    
    spatialTauSquared = output;
    
  }
  
  if(spatialRhoFixed){
    
    spatialRho = trueSpatialRhoValues;
    
  } else {
    
    List output = univariateGaussianNetworkLerouxRhoUpdate(squareSpatialNeighbourhoodMatrix,
                                                           spatialRandomEffects,
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
  
  
  if(sigmaSquaredUFixed){
    
    sigmaSquaredU = trueSigmaSquaredUValues;
    
  } else {
    
    sigmaSquaredU = univariateGaussianNetworkLerouxSigmaSquaredUUpdate(uRandomEffects,
                                                                       a2,
                                                                       b2);
  }
  
  if(sigmaSquaredEFixed){
    
    sigmaSquaredE = trueSigmaSquaredEValues;
    
  } else {
    
    sigmaSquaredE = univariateGaussianNetworkLerouxSigmaSquaredEUpdate(standardizedX,
                                                                       y,
                                                                       spatialAssignment,
                                                                       W,
                                                                       spatialAssignmentMatrixInTripletForm,
                                                                       WInTripletForm,
                                                                       numberOfRowsInX,
                                                                       numberOfRowsInSpatialAssignmentMatrixTripletForm,
                                                                       numberOfRowsInWTripletForm,
                                                                       beta,
                                                                       spatialRandomEffects,
                                                                       uRandomEffects,
                                                                       a3,
                                                                       b3);
  }
  
  List fittedValuesAndLikelihood = getUnivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration(standardizedX,
                                                                                                                   y,
                                                                                                                   spatialAssignment,
                                                                                                                   W,
                                                                                                                   beta,
                                                                                                                   spatialRandomEffects,
                                                                                                                   uRandomEffects,
                                                                                                                   sigmaSquaredE);
  
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
  
  output[13] = sigmaSquaredU;
  
  output[14] = sigmaSquaredE;
  
  output[15] = fittedValues;
  output[16] = logLikelihoods;
  
  return output;
  
}
