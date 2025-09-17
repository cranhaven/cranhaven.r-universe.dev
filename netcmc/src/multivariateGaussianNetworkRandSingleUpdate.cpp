#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List multivariateGaussianNetworkRandSingleUpdate(NumericMatrix standardizedX,
                                                   NumericVector y,
                                                   const int numberOfResponses,
                                                   NumericMatrix V,
                                                   NumericMatrix W,
                                                   NumericMatrix VInTripletForm,
                                                   NumericMatrix WInTripletForm,
                                                   NumericVector beta,
                                                   NumericVector vRandomEffects,
                                                   NumericVector uRandomEffects,
                                                   NumericMatrix varianceCovarianceV,
                                                   NumericMatrix varianceCovarianceU,
                                                   NumericVector sigmaSquaredE,
                                                   double covarianceBetaPrior,
                                                   double xiV,
                                                   NumericMatrix omegaV,
                                                   double xi,
                                                   NumericMatrix omega,
                                                   double a3,
                                                   double b3,
                                                   int currentNumberOfIterations,
                                                   bool betaFixed,
                                                   bool vRandomEffectsFixed,
                                                   bool uRandomEffectsFixed,
                                                   bool varianceCovarianceVFixed,
                                                   bool varianceCovarianceUFixed,
                                                   bool sigmaSquaredEFixed,
                                                   NumericVector trueBetaValues,
                                                   NumericVector trueVRandomEffectsValues,
                                                   NumericVector trueURandomEffectsValues,
                                                   NumericMatrix trueVarianceCovarianceVValues,
                                                   NumericMatrix trueVarianceCovarianceUValues,
                                                   NumericVector trueSigmaSquaredEValues,
                                                   bool centerVRandomEffects,
                                                   bool centerURandomEffects)
  
{
  
  int numberOfRowsInVTripletForm = VInTripletForm.rows();
  int numberOfRowsInWTripletForm = WInTripletForm.rows();
  
  if(betaFixed){
    
    beta = trueBetaValues;
    
  } else {
    
    beta = multivariateGaussianNetworkLerouxBetaUpdate(standardizedX,
                                                       y,
                                                       numberOfResponses,
                                                       VInTripletForm,
                                                       WInTripletForm,
                                                       numberOfRowsInVTripletForm,
                                                       numberOfRowsInWTripletForm,
                                                       beta,
                                                       vRandomEffects,
                                                       uRandomEffects,
                                                       sigmaSquaredE,
                                                       covarianceBetaPrior);
    
  }
  
  if(vRandomEffectsFixed){
    
    vRandomEffects = trueVRandomEffectsValues;
    
  } else {
    
    vRandomEffects = multivariateGaussianNetworkRandVRandomEffectsUpdate(standardizedX,
                                                                         y,
                                                                         V,
                                                                         W,
                                                                         numberOfResponses,
                                                                         WInTripletForm,
                                                                         numberOfRowsInWTripletForm,
                                                                         beta,
                                                                         vRandomEffects,
                                                                         uRandomEffects,
                                                                         varianceCovarianceV,
                                                                         sigmaSquaredE,
                                                                         centerVRandomEffects);
    
  }
  
  if(uRandomEffectsFixed){
    
    uRandomEffects = trueURandomEffectsValues;
    
  } else {
    
    uRandomEffects = multivariateGaussianNetworkLerouxURandomEffectsUpdate(standardizedX,
                                                                            y,
                                                                            W,
                                                                            numberOfResponses,
                                                                            VInTripletForm,
                                                                            WInTripletForm,
                                                                            numberOfRowsInVTripletForm,
                                                                            numberOfRowsInWTripletForm,
                                                                            beta,
                                                                            vRandomEffects,
                                                                            uRandomEffects,
                                                                            varianceCovarianceU,
                                                                            sigmaSquaredE,
                                                                            centerURandomEffects);
    
  }
  
  if(varianceCovarianceVFixed){
    
    varianceCovarianceV = trueVarianceCovarianceVValues;
    
  } else {
    
    varianceCovarianceV = multivariateGaussianNetworkLerouxVarianceCovarianceUUpdate(vRandomEffects,
                                                                                     numberOfResponses,
                                                                                     xiV,
                                                                                     omegaV);
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
                                                                         V,
                                                                         W,
                                                                         VInTripletForm,
                                                                         WInTripletForm,
                                                                         numberOfRowsInVTripletForm,
                                                                         numberOfRowsInWTripletForm,
                                                                         beta,
                                                                         vRandomEffects,
                                                                         uRandomEffects,
                                                                         a3,
                                                                         b3);
  }
  
  List fittedValuesAndLikelihood = getMultivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration(standardizedX,
                                                                                                                     y,
                                                                                                                     V,
                                                                                                                     W,
                                                                                                                     beta,
                                                                                                                     vRandomEffects,
                                                                                                                     uRandomEffects,
                                                                                                                     sigmaSquaredE,
                                                                                                                     numberOfResponses);
  
  NumericVector fittedValues = fittedValuesAndLikelihood[0];
  NumericVector logLikelihoods = fittedValuesAndLikelihood[1];
  
  
  
  List output(8);
  
  output[0] = beta;
  output[1] = vRandomEffects;
  output[2] = uRandomEffects;
  output[3] = varianceCovarianceV;
  output[4] = varianceCovarianceU;
  output[5] = sigmaSquaredE;
  
  output[6] = fittedValues;
  output[7] = logLikelihoods;
  
  return output;
  
}
