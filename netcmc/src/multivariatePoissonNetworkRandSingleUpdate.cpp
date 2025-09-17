#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List multivariatePoissonNetworkRandSingleUpdate(NumericMatrix standardizedX,
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
                                                     double covarianceBetaPrior,
                                                     const int numberOfBetaBlocks,
                                                     const int maxBetaBlockSize,
                                                     NumericVector betaTuningParameter,
                                                     NumericVector betaAcceptanceRate,
                                                     NumericVector numberOfAcceptedBetaDraws,
                                                     NumericVector numberOfAllAcceptedBetaDraws,
                                                     NumericVector vRandomEffectsTuningParameters,
                                                     NumericVector vRandomEffectsAcceptanceRate,
                                                     NumericVector numberOfAcceptedVREDraws,
                                                     NumericVector numberOfAllAcceptedVREDraws,
                                                     NumericVector uRandomEffectsTuningParameters,
                                                     NumericVector uRandomEffectsAcceptanceRate,
                                                     NumericVector numberOfAcceptedUREDraws,
                                                     NumericVector numberOfAllAcceptedUREDraws,
                                                     double xiV,
                                                     NumericMatrix omegaV,
                                                     double xi,
                                                     NumericMatrix omega,
                                                     int currentNumberOfIterations,
                                                     bool betaFixed,
                                                     bool vRandomEffectsFixed,
                                                     bool uRandomEffectsFixed,
                                                     bool varianceCovarianceVFixed,
                                                     bool varianceCovarianceUFixed,
                                                     NumericVector trueBetaValues,
                                                     NumericVector trueVRandomEffectsValues,
                                                     NumericVector trueURandomEffectsValues,
                                                     NumericMatrix trueVarianceCovarianceVValues,
                                                     NumericMatrix trueVarianceCovarianceUValues,
                                                     bool centerVRandomEffects,
                                                     bool centerURandomEffects)
  
{

  int numberOfRowsInVTripletForm = VInTripletForm.rows();
  int numberOfRowsInWTripletForm = WInTripletForm.rows();
  
  if(betaFixed){
    
    beta = trueBetaValues;
    
  } else {
    
    List output = multivariatePoissonNetworkLerouxBetaUpdate(standardizedX,
                                                               y,
                                                               numberOfResponses,
                                                               VInTripletForm,
                                                               WInTripletForm,
                                                               numberOfRowsInVTripletForm,
                                                               numberOfRowsInWTripletForm,
                                                               beta,
                                                               vRandomEffects,
                                                               uRandomEffects,
                                                               covarianceBetaPrior,
                                                               numberOfBetaBlocks,
                                                               maxBetaBlockSize,
                                                               betaTuningParameter,
                                                               betaAcceptanceRate,
                                                               numberOfAcceptedBetaDraws,
                                                               numberOfAllAcceptedBetaDraws,
                                                               currentNumberOfIterations);
    
    beta = output[0];
    betaTuningParameter = output[1];
    betaAcceptanceRate = output[2];
    numberOfAcceptedBetaDraws = output[3];
    numberOfAllAcceptedBetaDraws = output[4];
    
  }
  
  if(vRandomEffectsFixed){
    
    vRandomEffects = trueVRandomEffectsValues;
    
  } else {
    
    List output = multivariatePoissonNetworkRandVRandomEffectsUpdate(standardizedX,
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
                                                                       vRandomEffectsTuningParameters,
                                                                       vRandomEffectsAcceptanceRate,
                                                                       numberOfAcceptedVREDraws,
                                                                       numberOfAllAcceptedVREDraws,
                                                                       currentNumberOfIterations,
                                                                       centerVRandomEffects);
    
    vRandomEffects = output[0];
    vRandomEffectsTuningParameters = output[1];
    vRandomEffectsAcceptanceRate = output[2];
    numberOfAcceptedVREDraws = output[3];
    numberOfAllAcceptedVREDraws = output[4];
    
    
  }
  
  if(uRandomEffectsFixed){
    
    uRandomEffects = trueURandomEffectsValues;
    
  } else {
    
    List output = multivariatePoissonNetworkLerouxURandomEffectsUpdate(standardizedX,
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
                                                                         uRandomEffectsTuningParameters,
                                                                         uRandomEffectsAcceptanceRate,
                                                                         numberOfAcceptedUREDraws,
                                                                         numberOfAllAcceptedUREDraws,
                                                                         currentNumberOfIterations,
                                                                         centerURandomEffects);
    
    uRandomEffects = output[0];
    uRandomEffectsTuningParameters = output[1];
    uRandomEffectsAcceptanceRate = output[2];
    numberOfAcceptedUREDraws = output[3];
    numberOfAllAcceptedUREDraws = output[4];
    
  }
  
  if(varianceCovarianceVFixed){
    
    varianceCovarianceV = trueVarianceCovarianceVValues;
    
  } else {
    
    varianceCovarianceV = multivariatePoissonNetworkLerouxVarianceCovarianceUUpdate(vRandomEffects,
                                                                                      numberOfResponses,
                                                                                      xiV,
                                                                                      omegaV);
  }
  
  if(varianceCovarianceUFixed){
    
    varianceCovarianceU = trueVarianceCovarianceUValues;
    
  } else {
    
    varianceCovarianceU = multivariatePoissonNetworkLerouxVarianceCovarianceUUpdate(uRandomEffects,
                                                                                      numberOfResponses,
                                                                                      xi,
                                                                                      omega);
  }
  
  List fittedValuesAndLikelihood = getMultivariatePoissonNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration(standardizedX,
                                                                                                                       y,
                                                                                                                       V,
                                                                                                                       W,
                                                                                                                       beta,
                                                                                                                       vRandomEffects,
                                                                                                                       uRandomEffects,
                                                                                                                       numberOfResponses);
  
  NumericVector fittedValues = fittedValuesAndLikelihood[0];
  NumericVector logLikelihoods = fittedValuesAndLikelihood[1];
  
  
  
  List output(19);
  
  output[0] = beta;
  output[1] = betaTuningParameter;
  output[2] = betaAcceptanceRate;
  output[3] = numberOfAcceptedBetaDraws;
  output[4] = numberOfAllAcceptedBetaDraws;
  
  output[5] = vRandomEffects;
  output[6] = vRandomEffectsTuningParameters;
  output[7] = vRandomEffectsAcceptanceRate;
  output[8] = numberOfAcceptedVREDraws;
  output[9] = numberOfAllAcceptedVREDraws;
  
  output[10] = uRandomEffects;
  output[11] = uRandomEffectsTuningParameters;
  output[12] = uRandomEffectsAcceptanceRate;
  output[13] = numberOfAcceptedUREDraws;
  output[14] = numberOfAllAcceptedUREDraws;
  
  output[15] = varianceCovarianceV;
  
  output[16] = varianceCovarianceU;
  
  output[17] = fittedValues;
  output[18] = logLikelihoods;
  
  return output;
  
}
