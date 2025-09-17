// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

// [[Rcpp::export]]
List multivariateBinomialNetworkRandAllUpdate(NumericMatrix standardizedX,
                                                  NumericVector trials,
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
                                                  const int numberOfSamples,
                                                  const int burnin,
                                                  const int thin,
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
  
  NumericMatrix betaSamples((numberOfSamples / thin), standardizedX.cols() * numberOfResponses);
  NumericMatrix vRandomEffectsSamples((numberOfSamples / thin), vRandomEffects.size());
  NumericMatrix uRandomEffectsSamples((numberOfSamples / thin), uRandomEffects.size());
  NumericMatrix varianceCovarianceVSamples(numberOfSamples / thin, (numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2));
  NumericMatrix varianceCovarianceUSamples(numberOfSamples / thin, (numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2));
  
  NumericMatrix fittedValuesSamples((numberOfSamples / thin), standardizedX.rows() * numberOfResponses);
  NumericMatrix logLikelihoodsSamples((numberOfSamples / thin), standardizedX.rows() * numberOfResponses);
  
  int sampleCounter = 0;
  
  int totalNumberOfIterations = numberOfSamples + burnin;
  
  Progress progressBar(totalNumberOfIterations, true);
  
  for(int s = 0; s < totalNumberOfIterations; s++){
    
    progressBar.increment();
    
    if (Progress::check_abort()) return  Rcpp::List::create(Rcpp::Named("terminated") = -1);
    
    List output = multivariateBinomialNetworkRandSingleUpdate(standardizedX,
                                                               trials,
                                                               y,
                                                               numberOfResponses,
                                                               V,
                                                               W,
                                                               VInTripletForm,
                                                               WInTripletForm,
                                                               beta,
                                                               vRandomEffects,
                                                               uRandomEffects,
                                                               varianceCovarianceV,
                                                               varianceCovarianceU,
                                                               covarianceBetaPrior,
                                                               numberOfBetaBlocks,
                                                               maxBetaBlockSize,
                                                               betaTuningParameter,
                                                               betaAcceptanceRate,
                                                               numberOfAcceptedBetaDraws,
                                                               numberOfAllAcceptedBetaDraws,
                                                               vRandomEffectsTuningParameters,
                                                               vRandomEffectsAcceptanceRate,
                                                               numberOfAcceptedVREDraws,
                                                               numberOfAllAcceptedVREDraws,
                                                               uRandomEffectsTuningParameters,
                                                               uRandomEffectsAcceptanceRate,
                                                               numberOfAcceptedUREDraws,
                                                               numberOfAllAcceptedUREDraws,
                                                               xiV,
                                                               omegaV,
                                                               xi,
                                                               omega,
                                                               currentNumberOfIterations,
                                                               betaFixed,
                                                               vRandomEffectsFixed,
                                                               uRandomEffectsFixed,
                                                               varianceCovarianceVFixed,
                                                               varianceCovarianceUFixed,
                                                               trueBetaValues,
                                                               trueVRandomEffectsValues,
                                                               trueURandomEffectsValues,
                                                               trueVarianceCovarianceVValues,
                                                               trueVarianceCovarianceUValues,
                                                               centerVRandomEffects,
                                                               centerURandomEffects);
    
    beta = output[0];
    betaTuningParameter = output[1];
    betaAcceptanceRate = output[2];
    numberOfAcceptedBetaDraws = output[3];
    numberOfAllAcceptedBetaDraws = output[4];
    
    vRandomEffects = output[5];
    vRandomEffectsTuningParameters = output[6];
    vRandomEffectsAcceptanceRate = output[7];
    numberOfAcceptedVREDraws = output[8];
    numberOfAllAcceptedVREDraws = output[9];
    
    uRandomEffects = output[10];
    uRandomEffectsTuningParameters = output[11];
    uRandomEffectsAcceptanceRate = output[12];
    numberOfAcceptedUREDraws = output[13];
    numberOfAllAcceptedUREDraws = output[14];
  
    varianceCovarianceV = as<NumericMatrix>(output[15]);
  
    varianceCovarianceU = as<NumericMatrix>(output[16]);
    
    NumericVector fittedValues = output[17];
    NumericVector logLikelihoods = output[18];
    
    if((currentNumberOfIterations > burnin) && (currentNumberOfIterations % thin == 0)){
      
      betaSamples(sampleCounter, _) = beta;
      vRandomEffectsSamples(sampleCounter, _) = vRandomEffects;
      uRandomEffectsSamples(sampleCounter, _) = uRandomEffects;
      
      int varianceCovarianceSampleIndex1 = 0;
      NumericVector varianceCovarianceVCurrentSamples(numberOfResponses + (numberOfResponses * numberOfResponses - numberOfResponses) / 2);
      for(int j = 0; j < numberOfResponses; j++) {
        for(int z = j; z < numberOfResponses; z++) {
          varianceCovarianceVCurrentSamples[varianceCovarianceSampleIndex1] = varianceCovarianceV(j, z);
          varianceCovarianceSampleIndex1 ++;
        }
      }
      varianceCovarianceVSamples(sampleCounter, _) = varianceCovarianceVCurrentSamples;
      
      int varianceCovarianceSampleIndex = 0;
      NumericVector varianceCovarianceUCurrentSamples(numberOfResponses + (numberOfResponses * numberOfResponses - numberOfResponses) / 2);
      for(int j = 0; j < numberOfResponses; j++) {
        for(int z = j; z < numberOfResponses; z++) {
          varianceCovarianceUCurrentSamples[varianceCovarianceSampleIndex] = varianceCovarianceU(j, z);
          varianceCovarianceSampleIndex ++;
        }
      }
      varianceCovarianceUSamples(sampleCounter, _) = varianceCovarianceUCurrentSamples;
      
      fittedValuesSamples(sampleCounter, _) = fittedValues;
      logLikelihoodsSamples(sampleCounter, _) = logLikelihoods;
      
      sampleCounter++;
      
    }
    
    currentNumberOfIterations++;
    
  }
  
  List computationOfDIC = getMultivariateBinomialNetworkLerouxDIC(standardizedX,
                                                                   trials,
                                                                   y,
                                                                   V,
                                                                   W,
                                                                   betaSamples,
                                                                   vRandomEffectsSamples,
                                                                   uRandomEffectsSamples,
                                                                   logLikelihoodsSamples,
                                                                   numberOfResponses);
  
  
  double DBar = computationOfDIC[0];
  double posteriorDeviance = computationOfDIC[1];
  double pd = computationOfDIC[2];
  double DIC = computationOfDIC[3];
  
  List modelOutput(20);
  
  modelOutput[0] = betaSamples;
  modelOutput[1] = vRandomEffectsSamples;
  modelOutput[2] = uRandomEffectsSamples;
  modelOutput[3] = varianceCovarianceVSamples;
  modelOutput[4] = varianceCovarianceUSamples;
  
  modelOutput[5] = betaAcceptanceRate;
  modelOutput[6] = vRandomEffectsAcceptanceRate;
  modelOutput[7] = uRandomEffectsAcceptanceRate;

  modelOutput[8] = betaTuningParameter;
  modelOutput[9] = vRandomEffectsTuningParameters;
  modelOutput[10] = uRandomEffectsTuningParameters;

  modelOutput[11] = fittedValuesSamples;
  modelOutput[12] = logLikelihoodsSamples;
  
  modelOutput[13] = DBar;
  modelOutput[14] = posteriorDeviance;
  modelOutput[15] = pd;
  modelOutput[16] = DIC;
  
  return modelOutput;
  
}
