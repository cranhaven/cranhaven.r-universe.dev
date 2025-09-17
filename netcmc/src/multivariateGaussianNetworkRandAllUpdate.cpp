// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

// [[Rcpp::export]]
List multivariateGaussianNetworkRandAllUpdate(NumericMatrix standardizedX,
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
                                                const int numberOfSamples,
                                                const int burnin,
                                                const int thin,
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
  
  NumericMatrix betaSamples((numberOfSamples / thin), standardizedX.cols() * numberOfResponses);
  NumericMatrix vRandomEffectsSamples((numberOfSamples / thin), vRandomEffects.size());
  NumericMatrix uRandomEffectsSamples((numberOfSamples / thin), uRandomEffects.size());
  NumericMatrix varianceCovarianceVSamples(numberOfSamples / thin, (numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2));
  NumericMatrix varianceCovarianceUSamples(numberOfSamples / thin, (numberOfResponses  + (numberOfResponses * numberOfResponses - numberOfResponses) / 2));
  NumericMatrix sigmaSquaredESamples(numberOfSamples / thin, numberOfResponses);
  
  NumericMatrix fittedValuesSamples((numberOfSamples / thin), standardizedX.rows() * numberOfResponses);
  NumericMatrix logLikelihoodsSamples((numberOfSamples / thin), standardizedX.rows() * numberOfResponses);
  
  int sampleCounter = 0;
  
  int totalNumberOfIterations = numberOfSamples + burnin;
  
  Progress progressBar(totalNumberOfIterations, true);
  
  for(int s = 0; s < totalNumberOfIterations; s++){
    
    progressBar.increment();
    
    if (Progress::check_abort()) return  Rcpp::List::create(Rcpp::Named("terminated") = -1);
    
    List output = multivariateGaussianNetworkRandSingleUpdate(standardizedX,
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
                                                                sigmaSquaredE,
                                                                covarianceBetaPrior,
                                                                xiV,
                                                                omegaV,
                                                                xi,
                                                                omega,
                                                                a3,
                                                                b3,
                                                                currentNumberOfIterations,
                                                                betaFixed,
                                                                vRandomEffectsFixed,
                                                                uRandomEffectsFixed,
                                                                varianceCovarianceVFixed,
                                                                varianceCovarianceUFixed,
                                                                sigmaSquaredEFixed,
                                                                trueBetaValues,
                                                                trueVRandomEffectsValues,
                                                                trueURandomEffectsValues,
                                                                trueVarianceCovarianceVValues,
                                                                trueVarianceCovarianceUValues,
                                                                trueSigmaSquaredEValues,
                                                                centerVRandomEffects,
                                                                centerURandomEffects);
    
    beta = output[0];
    vRandomEffects = output[1];
    uRandomEffects = output[2];
    varianceCovarianceV = as<NumericMatrix>(output[3]);
    varianceCovarianceU = as<NumericMatrix>(output[4]);
    sigmaSquaredE = output[5];
    
    NumericVector fittedValues = output[6];
    NumericVector logLikelihoods = output[7];
    
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

      sigmaSquaredESamples(sampleCounter, _) = sigmaSquaredE;

      fittedValuesSamples(sampleCounter, _) = fittedValues;
      logLikelihoodsSamples(sampleCounter, _) = logLikelihoods;

      sampleCounter++;
      
    }
    
    currentNumberOfIterations++;
    
  }
  
  List computationOfDIC = getMultivariateGaussianNetworkLerouxDIC(standardizedX,
                                                                 y,
                                                                 V,
                                                                 W,
                                                                 betaSamples,
                                                                 vRandomEffectsSamples,
                                                                 uRandomEffectsSamples,
                                                                 sigmaSquaredESamples,
                                                                 logLikelihoodsSamples,
                                                                 numberOfResponses);


  double DBar = computationOfDIC[0];
  double posteriorDeviance = computationOfDIC[1];
  double pd = computationOfDIC[2];
  double DIC = computationOfDIC[3];
  
  List modelOutput(12);
  
  modelOutput[0] = betaSamples;
  modelOutput[1] = vRandomEffectsSamples;
  modelOutput[2] = uRandomEffectsSamples;
  modelOutput[3] = varianceCovarianceVSamples;
  modelOutput[4] = varianceCovarianceUSamples;
  modelOutput[5] = sigmaSquaredESamples;
  
  modelOutput[6] = fittedValuesSamples;
  modelOutput[7] = logLikelihoodsSamples;

  modelOutput[8] = DBar;
  modelOutput[9] = posteriorDeviance;
  modelOutput[10] = pd;
  modelOutput[11] = DIC;

  return modelOutput;
  
}
