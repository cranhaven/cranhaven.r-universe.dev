#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List getUnivariatePoissonNetworkDIC(NumericMatrix standardizedX,
                                   NumericVector y,
                                   NumericMatrix W,
                                   NumericMatrix betaSamples,
                                   NumericMatrix uRandomEffectsSamples,
                                   NumericMatrix logLikelihoodSamples)
{
  
  int numberOfBetas = betaSamples.cols();
  int numberOfURandomEffects = uRandomEffectsSamples.cols();
  int numberOfLogLikelihoods = standardizedX.rows();
  int numberOfSamplesPostBurninAndThinning = logLikelihoodSamples.rows();
  
  NumericVector meanOfBetaSamples(numberOfBetas);
  NumericVector meanOfURandomEffectsSamples(numberOfURandomEffects);
  NumericVector meanOfLogLikelihoodSamples(numberOfLogLikelihoods);
  
  double posteriorLogLikelihood = 0.0;
  
  for(int i = 0; i < numberOfBetas; i++) {
    meanOfBetaSamples[i] = getVectorMean(betaSamples(_, i));
  }
  
  for(int i = 0; i < numberOfURandomEffects; i++) {
    meanOfURandomEffectsSamples[i] = getVectorMean(uRandomEffectsSamples(_, i));
  }
  
  NumericVector meanLogTheta = matrixVectorMultiplicationRcpp(standardizedX, meanOfBetaSamples) + matrixVectorMultiplicationRcpp(W, meanOfURandomEffectsSamples);
  
  NumericVector meanFittedValues = getExp(meanLogTheta);
  
  for(int i = 0; i < numberOfLogLikelihoods; i++) {
    posteriorLogLikelihood = posteriorLogLikelihood + R::dpois(y[i], meanFittedValues[i], true);
  }
  
  double DBar = -2 * sumMatrix(logLikelihoodSamples) / numberOfSamplesPostBurninAndThinning;
  double posteriorDeviance = -2 * posteriorLogLikelihood;
  double pd = DBar - posteriorDeviance;
  double DIC = DBar + pd;
  
  List output(4);
  output[0] = DBar;
  output[1] = posteriorDeviance;
  output[2] = pd;
  output[3] = DIC;
  return output;
  
}
