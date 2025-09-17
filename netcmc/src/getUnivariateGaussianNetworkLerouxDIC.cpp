#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List getUnivariateGaussianNetworkLerouxDIC(NumericMatrix standardizedX,
                                           NumericVector y,
                                           NumericMatrix spatialAssignment,
                                           NumericMatrix W,
                                           NumericMatrix betaSamples,
                                           NumericMatrix spatialRandomEffectsSamples,
                                           NumericMatrix uRandomEffectsSamples,
                                           NumericVector sigmaSquaredESamples,
                                           NumericMatrix logLikelihoodSamples)
{
  
  int numberOfBetas = betaSamples.cols();
  int numberOfSpatialRandomEffects = spatialRandomEffectsSamples.cols();
  int numberOfURandomEffects = uRandomEffectsSamples.cols();
  int numberOfLogLikelihoods = standardizedX.rows();
  int numberOfSamplesPostBurninAndThinning = logLikelihoodSamples.rows();
  
  NumericVector meanOfBetaSamples(numberOfBetas);
  NumericVector meanOfSpatialRandomEffectsSamples(numberOfSpatialRandomEffects);
  NumericVector meanOfURandomEffectsSamples(numberOfURandomEffects);
  NumericVector meanOfLogLikelihoodSamples(numberOfLogLikelihoods);
  
  double posteriorLogLikelihood = 0.0;
  
  for(int i = 0; i < numberOfBetas; i++) {
    meanOfBetaSamples[i] = getVectorMean(betaSamples(_, i));
  }
  
  for(int i = 0; i < numberOfSpatialRandomEffects; i++) {
    meanOfSpatialRandomEffectsSamples[i] = getVectorMean(spatialRandomEffectsSamples(_, i));
  }
  
  for(int i = 0; i < numberOfURandomEffects; i++) {
    meanOfURandomEffectsSamples[i] = getVectorMean(uRandomEffectsSamples(_, i));
  }
  
  NumericVector meanTheta = matrixVectorMultiplicationRcpp(standardizedX, meanOfBetaSamples) 
                          + matrixVectorMultiplicationRcpp(spatialAssignment, meanOfSpatialRandomEffectsSamples)
                          + matrixVectorMultiplicationRcpp(W, meanOfURandomEffectsSamples);
  
  NumericVector meanFittedValues = meanTheta;
  
  double meanSigmaSquaredE = getVectorMean(sigmaSquaredESamples);
  
  for(int i = 0; i < numberOfLogLikelihoods; i++) {
    posteriorLogLikelihood = posteriorLogLikelihood + R::dnorm(y[i], meanFittedValues[i], sqrt(meanSigmaSquaredE), true);
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
