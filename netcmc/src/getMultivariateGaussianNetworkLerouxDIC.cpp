#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List getMultivariateGaussianNetworkLerouxDIC(NumericMatrix standardizedX,
                                             NumericVector y,
                                             NumericMatrix spatialAssignment,
                                             NumericMatrix W,
                                             NumericMatrix betaSamples,
                                             NumericMatrix spatialRandomEffectsSamples,
                                             NumericMatrix uRandomEffectsSamples,
                                             NumericMatrix sigmaSquaredESamples,
                                             NumericMatrix logLikelihoodSamples,
                                             int numberOfResponses)
{
  
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInV = spatialAssignment.cols();
  int numberOfColumnsInW = W.cols();
  
  int numberOfBetas = betaSamples.cols();
  int numberOfSpatialRandomEffects = spatialRandomEffectsSamples.cols();
  int numberOfURandomEffects = uRandomEffectsSamples.cols();
  int numberOfLogLikelihoods = standardizedX.rows() * numberOfResponses;
  int numberOfSamplesPostBurninAndThinning = logLikelihoodSamples.rows();
  
  NumericVector meanOfBetaSamples(numberOfBetas);
  NumericVector meanOfSpatialRandomEffectsSamples(numberOfSpatialRandomEffects);
  NumericVector meanOfURandomEffectsSamples(numberOfURandomEffects);
  NumericVector meanOfSigmaSquaredESamples(numberOfResponses);
  NumericVector meanTheta(numberOfLogLikelihoods);
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
  
  for(int i = 0; i < numberOfResponses; i++) {
    meanOfSigmaSquaredESamples[i] = getVectorMean(sigmaSquaredESamples(_, i));
  }
  
  for(int i = 0; i < numberOfResponses; i++) {
    meanTheta[Range(i * standardizedX.rows(), ((i + 1) * standardizedX.rows()) - 1)] = matrixVectorMultiplicationRcpp(standardizedX, getSubvector(meanOfBetaSamples, i * numberOfColumnsInX, ((i + 1) * numberOfColumnsInX) - 1))
    + matrixVectorMultiplicationRcpp(spatialAssignment, getSubvector(meanOfSpatialRandomEffectsSamples, i * numberOfColumnsInV, ((i + 1) * numberOfColumnsInV) - 1))
    + matrixVectorMultiplicationRcpp(W, getSubvector(meanOfURandomEffectsSamples, i * numberOfColumnsInW, ((i + 1) * numberOfColumnsInW) - 1));
  }
  
  NumericVector meanFittedValues = meanTheta;
  
  for(int r = 0; r < numberOfResponses; r++) {
    for(int i = 0; i < standardizedX.rows(); i++) {
      posteriorLogLikelihood = posteriorLogLikelihood + R::dnorm(y[i + (r * standardizedX.rows())], meanFittedValues[i + (r * standardizedX.rows())], sqrt(meanOfSigmaSquaredESamples[r]), true);
    }
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
