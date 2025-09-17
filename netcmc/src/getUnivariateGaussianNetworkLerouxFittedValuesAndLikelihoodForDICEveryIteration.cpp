#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List getUnivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration(NumericMatrix standardizedX,
                                                                                     NumericVector y,
                                                                                     NumericMatrix spatialAssignment,
                                                                                     NumericMatrix W,
                                                                                     NumericVector beta,
                                                                                     NumericVector spatialRandomEffects,
                                                                                     NumericVector uRandomEffects,
                                                                                     double sigmaSquaredE)
{
  
  int numberOfLoglikelihoods = standardizedX.rows();
  NumericVector logLikelihoods(numberOfLoglikelihoods);
  
  NumericVector theta = matrixVectorMultiplicationRcpp(standardizedX, beta)
                      + matrixVectorMultiplicationRcpp(spatialAssignment, spatialRandomEffects)
                      + matrixVectorMultiplicationRcpp(W, uRandomEffects);
                           
  NumericVector fittedValues = theta;
  
  for(int i = 0; i < numberOfLoglikelihoods; i++) {
    logLikelihoods[i] = R::dnorm(y[i], fittedValues[i], sqrt(sigmaSquaredE), true);
  }
  
  List output(2);
  output[0] = fittedValues;
  output[1] = logLikelihoods;
  return output;
  
}
