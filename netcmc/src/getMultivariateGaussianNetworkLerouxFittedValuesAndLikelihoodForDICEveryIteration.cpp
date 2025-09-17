#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List getMultivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration(NumericMatrix standardizedX,
                                                                                       NumericVector y,
                                                                                       NumericMatrix spatialAssignment,
                                                                                       NumericMatrix W,
                                                                                       NumericVector beta,
                                                                                       NumericVector spatialRandomEffects,
                                                                                       NumericVector uRandomEffects,
                                                                                       NumericVector sigmaSquaredE,
                                                                                       int numberOfResponses)
{
  
  int numberOfLoglikelihoods = (standardizedX.rows() * numberOfResponses);
  NumericVector theta(numberOfLoglikelihoods);
  NumericVector logLikelihoods(numberOfLoglikelihoods);
  
  int numberOfColumnsInX = standardizedX.cols();
  int numberOfColumnsInV = spatialAssignment.cols();
  int numberOfColumnsInW = W.cols();
  
  for(int i = 0; i < numberOfResponses; i++) {
    theta[Range(i * standardizedX.rows(), ((i + 1) * standardizedX.rows()) - 1)] = matrixVectorMultiplicationRcpp(standardizedX, getSubvector(beta, i * numberOfColumnsInX, ((i + 1) * numberOfColumnsInX) - 1))
    + matrixVectorMultiplicationRcpp(spatialAssignment, getSubvector(spatialRandomEffects, i * numberOfColumnsInV, ((i + 1) * numberOfColumnsInV) - 1))
    + matrixVectorMultiplicationRcpp(W, getSubvector(uRandomEffects, i * numberOfColumnsInW, ((i + 1) * numberOfColumnsInW) - 1));
  }
  
  NumericVector fittedValues = theta;
  
  for(int r = 0; r < numberOfResponses; r++) {
    for(int i = 0; i < standardizedX.rows(); i++) {
      logLikelihoods[i + (r * standardizedX.rows())] = R::dnorm(y[i + (r * standardizedX.rows())], fittedValues[i + (r * standardizedX.rows())], sqrt(sigmaSquaredE[r]), true);
    }
  }
  
  List output(2);
  output[0] = fittedValues;
  output[1] = logLikelihoods;
  return output;
  
}
