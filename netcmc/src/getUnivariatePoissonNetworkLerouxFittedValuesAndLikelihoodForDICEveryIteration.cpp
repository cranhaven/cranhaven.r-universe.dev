#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

List getUnivariatePoissonNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration(NumericMatrix standardizedX,
                                                                                    NumericVector y,
                                                                                    NumericMatrix spatialAssignment,
                                                                                    NumericMatrix W,
                                                                                    NumericVector beta,
                                                                                    NumericVector spatialRandomEffects,
                                                                                    NumericVector uRandomEffects)
{
  
  int numberOfLoglikelihoods = standardizedX.rows();
  NumericVector logLikelihoods(numberOfLoglikelihoods);
  
  NumericVector logTheta = matrixVectorMultiplicationRcpp(standardizedX, beta)
                         + matrixVectorMultiplicationRcpp(spatialAssignment, spatialRandomEffects)
                         + matrixVectorMultiplicationRcpp(W, uRandomEffects);
                           
  NumericVector fittedValues = getExp(logTheta);
  
  for(int i = 0; i < numberOfLoglikelihoods; i++) {
    logLikelihoods[i] = R::dpois(y[i], fittedValues[i], true);
  }
  
  List output(2);
  output[0] = fittedValues;
  output[1] = logLikelihoods;
  return output;
  
}
