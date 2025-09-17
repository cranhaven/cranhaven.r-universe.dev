#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector getExpDividedByOnePlusExp(NumericVector logitTheta)
{
  int numberOfRows = logitTheta.size();
  NumericVector outputVector(numberOfRows);
  
  for(int i = 0; i < numberOfRows; i++){
    outputVector[i] = exp(logitTheta[i]) / (1 + exp(logitTheta[i]));
  }
  
  return outputVector;
  
}
