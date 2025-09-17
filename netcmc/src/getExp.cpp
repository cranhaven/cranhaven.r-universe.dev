#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector getExp(NumericVector logTheta)
{
  int numberOfRows = logTheta.size();
  NumericVector outputVector(numberOfRows);
  
  for(int i = 0; i < numberOfRows; i++){
    outputVector[i] = exp(logTheta[i]);
  }
  
  return outputVector;
  
}
