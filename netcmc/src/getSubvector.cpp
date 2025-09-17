#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

NumericVector getSubvector(NumericVector vector,
                           int startIndex,
                           int endIndex)
{
  
  NumericVector outputVector((endIndex - startIndex + 1));
  int index = 0;
  for(int i = startIndex; i <= endIndex; i++){
    outputVector[index] = vector[i];
    index++;
  }
  
  return outputVector;
  
}
