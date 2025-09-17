#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

double getVectorMean(NumericVector vector)
{
  
  int numberOfRows = vector.size();
  double answer = 0.0;
  
  for(int i = 0; i < numberOfRows; i++){
    answer = answer + vector[i];
  }
  
  return answer / numberOfRows;
  
}
