#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

double getSumVector(NumericVector vector)
{
  
  double answer = 0.0;
  
  for(int z = 0; z < vector.size(); z++){
    answer = answer + vector[z];
  }
  
  return answer;
  
}
