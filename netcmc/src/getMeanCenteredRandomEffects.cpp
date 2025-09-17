#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector getMeanCenteredRandomEffects(NumericVector vector)
{
  int nRow = vector.size();
  
  double sum = 0.0;
  for(int j = 0; j < nRow; j++){
    sum = sum + vector[j];
  }
  
  double mean = sum / nRow;
  
  NumericVector ans(nRow);
  for(int j = 0; j < nRow; j++){
    ans[j] = vector[j] - mean;
  }
  
  return ans;
}
