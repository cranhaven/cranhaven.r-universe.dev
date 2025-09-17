#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericVector doubleVectorMultiplicationRcpp(double number, 
                                             NumericVector vector)
{
  int nRow = vector.size();
  NumericVector ans(nRow);
  for(int j = 0; j < nRow; j++){
    ans[j] = number * vector[j];
  }
  return ans;
}

