#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

double vectorTransposeVectorMultiplicationRcpp(NumericVector vectorOne, 
                                               NumericVector vectorTwo)
{
  int nRow = vectorOne.size();
  double answer = 0.0;
  for(int j = 0; j < nRow; j++){
    answer += vectorOne[j] * vectorTwo[j];
  }
  return answer;
}
