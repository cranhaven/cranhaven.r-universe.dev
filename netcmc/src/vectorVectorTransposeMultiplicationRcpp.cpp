#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericMatrix vectorVectorTransposeMultiplicationRcpp(NumericVector vectorOne, 
                                                      NumericVector vectorTwo)
{
  int nRow = vectorOne.size();
  NumericMatrix answer(nRow, nRow);
  for(int i = 0; i < nRow; i++){
    for(int j = 0; j < nRow; j++){
      answer(i, j) += vectorOne[i] * vectorTwo[j];
    }
  }
  return answer;
}
