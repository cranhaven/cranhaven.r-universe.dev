#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

NumericVector matrixVectorMultiplicationRcpp(NumericMatrix matrix, 
                                             NumericVector vector)
{
  int nRow = matrix.rows();
  int nCol = matrix.cols();
  NumericVector ans(nRow);
  double v_j;
  for(int j = 0; j < nCol; j++){
    v_j = vector[j];
    for(int i = 0; i < nRow; i++){
      ans[i] += matrix(i,j) * v_j;
    }
  }
  return ans;
}

