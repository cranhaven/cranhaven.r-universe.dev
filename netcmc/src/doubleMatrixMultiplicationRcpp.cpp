#include <Rcpp.h>
#include "header.h"
using namespace Rcpp;

NumericMatrix doubleMatrixMultiplicationRcpp(double number, 
                                             NumericMatrix matrix)
{
  int nRow = matrix.rows();
  int nCol = matrix.cols();
  NumericMatrix ans(nRow, nCol);
  for(int i = 0; i < nRow; i++){
    for(int j = 0; j < nCol; j++){
      ans(i, j) = number * matrix(i, j);
    }
  }
  return ans;
}
