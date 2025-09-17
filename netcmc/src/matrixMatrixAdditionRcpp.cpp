#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericMatrix matrixMatrixAdditionRcpp(NumericMatrix matrix1, 
                                       NumericMatrix matrix2)
{
  int nRow = matrix1.rows();
  int nCol = matrix1.cols();
  NumericMatrix ans(nRow, nCol);
  for(int i = 0; i < nRow; i++){
    for(int j = 0; j < nCol; j++){
      ans(i, j) += matrix1(i, j) + matrix2(i, j);
    }
  }
  return ans;
}
