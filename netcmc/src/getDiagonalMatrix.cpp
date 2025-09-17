#include <Rcpp.h>
#include "header.h"

using namespace Rcpp;

NumericMatrix getDiagonalMatrix(NumericVector vector)
{
  int nRow = vector.size();
  NumericMatrix diagonalMatrix(nRow, nRow);
  for(int i = 0; i < nRow; i++){
    diagonalMatrix(i, i) = vector[i];
  }
  return diagonalMatrix;
}
