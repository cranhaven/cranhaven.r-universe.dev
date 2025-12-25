#include <Rcpp.h>
using namespace Rcpp;
#include "utils.h"

void transposeAndNormalize(double* mat, int row, int col, double* matT) {
  double *currentrow = matT;
  for (int k =0; k < row; k++) {
    double dsum = 0.0;
    for (int l = 0; l <col; l++) {
      //transpose first
      currentrow[l] = mat[l*row+k];  //matT[k * col + l] = mat[l*row+k];
      dsum += currentrow[l];
    }
    currentrow[0] /= dsum;
    for (int l = 1; l <col; l++) {
      currentrow[l] = currentrow[l]/dsum + currentrow[l-1]; //normilized cum_sum
    }
    currentrow += col;
  }
}
NumericMatrix transposeAndNormalize(NumericMatrix mat) {
  int row = mat.nrow();
  int col = mat.ncol();
  NumericMatrix matT(col,row);
  transposeAndNormalize(mat.begin(), row, col, matT.begin());
  /*
  double *currentrow = matT.begin();
  for (int k =0; k < row; k++) {
    double dsum = 0.0;
    for (int l = 0; l <col; l++) {
      //transpose first
      currentrow[l] = mat[l*row+k];  //matT[k * col + l] = mat[l*row+k];
      dsum += currentrow[l];
    }
    currentrow[0] /= dsum;
    for (int l = 1; l <col; l++) {
      currentrow[l] = currentrow[l]/dsum + currentrow[l-1]; //normilized cum_sum
    }
    currentrow += col;
  }
   */
  return matT;
}


