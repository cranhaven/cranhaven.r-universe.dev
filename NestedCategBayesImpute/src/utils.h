#include <Rcpp.h>
using namespace Rcpp;

void transposeAndNormalize(double* mat, int row, int col, double* matT);
NumericMatrix transposeAndNormalize(NumericMatrix mat);
