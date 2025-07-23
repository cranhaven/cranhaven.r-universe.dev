#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix crossdist(NumericMatrix m1, NumericMatrix m2) {
  int nrow1 = m1.nrow();
  int nrow2 = m2.nrow();
  int ncol = m1.ncol();

  // Check if number of columns in both matrices are equal
  if (ncol != m2.ncol()) {
    throw std::runtime_error("Incompatible number of dimensions");
  }

  NumericMatrix out(nrow1, nrow2);

  // Calculate pairwise distances
  for (int r1 = 0; r1 < nrow1; r1++) {
    for (int r2 = 0; r2 < nrow2; r2++) {
      double total = 0;
      for (int c12 = 0; c12 < ncol; c12++) {
        total += pow(m1(r1, c12) - m2(r2, c12), 2);
      }
      out(r1, r2) = sqrt(total);
    }
  }

  return out;
}
