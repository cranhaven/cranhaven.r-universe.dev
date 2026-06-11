#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sliding_cov_c(NumericVector shortvec, NumericVector longvec) {
  
  // Get vector lengths and initialize output vector
  int length_longvec = longvec.size();
  int n = shortvec.size();
  int n_minus1 = n - 1;
  int out_length = length_longvec - n_minus1;
  NumericVector out(out_length);
  
  // Calculate sum of short vector divided by n nsquared
  double term2 = sum(shortvec) / n / n_minus1;
  
  // Loop through longvec. For each segment calculate the sum of the products 
  // with shortvec and record the covariance
  NumericVector longvec_current(n);
  for (int a = 0; a < out_length; ++a) {
    longvec_current = longvec[Range(a, a + n_minus1)];
    double sum_longvec_current = 0;
    double sum_products = 0;
    for (int b = 0; b < n; ++b) {
      double longvec_current_b = longvec_current[b];
      sum_longvec_current += longvec_current_b;
      sum_products += longvec_current_b * shortvec[b];
    }
    out[a] = sum_products / n_minus1 - sum_longvec_current * term2;
  }
  return out;
}
