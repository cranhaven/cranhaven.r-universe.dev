#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sliding_cor_c(NumericVector shortvec, NumericVector longvec, double sd_shortvec) {
  
  // Get vector lengths and initialize output vector
  int length_longvec = longvec.size();
  int n = shortvec.size();
  int n_minus1 = n - 1;
  int out_length = length_longvec - n_minus1;
  NumericVector out(out_length);
  
  // Calculate constant term to multiply all 
  double mean_shortvec = sum(shortvec) / n;
  double ss_shortvec = 0;
  for (int a = 0; a < n; ++a) {
    ss_shortvec += pow(shortvec[a] - mean_shortvec, 2);
  }
  
  // Calculate sum of short vector divided by n nsquared
  double term2 = sum(shortvec) / n / n_minus1;
  
  // Loop through longvec and for each segment calculate the sum of the products 
  // with shortvec and record covariance
  NumericVector longvec_current(n);
  for (int a = 0; a < out_length; ++a) {
    longvec_current = longvec[Range(a, a + n_minus1)];
    double sum_longvec_current = 0;
    double sum_products = 0;
    double mean_longvec_current = sum(longvec_current) / n;
    double ss_longvec_current = 0;
    for (int b = 0; b < n; ++b) {
      double longvec_current_b = longvec_current[b];
      sum_longvec_current += longvec_current_b;
      sum_products += longvec_current_b * shortvec[b];
      ss_longvec_current += pow(longvec_current_b - mean_longvec_current, 2);
    }
    double sd_longvec_current = sqrt(ss_longvec_current / n_minus1);
    out[a] = (sum_products / n_minus1 - sum_longvec_current * term2) / sd_shortvec / sd_longvec_current;
  }
  return out;
}
