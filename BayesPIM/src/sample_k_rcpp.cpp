#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sample_k_rcpp(List pobs_norm) {
  int n = pobs_norm.size();
  IntegerVector k(n);
  
  for(int i = 0; i < n; ++i) {
    NumericVector p = as<NumericVector>(pobs_norm[i]);
    int len = p.size();
    
    // Initialize result for multinomial sampling
    IntegerVector result(len);
    
    // Sample from the multinomial distribution
    rmultinom(1, p.begin(), len, result.begin());
    
    // Calculate k
    int k_value = 0;
    for(int j = 0; j < len; ++j) {
      k_value += (j+1) * result[j];
    }
    
    // Store k_value in the output vector
    k[i] = k_value;
  }
  
  return k;
}
