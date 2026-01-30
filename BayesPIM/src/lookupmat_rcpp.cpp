#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix lookUpMat_rcpp(List L, IntegerVector a) {
  int n = a.size();
  NumericMatrix b(n, 2);
  
  for(int i = 0; i < n; ++i) {
    NumericVector l_i = as<NumericVector>(L[i]);
    int a_i = a[i];
    b(i, 0) = l_i[a_i - 1];  // R is 1-based indexing, C++ is 0-based
    b(i, 1) = l_i[a_i];      // R is 1-based indexing, C++ is 0-based
  }
  
  return b;
}
