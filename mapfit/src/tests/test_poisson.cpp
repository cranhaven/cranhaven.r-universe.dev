#include "../poisson.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector test_poisson(double lam) {
  double eps = 1.0e-8;
  int right = poi::rightbound(lam, eps);
  NumericVector x(right+1);
  poi::pmf(lam, 0, right, x);
  return x;
}

/*** R
lam <- 10.0
result <- test_poisson(lam)
print(result)
*/
