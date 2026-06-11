#include <Rcpp.h>
using namespace Rcpp;

//' Mean of Integer Values
//' 
//' Written in C++, this function runs faster than \code{\link[base]{mean}} for 
//' large integer vectors/matrices.
//' 
//' @param x Integer vector or matrix.
//' 
//' @return Numeric value.
//' 
//' @examples
//' # For very large integer objects, sum_i is faster than sum
//' x <- rpois(100000, lambda = 5)
//' mean(x) == mean_i(x)
//' benchmark(mean(x), mean_i(x), replications = 1000)
//' 
//' # For smaller integer objects, sum_i is slower than sum 
//' x <- rpois(1000, lambda = 5)
//' mean(x) == mean_i(x)
//' benchmark(mean(x), mean_i(x), replications = 1000)
//' 
//' @export
// [[Rcpp::export]]
double mean_i(IntegerVector x) {
  int n = x.size();
  double sum_x = 0;
  for (int a = 0; a < n; ++a) {
    sum_x += x[a];
  }
  double mean_x = sum_x / n;
  return mean_x;
}
