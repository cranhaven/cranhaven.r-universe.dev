#include <Rcpp.h>
using namespace Rcpp;

//' Sum of Integer Values
//' 
//' Written in C++, this function runs faster than \code{\link[base]{sum}} for 
//' large integer vectors/matrices.
//' 
//' @param x Integer vector or matrix.
//' 
//' @return Numeric value.
//' 
//' @examples
//' # For very large integer objects, sum_i is faster than sum
//' x <- rpois(100000, lambda = 5)
//' sum(x) == sum_i(x)
//' benchmark(sum(x), sum_i(x), replications = 1000)
//' 
//' # For smaller integer objects, sum_i is slower than sum 
//' x <- rpois(1000, lambda = 5)
//' sum(x) == sum_i(x)
//' benchmark(sum(x), sum_i(x), replications = 1000)
//' 
//' @export
// [[Rcpp::export]]
int sum_i(IntegerVector x) {
  int n = x.size();
  int sumx = 0;
  for (int a = 0; a < n; ++a) {
    sumx += x[a];
  }
  return sumx;
}
