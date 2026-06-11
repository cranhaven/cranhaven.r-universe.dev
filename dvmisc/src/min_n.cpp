#include <Rcpp.h>
using namespace Rcpp;

//' Minimum of Numeric Values
//' 
//' Written in C++, this function tends to run faster than \code{min} for large 
//' numeric vectors/matrices.
//' 
//' @param x Numeric vector.
//' 
//' @return Numeric value.
//' 
//' @examples
//' # For large objects, min_n is faster than min
//' x <- rnorm(100000)
//' min(x) == min_n(x)
//' benchmark(min(x), min_n(x), replications = 1000)
//' 
//' # For smaller objects, min_n is slower than min
//' x <- rnorm(100)
//' min(x) == min_n(x)
//' benchmark(min(x), min_n(x), replications = 20000)
//' 
//' @export
// [[Rcpp::export]]
double min_n(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double minx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
  }
  return(minx);
}
