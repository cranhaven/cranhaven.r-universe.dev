#include <Rcpp.h>
using namespace Rcpp;

//' Maximum of Numeric Values
//' 
//' Written in C++, this function tends to run faster than \code{max} for large 
//' numeric vectors/matrices.
//' 
//' @param x Numeric vector.
//' 
//' @return Numeric value.
//' 
//' @examples
//' # For large objects, max_n is faster than max
//' x <- rnorm(100000)
//' max(x) == max_n(x)
//' benchmark(max(x), max_n(x), replications = 1000)
//' 
//' # For smaller objects, max_n is slower than max
//' x <- rnorm(100)
//' max(x) == max_n(x)
//' benchmark(max(x), max_n(x), replications = 1000)
//' 
//' @export
// [[Rcpp::export]]
double max_n(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx > maxx) maxx = currentx;
  }
  return(maxx);
}
