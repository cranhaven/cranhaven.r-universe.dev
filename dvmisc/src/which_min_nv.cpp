#include <Rcpp.h>
using namespace Rcpp;

//' Return Index of (First) Minimum of a Numeric Vector
//' 
//' Written in C++, this function tends to run faster than 
//' \code{\link[base]{which.min}} for large numeric vectors.
//' 
//' For optimal speed, choose the version of this function that matches the 
//' class of your \code{x}:
//' 
//' \code{\link{which_min_nv}} for numeric vector. \cr
//' \code{\link{which_min_iv}} for integer vector. \cr
//' \code{\link{which_min_nm}} for numeric matrix. \cr
//' \code{\link{which_min_im}} for integer matrix.
//' 
//' @param x Numeric vector.
//' 
//' @return Integer value.
//' 
//' @examples 
//' # For long vectors, which_min_nv is faster than which.min
//' x <- rnorm(100000)
//' which.min(x) == which_min_nv(x)
//' benchmark(which.min(x), which_min_nv(x), replications = 1000)
//' 
//' # For shorter vectors, which_min_nv is slower than which.min
//' x <- rnorm(100)
//' which.min(x) == which_min_nv(x)
//' benchmark(which.min(x), which_min_nv(x), replications = 10000)
//' 
//' @export
// [[Rcpp::export]]
int which_min_nv(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double minx = currentx;
  int loc = 0;
  for (int a = 0; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) {
      minx = currentx;
      loc = a;
    }
  }
  loc += 1;
  return(loc);
}
