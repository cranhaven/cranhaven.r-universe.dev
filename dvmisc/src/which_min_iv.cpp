#include <Rcpp.h>
using namespace Rcpp;

//' Return Index of (First) Minimum of an Integer Vector
//' 
//' Written in C++, this function tends to run faster than 
//' \code{\link{which.min}} for large integer vectors.
//' 
//' For optimal speed, choose the version of this function that matches the 
//' class of your \code{x}:
//' 
//' \code{\link{which_min_nv}} for numeric vector. \cr
//' \code{\link{which_min_iv}} for integer vector. \cr
//' \code{\link{which_min_nm}} for numeric matrix. \cr
//' \code{\link{which_min_im}} for integer matrix.
//' 
//' @param x Integer vector.
//' 
//' @return Integer value.
//' 
//' @examples 
//' # For long vectors, which_min_iv is faster than which.min 
//' x <- rpois(10000, lambda = 15)
//' which.min(x) == which_min_iv(x)
//' benchmark(which.min(x), which_min_iv(x), replications = 5000)
//' 
//' # For shorter vectors, which_min_iv is slower than which.min
//' x <- rpois(100, lambda = 15)
//' which.min(x) == which_min_iv(x)
//' benchmark(which.min(x), which_min_iv(x), replications = 20000)
//' 
//' @export
// [[Rcpp::export]]
int which_min_iv(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int minx = currentx;
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
