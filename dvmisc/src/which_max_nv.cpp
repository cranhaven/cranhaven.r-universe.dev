#include <Rcpp.h>
using namespace Rcpp;

//' Return Index of (First) Maximum of a Numeric Vector
//' 
//' Written in C++, this function tends to run faster than \code{which.max} for 
//' large numeric vectors.
//' 
//' For optimal speed, choose the version of this function that matches the 
//' class of your \code{x}:
//' 
//' \code{\link{which_max_nv}} for numeric vector. \cr
//' \code{\link{which_max_iv}} for integer vector. \cr
//' \code{\link{which_max_nm}} for numeric matrix. \cr
//' \code{\link{which_max_im}} for integer matrix.
//' 
//' @param x Numeric vector.
//' 
//' @return Integer value.
//' 
//' @examples 
//' # For long vectors, which_max_nv is faster than which.max
//' x <- rnorm(100000)
//' which.max(x) == which_max_nv(x)
//' benchmark(which.max(x), which_max_nv(x), replications = 500)
//' 
//' # For shorter vectors, which_max_nv is slower than which.max
//' x <- rnorm(100)
//' which.max(x) == which_max_nv(x)
//' benchmark(which.max(x), which_max_nv(x), replications = 10000)
//' 
//' @export
// [[Rcpp::export]]
int which_max_nv(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double maxx = currentx;
  int loc = 0;
  for (int a = 0; a < n; ++a) {
    currentx = x[a];
    if (currentx > maxx) {
      maxx = currentx;
      loc = a;
    }
  }
  loc += 1;
  return(loc);
}
