#include <Rcpp.h>
using namespace Rcpp;

//' Return Index of (First) Maximum of an Integer Vector
//' 
//' Written in C++, this function tends to run faster than \code{which.max} for 
//' large integer vectors.
//' 
//' For optimal speed, choose the version of this function that matches the 
//' class of your \code{x}:
//' 
//' \code{\link{which_max_nv}} for numeric vector. \cr
//' \code{\link{which_max_iv}} for integer vector. \cr
//' \code{\link{which_max_nm}} for numeric matrix. \cr
//' \code{\link{which_max_im}} for integer matrix.
//' 
//' @param x Integer vector.
//' 
//' @return Integer value.
//' 
//' @examples 
//' # For long vectors, which_max_iv is faster than which.max
//' x <- rpois(10000, lambda = 15)
//' which.max(x) == which_max_iv(x)
//' benchmark(which.max(x), which_max_iv(x), replications = 5000)
//' 
//' # For shorter vectors, which_max_iv is slower than which.max
//' x <- rpois(100, lambda = 15)
//' which.max(x) == which_max_iv(x)
//' benchmark(which.max(x), which_max_iv(x), replications = 20000)
//' 
//' @export
// [[Rcpp::export]]
int which_max_iv(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int maxx = currentx;
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
