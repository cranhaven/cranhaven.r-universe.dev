#include <Rcpp.h>
using namespace Rcpp;
//' Efficient maximal mean values.
//'
//' A more efficient implementation of \code{\link{mmv}}. Simply takes a vector
//' (\code{x}) of values and rolls over them element wise by \code{windows}.
//' Returns a vector of maximum mean values for each window. \code{NA}s are not
//' ignored.
//'
//' @param x a numeric vector of values.
//' @param windows window size(s) (in element units) for which to
//'   generate maximum mean values.
//'
//' @return a vector of \code{length(windows)}.
//'
//' @examples
//' x <- rnorm(100, 500, 200)
//' mmv2(x, windows = c(5, 10, 20))
//'
//' @export
// [[Rcpp::export]]
std::vector<double> mmv2(NumericVector x, NumericVector windows)
{
  double mn, n = x.size(), winsz = windows.size();
  std::vector<double> out_max(winsz);
  for (unsigned int p = 0; p < winsz; ++p)     // Rolling over windows.
  {
    double st = (windows[p] - 1);
    for (unsigned int i = st; i < n; ++i)      // Roll to the end of x, row-wise.
    {
      mn = 0; // Reset.
      for (double c = (i - st); c <= i; ++c) mn += (x[c] / windows[p]);
      if (mn > out_max[p]) out_max[p] = mn;
    }
  }
  return out_max;
}
