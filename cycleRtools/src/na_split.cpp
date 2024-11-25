#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> na_split(NumericVector x) {
  // NA values are "binned" into an index value of 1.
  double n = x.size(), section = 2;
  std::vector<double> out(n);
  out[0] = (NumericVector::is_na(x[0])) ? 1 : section;
  for (double i = 1; i < n; ++i)
  {
    if (NumericVector::is_na(x[i]))
    {
      out[i] = 1;
      if (out[i - 1] != 1) ++section;
    }
    else out[i] = section;
  }
  return out;
}

/*** R
x <- c(1:10, rep_len(NA, 5), 20:30, rep_len(NA, 5))
split(x, na_split(x))[-1]
*/
