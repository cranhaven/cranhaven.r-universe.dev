#include <Rcpp.h>
using namespace Rcpp;
//' Rolling mean for nonuniform data.
//'
//' Produce a rolling average for data sampled at non-uniform time intervals.
//'
//' @param x numeric vector of values to be rolled.
//' @param t numeric vector of time values corresponding to elements in \code{x}.
//' @param window size of the window in terms of \code{t}. E.g. \code{30} (seconds).
//'
//' @export
// [[Rcpp::export]]
std::vector<double> rollmean_nunif(NumericVector x, NumericVector t, double window) {
  double n = x.size(), sum, len, tmp;
  std::vector<double> out(n);

  // Find first index.
  int start = 0; while (t[start] < window) ++start;

  for (int i = start; i < n; ++i)
  {
    sum = 0; len = 0; // Reset.

    tmp = i; while(t[tmp] > (t[i] - window)) // Work backwards.
    {
      sum += x[tmp]; ++len;
      // Stop if we hit the bottom of x.
      if ((tmp - 1) < 0) break; else --tmp;
    }
    out[i] = sum / len;
  }
  return out;
}
