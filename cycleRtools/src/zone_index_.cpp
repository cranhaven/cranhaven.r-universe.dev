#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
std::vector<double> zone_index_(NumericVector x, NumericVector zb)
{
  double xn = x.size(), zbn = zb.size();
  std::vector<double> out(xn);
  bool cond;
  for (double i = 0; i < xn; ++i)
  {
    /* If the current element of x is greater
     * than the largest boundary value,
     * assign the max' zone value.
     */
    if (x[i] > zb[zbn - 1])
    {
      out[i] = zbn + 1;
      continue;
    }
    // Otherwise, find appropriate zone value.
    int count = 0; // Reset.
    do {
      cond  = x[i] > zb[count++];
    } while (cond);
    out[i] = count;
  }
  return out;
}
