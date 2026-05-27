#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_mean_day_clim_cpp(NumericVector obs, IntegerMatrix keepMat) {
  NumericVector mean_day_clim(366);
  NumericVector v(keepMat.ncol());
  IntegerVector keep(keepMat.ncol());

  for (int d = 0; d < 365; ++d) {
    keep = keepMat.row(d) - 1;
    v = obs[keep];
    mean_day_clim[d] = mean(v);
  }
  mean_day_clim[366] = mean_day_clim[365];
  return(mean_day_clim);

}

