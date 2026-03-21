#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_avg_count(NumericVector x, NumericVector y, NumericVector counts, int resol) {
  int n = x.size();
  int pad = resol * 5;
  NumericVector result(n);
  for (int i = 0; i < n; i++) {
    double x_min = x[i] - pad;
    double x_max = x[i] + pad;
    double y_min = y[i] - pad;
    double y_max = y[i] + pad;
    double sum = 0.0;
    for (int j = 0; j < n; j++) {
      if (x[j] >= x_min && x[j] <= x_max && y[j] >= y_min && y[j] <= y_max) {
        sum += counts[j];
      }
    }
    result[i] = sum / 121;
  }
  return result;
}
