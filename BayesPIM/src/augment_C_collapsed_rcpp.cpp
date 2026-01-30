#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector augment_C_collapsed_rcpp(NumericVector w_sums, List Vobs, double kappa, NumericVector theta1, NumericVector r, LogicalVector g_fixed) {
  int n = Vobs.size();
  LogicalVector cens(n);
  NumericVector m(n);
  NumericVector p0(n, NumericVector::get_na()), p1(n, NumericVector::get_na()), psum(n);
  NumericVector g(n, NumericVector::get_na());
  NumericVector theta0 = 1.0 - theta1;

  for(int i = 0; i < n; ++i) {
    NumericVector currentVec = Vobs[i];
    cens[i] = std::isinf(currentVec[currentVec.size() - 1]);
    m[i] = currentVec.size();
  }

  // Calculate p0 and p1 for non-fixed g
  for(int i = 0; i < n; ++i) {
    if(!g_fixed[i]) {
      p0[i] = theta0[i] * w_sums[i];
      p1[i] = theta1[i] * std::pow(kappa, 1 - cens[i]) * std::pow(1 - kappa, m[i] - 2 + r[i]);
    }
  }

  // Normalize probabilities
  for(int i = 0; i < n; ++i) {
    if(!g_fixed[i]) {
      psum[i] = p0[i] + p1[i];
      p1[i] /= psum[i];
    }
  }

  // Sample new g values for non-fixed g
  for(int i = 0; i < n; ++i) {
    if(!g_fixed[i]) {
      g[i] = R::rbinom(1, p1[i]);
    } else {
      g[i] = 1;
    }
  }

  return g;
}
