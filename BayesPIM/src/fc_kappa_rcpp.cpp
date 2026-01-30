#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double fc_kappa_rcpp(List Vobs, NumericVector j_, double a, double b, NumericVector g, NumericVector r, LogicalVector g_fixed) {
  int n = Vobs.size();
  NumericVector delta(n);
  int rho = 0;
  int L0 = 0;
  int C1 = 0;
  NumericVector m(n);
  
  // Calculate delta, rho, and m
  for(int i = 0; i < n; ++i) {
    NumericVector currentVec = Vobs[i];
    delta[i] = !std::isinf(currentVec[currentVec.size() - 1]);
    delta[i] += g_fixed[i];
    rho += delta[i];
    m[i] = currentVec.size() - g_fixed[i];
  }
  
  // Calculate L0 and C1
  for(int i = 0; i < n; ++i) {
    if(g[i] == 0) {
      L0 += m[i] - j_[i] - 1;
    } else {
      C1 += m[i] + r[i] - 2;
    }
  }
  
  // Calculate alpha and beta
  double alpha = rho + a;
  double beta = L0 + C1 + b;
  
  // Generate random beta variable
  double kappa_new = R::rbeta(alpha, beta);
  
  return kappa_new;
}
