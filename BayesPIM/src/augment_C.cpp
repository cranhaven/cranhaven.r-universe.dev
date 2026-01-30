#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector augment_C_Rcpp(List pobs, List Vobs, NumericVector X, double kappa, NumericVector theta1, NumericVector r, LogicalVector g_fixed) {
  int n = Vobs.size();
  NumericVector cens(n);
  IntegerVector m(n);
  IntegerVector k(n);
  
  // Calculate cens and m
  for(int i = 0; i < n; ++i) {
    NumericVector currentVec = Vobs[i];
    cens[i] = currentVec[currentVec.size() - 1] == INFINITY;
    m[i] = currentVec.size();
    k[i] = sum(X[i] > currentVec);
  }
  
  NumericVector theta0 = 1.0 - theta1; // Element-wise subtraction to create theta0 vector
  NumericVector p0(n, NA_REAL);
  NumericVector p1(n, NA_REAL);
  
  for(int i = 0; i < n; ++i) {
    if(!g_fixed[i]) {
      p0[i] = theta0[i] * std::pow(kappa, 1-cens[i]) * std::pow(1-kappa, m[i]-k[i]-1);
      p1[i] = theta1[i] * std::pow(kappa, 1-cens[i]) * std::pow(1-kappa, m[i]-2+r[i]);
    }
  }
  
  // Handle negative values of (m-k-1)
  for(int i = 0; i < n; ++i) {
    if((m[i]-k[i]-1) < 0 && !g_fixed[i]) {
      p0[i] = 0;
    }
  }
  
  NumericVector psum = p0 + p1;
  p1 = p1 / psum;
  
  NumericVector g(n, NA_REAL);
  for(int i = 0; i < n; ++i) {
    if(!g_fixed[i]) {
      g[i] = R::rbinom(1, p1[i]);
    } else {
      g[i] = 1;
    }
  }
  
  return g;
}
