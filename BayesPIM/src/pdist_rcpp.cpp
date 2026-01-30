#include <Rcpp.h>
#include "pdist_rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pdist_rcpp(NumericVector q, NumericMatrix par, std::string dist) {
  int n = q.size();
  NumericVector result(n);
  
  if(dist == "exp") {
    for(int i = 0; i < n; ++i) {
      result[i] = R::pexp(q[i], par(i, 0), 1, 0); // q, rate, lower_tail, log_p
    }
  } else if(dist == "weibull2") {
    // Assuming pweibull2 is defined elsewhere
  } else if(dist == "gamma") {
    for(int i = 0; i < n; ++i) {
      result[i] = R::pgamma(q[i], par(i, 0), 1.0 / par(i, 1), 1, 0); // q, shape, scale, lower_tail, log_p
    }
  } else if(dist == "weibull") {
    for(int i = 0; i < n; ++i) {
      result[i] = R::pweibull(q[i], par(i, 1), par(i, 0), 1, 0); // q, shape, scale, lower_tail, log_p
    }
  } else if(dist == "loglog") {
    // Assuming ploglog is defined elsewhere
  } else if(dist == "lognormal") {
    for(int i = 0; i < n; ++i) {
      result[i] = R::plnorm(q[i], par(i, 0), par(i, 1), 1, 0); // q, meanlog, sdlog, lower_tail, log_p
    }
  } else if(dist == "gengamma") {
    // Assuming pggamma is defined elsewhere
  } else {
    stop("Unknown distribution");
  }
  
  return result;
}
