// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/math/distributions/non_central_t.hpp>

// [[Rcpp::export]]
double pt_boost(double q, double nu, double delta){
  return boost::math::cdf(boost::math::non_central_t(nu, delta), q);
}

// [[Rcpp::export]]
double qt_boost(double p, double nu, double delta){
  return boost::math::quantile(boost::math::non_central_t(nu, delta), p);
}
