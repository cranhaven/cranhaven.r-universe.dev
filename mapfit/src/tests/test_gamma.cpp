
#include "../gamma.h"
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
void test_gamma(double alpha) {
  Rcout << gam::lgamma(alpha) << std::endl;
  Rcout << gam::erlang_pdf(2, 3.4, 5.6) << std::endl;
}

/*** R
alpha <- 10.0
test_gamma(alpha)
*/
