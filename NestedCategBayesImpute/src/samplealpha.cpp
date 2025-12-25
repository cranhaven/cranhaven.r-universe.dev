#include <Rcpp.h>
using namespace Rcpp;
#include <math.h>
#include "groupcount.h"

// [[Rcpp::export]]
double UpdateBeta(double ba, double bb, NumericMatrix v) {
  int FF = v.nrow();
  int SS = v.ncol();
  double sum = 0;
  for (int i = 0; i < FF; i++) {
    for (int j = 0; j < SS - 1; j++) {
      sum += log( 1.0 - v(i,j));
    }
  }
  double beta = rgamma(1,ba + FF*(SS-1),1.0 / (bb - sum))[0];
  return beta;
}

// [[Rcpp::export]]
double UpdateAlpha( double aa, double ab, NumericVector u) {
    int FF = u.length();
    double sum = 0;
    for (int i = 0; i < FF -1; i++) {
      sum += log( 1.0 - u[i]);
    }
    double alpha = rgamma(1,aa + FF -1, 1.0/ (ab - sum))[0];
    return alpha;
}
/*
UpdateAlpha <- function(aa,ab,u) {
  FF <- length(u)
  alpha <- rgamma(1,aa + FF - 1,scale = 1/(ab - sum(log(1-u[1:FF-1]))))
  return(alpha)
}
*/
/*
UpdateBeta <- function(ba,bb,v) {
  FF <- dim(v)[1]
  SS <- dim(v)[2]
  beta <- rgamma(1,ba + FF*(SS-1), scale = 1/(bb - sum(log(1-v[,1:SS-1])) ))
  return(beta)
}
 */
