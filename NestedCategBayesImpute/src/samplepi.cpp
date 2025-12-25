#include <Rcpp.h>
using namespace Rcpp;
#include "groupcount.h"

// [[Rcpp::export]]
List UpdatePi( double alpha, IntegerVector G_all, int FF) {
    IntegerVector kcount = groupcount1D(G_all, FF);
    IntegerVector cum(FF);
    cum[FF-1] = kcount[FF-1];
    for (int j = FF-2; j >=0; j--) {
      cum[j] = cum[j+1] + kcount[j];
    }

    double upperlimit = 1-1e-5;
    NumericVector u(FF);
    for (int j = 0; j < FF -1; j++) {
      u[j] = rbeta(1, 1 + kcount[j], alpha +cum[j+1])[0];
      if (u[j] > upperlimit) {u[j] = upperlimit;}
    }
    u[FF-1] = 1.0;

    NumericVector pi(FF);
    pi[0] = u[0];
    double cumprod = 1.0;
    for (int j = 1; j < FF; j++) {
      cumprod *= 1.0 - u[j-1];
      pi[j] = u[j] * cumprod;
    }
    return List::create(Named("pi", pi), Named("u", u));
}

// [[Rcpp::export]]
List UpdatePiWeighted( double alpha, List G_all, int FF, NumericVector struc_weight) {
  NumericVector kcount(FF);
  for(int g = 0; g <struc_weight.length();g++){
    IntegerVector G_all_w_i = G_all[g];
    IntegerVector current_counts = groupcount1D(G_all_w_i, FF);
    for (int i = 0; i < FF; i++) {
      kcount[i] += ((double)current_counts[i]) / struc_weight[g];
    }
  }

  NumericVector cum(FF);
  cum[FF-1] = kcount[FF-1];
  for (int j = FF-2; j >=0; j--) {
    cum[j] = cum[j+1] + kcount[j];
  }

  double upperlimit = 1-1e-5;
  NumericVector u(FF);
  for (int j = 0; j < FF -1; j++) {
    u[j] = rbeta(1, 1 + kcount[j], alpha +cum[j+1])[0];
    if (u[j] > upperlimit) {u[j] = upperlimit;}
  }
  u[FF-1] = 1.0;

  NumericVector pi(FF);
  pi[0] = u[0];
  double cumprod = 1.0;
  for (int j = 1; j < FF; j++) {
    cumprod *= 1.0 - u[j-1];
    pi[j] = u[j] * cumprod;
  }
  return List::create(Named("pi", pi), Named("u", u));
}

/*
UpdatePiWeightedR <- function(alpha,G_all,FF,struc_weight) {
  kcount <- 0
  for(w_i in 1:length(struc_weight)){
    kcount <- kcount + (groupcount1D(G_all[[w_i]], FF)/ struc_weight[w_i])
  }

  s <- seq(FF,1)
    cum <- cumsum(kcount[s])[s]
  u <- mapply(function(x,y) rbeta(1,x,y), 1 + kcount[1:FF-1], alpha + cum[2:FF])
    u[u > 1-1e-5] <- 1-1e-5
  u <- c(u,1)
    u[FF] <- 1

  pi  <- u* cumprod(c(1,1-u[1:FF-1]))

    return(list(pi = pi, u = u))
}
 */


/*
UpdatePi <- function(alpha,G_all,FF) {
  kcount <- groupcount1D(G_all, FF)
  s <- seq(FF,1)
  cum <- cumsum(kcount[s])[s]
  u <- mapply(function(x,y) rbeta(1,x,y), 1 + kcount[1:FF-1], alpha + cum[2:FF])
  u[u > 1-1e-5] <- 1-1e-5
  u <- c(u,1)
  u[FF] <- 1

  pi  <- u* cumprod(c(1,1-u[1:FF-1]))

  return(list(pi = pi, u = u))
}
*/
