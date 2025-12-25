#include <Rcpp.h>
using namespace Rcpp;
#include "groupcount.h"

// [[Rcpp::export]]
List UpdateOmega( double beta, IntegerMatrix M_all, int FF, int SS) {

  int n = M_all.ncol();
  IntegerVector row1(n);
  IntegerVector row2(n);

  NumericMatrix v(FF,SS);
  NumericMatrix omega(FF,SS);

  for (int i = 0; i < n; i++) {
    row1[i] = M_all(0,i);
    row2[i] = M_all(1,i);
  }
  IntegerMatrix phicountcluster = groupcount(row1, row2,FF,SS);

  IntegerMatrix cum(FF,SS);
  for(int i = 0; i <FF; i++) {
    cum(i,SS-1) = phicountcluster(i,SS-1);
    for (int j = SS-2; j >=0; j--) {
        cum(i,j) = cum(i,j+1) + phicountcluster(i,j);
    }
  }
  double upperlimit = 1-1e-5;
  for (int i =0; i < FF; i++) {
    for (int j =0; j < SS -1; j++) {
      v(i,j) = rbeta(1, 1 + phicountcluster(i,j), beta +cum(i,j+1))[0];
      if (v(i,j) > upperlimit) {v(i,j) = upperlimit;}
    }
    v(i,SS-1) = 1.0;
  }
  for (int i = 0; i < FF; i++) {
    omega(i,0) = v(i,0);
    double cumprod = 1.0;
    for (int j = 1; j < SS; j++) {
      cumprod *= 1.0 - v(i,j-1);
      omega(i,j) = v(i,j) * cumprod;
    }
  }
  return List::create(Named("omega", omega), Named("v", v));
}

// [[Rcpp::export]]
List UpdateOmegaWeighted(double beta, List M_all, int FF, int SS, NumericVector struc_weight) {
  NumericMatrix v(FF,SS);
  NumericMatrix omega(FF,SS);

  NumericMatrix phicountcluster(FF,SS);
  for (int i = 0; i < struc_weight.length(); i++ ) {
    NumericMatrix M = M_all[i];
    int n = M.ncol();
    IntegerVector row1(n);
    IntegerVector row2(n);
    for (int j = 0; j < n; j++) {
      row1[j] = M(0,j);
      row2[j] = M(1,j);
    }
    IntegerMatrix counts = groupcount(row1, row2,FF,SS);
    for (int j = 0; j< counts.length(); j++) {
      phicountcluster[j] += ((double)counts[j]) / struc_weight[i];
    }
  }
  NumericMatrix cum(FF,SS);
  for(int i = 0; i <FF; i++) {
    cum(i,SS-1) = phicountcluster(i,SS-1);
    for (int j = SS-2; j >=0; j--) {
      cum(i,j) = cum(i,j+1) + phicountcluster(i,j);
    }
  }
  double upperlimit = 1-1e-5;
  for (int i =0; i < FF; i++) {
    for (int j =0; j < SS -1; j++) {
      v(i,j) = rbeta(1, 1 + phicountcluster(i,j), beta +cum(i,j+1))[0];
      if (v(i,j) > upperlimit) {v(i,j) = upperlimit;}
    }
    v(i,SS-1) = 1.0;
  }
  for (int i = 0; i < FF; i++) {
    omega(i,0) = v(i,0);
    double cumprod = 1.0;
    for (int j = 1; j < SS; j++) {
      cumprod *= 1.0 - v(i,j-1);
      omega(i,j) = v(i,j) * cumprod;
    }
  }
  return List::create(Named("omega", omega), Named("v", v));
}
/*
UpdateOmegaWeighted <- function(beta,M_all, FF, SS, struc_weight) {
  phicountcluster <- 0
  for(w_i in 1:length(struc_weight)){
    M_all_w_i <- M_all[[w_i]]
    phicountcluster <- phicountcluster + (groupcount(M_all_w_i[1,],M_all_w_i[2,],FF,SS) / struc_weight[w_i])
  }

  cum <- t(apply(phicountcluster[,seq(SS,1)],1, cumsum))[,seq(SS,1)]
  v <- mapply(function(x,y) rbeta(1,x,y), 1 + phicountcluster[,1:(SS-1)], beta + cum[,2:SS])
    dim(v) <- c(FF, SS-1)
    v[v>1-1e-5] <- 1-1e-5
  v = cbind(v,1)
    omega <- v * t(apply(cbind(1, 1-v[,1:(SS-1)]), 1, cumprod))

    return(list(omega = omega, v = v))
}
*/

/*
UpdateOmegaR <- function(beta,M_all, FF, SS) {
  phicountcluster <- groupcount(M_all[1,],M_all[2,],FF,SS)

  cum <- t(apply(phicountcluster[,seq(SS,1)],1, cumsum))[,seq(SS,1)]
  v <- mapply(function(x,y) rbeta(1,x,y), 1 + phicountcluster[,1:(SS-1)], beta + cum[,2:SS])
  dim(v) <- c(FF, SS-1)
  v[v>1-1e-5] <- 1-1e-5
  v = cbind(v,1)

  omega <- v * t(apply(cbind(1, 1-v[,1:(SS-1)]), 1, cumprod))
  return(list(omega = omega, v = v))
}
 */
