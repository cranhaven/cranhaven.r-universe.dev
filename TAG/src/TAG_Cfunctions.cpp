// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export]]
arma::vec getEigen(arma::mat M){
  return arma::eig_sym(M);
}


// [[Rcpp::export]]
Rcpp::NumericMatrix calcADDR3(Rcpp::NumericMatrix x,Rcpp::NumericVector theta,Rcpp::NumericVector omega){
  int outrows = x.nrow();
  int outcols = x.nrow();
  int inputdim = x.ncol();
  Rcpp::NumericMatrix out(outrows,outcols);

  for (int k = 0 ; k < inputdim; k++){
    for (int i = 0 ; i < outrows - 1; i++){
      out(i,i) = 1;
      for (int j = i + 1; j < outrows ; j ++) {
        double v1 = x(i,k);
        double v2 = x(j,k);
        double d =  omega(k)*exp(-pow((v1 - v2)/theta(k), 2));
        out(j,i) = out(j,i) + d;
        out(i,j) = out(i,j) + d;
      }
    }
  }
  out(outrows-1,outrows-1) =  1;
  return (out) ;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix calcProdR(Rcpp::NumericMatrix x,Rcpp::NumericVector theta){
  int outrows = x.nrow();
  int outcols = x.nrow();
  int inputdim = x.ncol();
  Rcpp::NumericMatrix out(outrows,outcols);

  for (int k = 0 ; k < inputdim; k++){
    for (int i = 0 ; i < outrows - 1; i++){
      out(i,i) = 0;
      for (int j = i + 1; j < outrows ; j ++) {
        double v1 = x(i,k);
        double v2 = x(j,k);
        double d =  -pow((v1 - v2)/theta(k), 2);
        out(j,i) = out(j,i) + d;
        out(i,j) = out(i,j) + d;
      }
    }
  }
  out(outrows-1,outrows-1) =  0;
  return (out) ;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix calcDR(Rcpp::NumericMatrix x,Rcpp::NumericVector theta, double phiest){
  int outrows = x.nrow();
  int outcols = x.nrow();
  int inputdim = x.ncol();
  Rcpp::NumericMatrix out(outrows,outcols);

  for (int k = 0 ; k < inputdim; k++){
    for (int i = 0 ; i < outrows - 1; i++){
      out(i,i) = 0;
      for (int j = i + 1; j < outrows ; j ++) {
        double v1 = x(i,k);
        double v2 = x(j,k);
        double d =  2*pow((v1 - v2)/theta(k), 2)/pow(phiest,3);
        out(j,i) = out(j,i) + d;
        out(i,j) = out(i,j) + d;
      }
    }
  }
  out(outrows-1,outrows-1) =  0;
  return (out) ;
}
