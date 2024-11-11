#include "WpProj_types.h"

//[[Rcpp::export]]
Rcpp::NumericMatrix selVarMeanGen(const SEXP & X_,
              const SEXP & theta_,
              const SEXP & beta_) {
  const matMap X(Rcpp::as<matMap >(X_));
  const vecMap beta(Rcpp::as<vecMap >(beta_));
  const matMap theta(Rcpp::as<matMap >(theta_));
  if (X.rows() != theta.rows()) Rcpp::stop("rows of theta must match rows of X");
  
  // return( Rcpp::wrap(X * beta.asDiagonal() * theta) );
  return( Rcpp::wrap(X.transpose() * beta.asDiagonal() * theta) );
}
