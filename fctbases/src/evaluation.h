# ifndef _oo_int
# define _oo_int

#include <RcppArmadillo.h>
#include "function_class.h"
#include <set>

/* Contains evaluation stuff.
 * 
 * 
 */



// [[Rcpp::export]]
SEXP cpp_eval_coefs(const SEXP& address, const arma::vec& x, const NumericVector& coefs, bool check_valid = true) {

  if ((!check_valid) || check_if_valid(address)) {
    functionObject* fj = (functionObject*) R_ExternalPtrAddr(address);

    if (Rf_isMatrix(coefs))  {
      return wrap(fj->eval_fct_mat(x, as<mat>(coefs)));
    }
    else
    {
      vec ud = fj->eval_fct(x, as<vec>(coefs));
      return NumericVector(ud.begin(), ud.end());
    }
  }
  else stop("not a valid pointer!");
}

// [[Rcpp::export]]
arma::mat cpp_eval_0(const SEXP& address, const arma::vec& x, bool check_valid = true) {

  if ((!check_valid) || check_if_valid(address)) {
    functionObject* fj = (functionObject*) R_ExternalPtrAddr(address);
    return fj->eval_coefs(x);
  }
  else stop("not a valid pointer!");
}

// [[Rcpp::export]]
SEXP cpp_eval_Dcoefs(const SEXP& address, const arma::vec& x, const NumericVector& coefs, bool check_valid = true) {

  if ((!check_valid) || check_if_valid(address)) {
    functionObject* fj = (functionObject*) R_ExternalPtrAddr(address);

    if (Rf_isMatrix(coefs))  {
      return wrap(fj->eval_deriv_mat(x, as<mat>(coefs)));
    }
    else {
      vec ud = fj->eval_deriv(x, as<vec>(coefs));
      return NumericVector(ud.begin(), ud.end());
    }
  }
  else stop("not a valid pointer!");
}

// [[Rcpp::export]]
arma::mat cpp_eval_D(const SEXP& address, const arma::vec& x, bool check_valid = true) {
  
  if ((!check_valid) || check_if_valid(address)) {
    functionObject* fj = (functionObject*) R_ExternalPtrAddr(address);
    return fj->eval_deriv_coefs(x);
  }
  else stop("not a valid pointer!");
}

// [[Rcpp::export]]
SEXP cpp_eval_D2_coefs(const SEXP& address, const arma::vec& x, const NumericVector& coefs, bool check_valid = true) {

  if ((!check_valid) || check_if_valid(address)) {
    functionObject* fj = (functionObject*) R_ExternalPtrAddr(address);

    if (Rf_isMatrix(coefs))  {
      return wrap(fj->eval_d2_mat(x, as<mat>(coefs)));
    }
    else {
      vec ud = fj->eval_d2(x, as<vec>(coefs));
      return NumericVector(ud.begin(), ud.end());
    }
  }
  else stop("not a valid pointer!");
}

// [[Rcpp::export]]
arma::mat cpp_eval_D2(const SEXP& address, const arma::vec& x, bool check_valid = true) {

  if ((!check_valid) || check_if_valid(address)) {
    functionObject* fj = (functionObject*) R_ExternalPtrAddr(address);
    return fj->eval_d2_coefs(x);
  }
  else stop("not a valid pointer!");
}

// [[Rcpp::export]]
Rcpp::List describe_object(const SEXP& address, bool check_valid = true) {
  if ((!check_valid) || check_if_valid(address)) {
    functionObject* fj = (functionObject*) R_ExternalPtrAddr(address);
    return fj->returnObject();
  }
  else stop("not a valid pointer!");
}


#endif
