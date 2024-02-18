#include "utils.h"


template <>
double* get_sexp_pointer<double>(const SEXP& x){
  return REAL(x);
}

template <>
int* get_sexp_pointer<int>(const SEXP& x){
  if(TYPEOF(x) == LGLSXP){
    return LOGICAL(x);
  }
  return INTEGER(x);
}

template <>
Rbyte* get_sexp_pointer<Rbyte>(const SEXP& x){
  return RAW(x);
}

template <>
Rcomplex* get_sexp_pointer<Rcomplex>(const SEXP& x){
  return COMPLEX(x);
}

SEXP make_error(const char* message){
  SEXP error;
  PROTECT(error = Rf_mkString(message));
  Rf_classgets(error, Rf_mkString("ravetools_error"));
  UNPROTECT(1);
  return error;
}

void* SEXPPOINTER(const SEXP& x){
  void* re = NULL;
  switch(TYPEOF(x)) {
  case INTSXP:
  case LGLSXP:
    re = get_sexp_pointer<int>(x);
    break;

  case REALSXP:
    re = get_sexp_pointer<double>(x);
    break;

  case RAWSXP:
    re = get_sexp_pointer<Rbyte>(x);
    break;

  case CPLXSXP:
    re = get_sexp_pointer<Rcomplex>(x);
    break;

  default:
    Rcpp::stop("Unsupported SEXP type: only raw, int, double, complex types are allowed");

  }
  return re;
}

