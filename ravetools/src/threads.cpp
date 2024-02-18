#include "common.h"
#include <tthread/tinythread.h>

// [[Rcpp::export]]
SEXP getDefaultNumThreads() {
  SEXP threadsSEXP = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(threadsSEXP)[0] = tthread::thread::hardware_concurrency();
  UNPROTECT(1);
  return threadsSEXP;
}

