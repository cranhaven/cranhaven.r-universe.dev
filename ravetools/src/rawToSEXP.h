#ifndef RAVETOOLS_RAWTOSEXP_H
#define RAVETOOLS_RAWTOSEXP_H

#include <Rcpp.h>

SEXP rawToUInt8(SEXP x);
SEXP rawToInt8(SEXP x);
SEXP rawToUInt16(SEXP x);
SEXP rawToInt16(SEXP x);
SEXP rawToUInt32(SEXP x);
SEXP rawToInt32(SEXP x);
SEXP rawToInt64(SEXP x);
SEXP rawToFloat(SEXP x);
SEXP rawToString(SEXP x);

#endif // RAVETOOLS_RAWTOSEXP_H
