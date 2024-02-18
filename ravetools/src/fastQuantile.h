#ifndef RAVETOOLS_FASTQUANTILE_H
#define RAVETOOLS_FASTQUANTILE_H

#include "utils.h"

SEXP quickQuantile(const SEXP &x, const double &prob, const bool &naRm = true);
SEXP quickMedian(const SEXP &x, const bool &naRm = true);

SEXP quickQuantile_double(const SEXP &x, const double &prob, const bool &naRm,
                          const bool &inplace);
SEXP quickQuantile_integer(const SEXP &x, const double &prob, const bool &naRm,
                           const bool &inplace);

template <typename T>
double quickQuantileInternal(T* &xptr, T* &bufptr, R_xlen_t &xlen, const double &prob);

#endif // RAVETOOLS_FASTQUANTILE_H
