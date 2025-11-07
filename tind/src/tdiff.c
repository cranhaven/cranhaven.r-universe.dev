/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 **********************
 * tdiff - validation *
 **********************
 */


#include "tdiff.h"


// validation
// ==================================================================

#define VTDIFF_START_ \
    size_t i, n = XLENGTH(sx); \
    const int *x; \
    int *y; \
    SEXP sy; \
 \
    x = INTEGER(sx); \
    sy = PROTECT(allocVector(INTSXP, n)); \
    y = INTEGER(sy); \

#define VTDIFF_END_ \
    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol)); \
    UNPROTECT(1); \
    return sy;


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_tdiff_y(SEXP sx)
{
    VTDIFF_START_
    for (i = 0; i < n; ++i) y[i] = valid_tdiff_y(x[i]) ? x[i] : NA_INTEGER;
    VTDIFF_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_tdiff_q(SEXP sx)
{
    VTDIFF_START_
    for (i = 0; i < n; ++i) y[i] = valid_tdiff_q(x[i]) ? x[i] : NA_INTEGER;
    VTDIFF_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_tdiff_m(SEXP sx)
{
    VTDIFF_START_
    for (i = 0; i < n; ++i) y[i] = valid_tdiff_m(x[i]) ? x[i] : NA_INTEGER;
    VTDIFF_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_tdiff_w(SEXP sx)
{
    VTDIFF_START_
    for (i = 0; i < n; ++i) y[i] = valid_tdiff_w(x[i]) ? x[i] : NA_INTEGER;
    VTDIFF_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_tdiff_d(SEXP sx)
{
    VTDIFF_START_
    for (i = 0; i < n; ++i) y[i] = valid_tdiff_d(x[i]) ? x[i] : NA_INTEGER;
    VTDIFF_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_tdiff_t(SEXP sx)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    double *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(REALSXP, n));
    y = REAL(sy);

    for (i = 0; i < n; ++i) y[i] = valid_tdiff_t(x[i]) ? x[i] : NA_REAL;
    VTDIFF_END_
}

