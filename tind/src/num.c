/*
 * This file is a part of ats.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ********************************************************************
 * conversion to/from YYYYMM, YYYYMMDD, etc. numeric representation *
 ********************************************************************
 */


#include "num.h"
#include "compiler.h"
#include "calendar.h"


// automatic number parsing
// ==================================================================

static inline
TIND__ATTRIBUTE_INLINE
int
validy0(int y)
{
    return (y >= 1800) && (y <= 2199);
}


TIND__ATTRIBUTE_FUNCTION
SEXP
autoparse_num(SEXP sx)
{
    size_t i, n = XLENGTH(sx);
    const int *x = INTEGER(sx);
    int ok = 0;
    char type = '\0', ntype;

    SEXP sy = PROTECT(allocVector(INTSXP, n));
    int *y = INTEGER(sy);

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        if (validy0(x[i])) {
            y[i] = x[i];
            ntype = 'y';
            ok = 1;
        } else if (validy0(x[i] / 10) && validyq(x[i] / 10, x[i] % 10)) {
            y[i] = (x[i] / 10) * 4 + x[i] % 10 - 1;
            ntype = 'q';
            ok = 1;
        } else if (validy0(x[i] / 100) && validym(x[i] / 100, x[i] % 100)) {
            y[i] = (x[i] / 100) * 12 + x[i] % 100 - 1;
            ntype = 'm';
            ok = 1;
        } else if (validy0(x[i] / 10000) &&
                   validymd(x[i] / 10000, x[i] / 100 % 100, x[i] % 100)) {
            y[i] = ymd2d(x[i] / 10000, x[i] / 100 % 100, x[i] % 100);
            ntype = 'd';
            ok = 1;
        } else { ok = 0; break; }
        if (type) {
            if (type != ntype) { ok = 0; break; }
        } else type = ntype;
    }

    if (!ok) {
        UNPROTECT(1);
        return R_NilValue;
    }

    SEXP sres = PROTECT(allocVector(VECSXP, 2));
    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    SET_VECTOR_ELT(sres, 0, sy);
    char buf[2] = "x";
    buf[0] = type;
    SET_VECTOR_ELT(sres, 1, mkString(buf));
    UNPROTECT(2);
    return sres;
}



// number parsing
// ==================================================================

#define NUM2I_START_ \
    size_t i, n; \
    const int *x; \
    int *y; \
    SEXP sy; \
 \
    n = XLENGTH(sx); \
    x = INTEGER(sx); \
    sy = PROTECT(allocVector(INTSXP, n)); \
    y = INTEGER(sy);

#define NUM2I_END_ \
    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol)); \
    UNPROTECT(1); \
    return sy;



TIND__ATTRIBUTE_FUNCTION
SEXP
num2q(SEXP sx)
{
    NUM2I_START_
    for (i = 0; i < n; ++i) y[i] = validateyq(x[i] / 10, x[i] % 10);
    NUM2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
num2m(SEXP sx)
{
    NUM2I_START_
    for (i = 0; i < n; ++i) y[i] = validateym(x[i] / 100, x[i] % 100);
    NUM2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
num2w(SEXP sx)
{
    NUM2I_START_
    for (i = 0; i < n; ++i) y[i] = validateyw(x[i] / 100, x[i] % 100);
    NUM2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
num2d(SEXP sx)
{
    NUM2I_START_
    for (i = 0; i < n; ++i) y[i] = validateymd(x[i] / 10000, x[i] / 100 % 100,
                                               x[i] % 100);
    NUM2I_END_
}



// x2num
// ==================================================================

#define I2NUM_START_ \
    size_t i, n; \
    const int *x; \
    int *y; \
    SEXP sy; \
 \
    n = XLENGTH(sx); \
    x = INTEGER(sx); \
    sy = PROTECT(allocVector(INTSXP, n)); \
    y = INTEGER(sy);

#define I2NUM_END_ \
    UNPROTECT(1); \
    return sy;



TIND__ATTRIBUTE_FUNCTION
SEXP
q2num(SEXP sx)
{
    I2NUM_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        y[i] = (x[i] / 4) * 10 + x[i] % 4 + 1;
    }
    I2NUM_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
m2num(SEXP sx)
{
    I2NUM_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        y[i] = (x[i] / 12) * 100 + x[i] % 12 + 1;
    }
    I2NUM_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
w2num(SEXP sx)
{
    I2NUM_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy, w;
        w2yw(x[i], &yy, &w);
        y[i] = yy * 100 + w;
    }
    I2NUM_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2num(SEXP sx)
{
    I2NUM_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy, m, d;
        d2ymd(x[i], &yy, &m, &d);
        y[i] = yy * 10000 + m * 100 + d;
    }
    I2NUM_END_
}

