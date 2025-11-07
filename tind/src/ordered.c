/*
 * This file is a part of ats.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ***********************************************
 * optimised routines for ordered time indices *
 ***********************************************
 */


#include "ordered.h"
#include "compiler.h"


// in-place transformation of findInterval results to match left/exact/right
// -1 - left, 0 - exact, 1 - right
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
fi2match(SEXP sfi, SEXP sx, SEXP st, SEXP stype, SEXP snomatch)
{
    size_t i, n = XLENGTH(sfi);
    int real = isReal(sx), type = asInteger(stype), nt = length(st);
    int nomatch = *INTEGER(snomatch);

    int *fi = INTEGER(sfi);
    switch (type) {
        case -1:
            for (i = 0; i < n; ++i)
                if ((fi[i] <= 0)) fi[i] = nomatch;
            break;
        case 1:
            for (i = 0; i < n; ++i)
                fi[i] = ((fi[i] < 0) || (fi[i] == nt)) ? nomatch : fi[i] + 1;
            break;
        case 0: {
            if (real) {
                const double *x = REAL(sx);
                const double *t = REAL(st) - 1; // 1-based indexing used here
                for (i = 0; i < n; ++i)
                    if ((fi[i] <= 0) || (x[i] != t[fi[i]])) fi[i] = nomatch;
            } else {
                const int *x = INTEGER(sx);
                const int *t = INTEGER(st) - 1; // 1-based indexing used here
                for (i = 0; i < n; ++i)
                    if ((fi[i] <= 0) || (x[i] != t[fi[i]])) fi[i] = nomatch;
            }
        }
    }

    return sfi;
}



// is ordered w/o NAs?
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
is_ordered(SEXP sx, SEXP sstrict)
{
    size_t i, n = XLENGTH(sx);
    int real, strict = asLogical(sstrict);

    if (!n) return ScalarLogical(1);

    real = isReal(sx);

    if (real) {
        const double *x = REAL(sx);
        if (!(R_FINITE(x[0]))) return ScalarLogical(0);
        if (strict) {
            for (i = 1; i < n; ++i) {
                if (!(x[i] > x[i - 1])) return ScalarLogical(0);
            }
        } else {
            for (i = 1; i < n; ++i) {
                if (!(x[i] >= x[i - 1])) return ScalarLogical(0);
            }
        }
    } else {
        const int *x = INTEGER(sx);
        if (x[0] == NA_INTEGER) return ScalarLogical(0);
        if (strict) {
            for (i = 1; i < n; ++i) {
                if (x[i] <= x[i - 1]) return ScalarLogical(0);
            }
        } else {
            for (i = 1; i < n; ++i) {
                if (x[i] < x[i - 1]) return ScalarLogical(0);
            }
        }
    }

    return ScalarLogical(1);
}


// is regular? (high resolution type "t")
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
is_regular_t(SEXP sx, SEXP sres)
{
    size_t i, n = XLENGTH(sx);

    const double *x = REAL(sx);
    // NOTE: resolution with tolerance of 1e-7 (as up to 6 digits are supported)
    double res = *REAL(sres) + 1e-7;
    for (i = 1; i < n; ++i) {
        if (x[i] - x[i - 1] > res) return ScalarLogical(0);
    }

    return ScalarLogical(1);
}


// is regular? (type "t" with resolution in hours)
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
is_regular_th(SEXP sx, SEXP sres)
{
    size_t i, n = XLENGTH(sx);
    double res = *REAL(sres);
    int hmult = (int) (res / 3600);

    const double *x = REAL(sx);
    int h0, h1;
    h0 = (int) (x[0] / 3600);
    for (i = 1; i < n; ++i) {
        h1 = (int) (x[i] / 3600);
        if (h1 != (h0 + hmult) % 24) return ScalarLogical(0);
        h0 = h1;
    }

    return ScalarLogical(1);
}



// unique
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
unique_ord(SEXP sx)
{
    size_t i, n = XLENGTH(sx), j;
    int real = isReal(sx);
    SEXP sy;

    if (!n) {
        sy = PROTECT(allocVector(real ? REALSXP : INTSXP, 0));
        setAttrib(sy, R_ClassSymbol, getAttrib(sx, R_ClassSymbol));
        UNPROTECT(1);
        return sy;
    }

    if (real) {
        const double *x = REAL(sx);
        // determine size of the result
        size_t ny = 1;
        double x0 = x[0];
        for (i = 1; i < n; ++i)
            if (x[i] > x0) { ++ny; x0 = x[i]; }
        // alloc result
        sy = PROTECT(allocVector(REALSXP, ny));
        double *y = REAL(sy);
        // main loop
        y[0] = x[0];
        x0 = x[0];
        for (i = 1, j = 1; j < ny; ++i)
            if (x[i] > x0) { x0 = x[i]; y[j++] = x0; }
    } else {
        const int *x = INTEGER(sx);
        // determine size of the result
        size_t ny = 1;
        int x0 = x[0];
        for (i = 1; i < n; ++i)
            if (x[i] > x0) { ++ny; x0 = x[i]; }
        // alloc result
        sy = PROTECT(allocVector(INTSXP, ny));
        int *y = INTEGER(sy);
        // main loop
        y[0] = x[0];
        x0 = x[0];
        for (i = 1, j = 1; j < ny; ++i)
            if (x[i] > x0) { x0 = x[i]; y[j++] = x0; }
    }

    setAttrib(sy, R_ClassSymbol, getAttrib(sx, R_ClassSymbol));
    UNPROTECT(1);
    return sy;
}


// intersect_ord
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
intersect_ord(SEXP sx, SEXP sy)
{
    size_t nx = XLENGTH(sx), ny = XLENGTH(sy);
    int real = isReal(sx);
    size_t nz = 0, i = 0, j = 0;

    // determine result size
    if (real) {
        const double *x, *y;
        x = REAL(sx);
        y = REAL(sy);
        double z0 = 1. / 0.;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i; ++j;
            }
        }
    } else {
        const int *x, *y;
        x = INTEGER(sx);
        y = INTEGER(sy);
        int z0 = NA_INTEGER;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i; ++j;
            }
        }
    }

    // alloc result
    SEXP sz = PROTECT(allocVector(real ? REALSXP : INTSXP, nz));
    i = 0, j = 0;
    int k = 0;

    // construct result
    if (real) {
        const double *x, *y;
        double *z;
        x = REAL(sx);
        y = REAL(sy);
        z = REAL(sz);
        double z0 = 1. / 0.;
        while (k < nz) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i; ++j;
            }
        }
    } else {
        const int *x, *y;
        int *z;
        x = INTEGER(sx);
        y = INTEGER(sy);
        z = INTEGER(sz);
        int z0 = NA_INTEGER;
        while (k < nz) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i; ++j;
            }
        }
    }

    setAttrib(sz, R_ClassSymbol, getAttrib(sx, R_ClassSymbol));
    UNPROTECT(1);
    return sz;
}


// union_ord
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
union_ord(SEXP sx, SEXP sy)
{
    size_t nx = XLENGTH(sx), ny = XLENGTH(sy);
    int real = isReal(sx);
    size_t nz = 0, i = 0, j = 0, k;

    // determine result size
    if (real) {
        const double *x, *y;
        x = REAL(sx);
        y = REAL(sy);
        double z0 = 1. / 0.;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i;
            } else if (x[i] > y[j]) {
                if (y[j] != z0) { z0 = y[j]; ++nz; }
                ++j;
            } else {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i; ++j;
            }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; ++nz; }
        for (; j < ny; ++j) if (y[j] != z0) { z0 = y[j]; ++nz; }
    } else {
        const int *x, *y;
        x = INTEGER(sx);
        y = INTEGER(sy);
        int z0 = NA_INTEGER;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i;
            } else if (x[i] > y[j]) {
                if (y[j] != z0) { z0 = y[j]; ++nz; }
                ++j;
            } else {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i; ++j;
            }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; ++nz; }
        for (; j < ny; ++j) if (y[j] != z0) { z0 = y[j]; ++nz; }
    }

    // alloc result
    SEXP sz = PROTECT(allocVector(real ? REALSXP : INTSXP, nz));
    i = 0, j = 0, k = 0;

    // construct result
    if (real) {
        const double *x, *y;
        double *z;
        x = REAL(sx);
        y = REAL(sy);
        z = REAL(sz);
        double z0 = 1. / 0.;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i;
            } else if (x[i] > y[j]) {
                if (y[j] != z0) { z0 = y[j]; z[k++] = z0; }
                ++j;
            } else {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i; ++j;
            }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
        for (; j < ny; ++j) if (y[j] != z0) { z0 = y[j]; z[k++] = z0; }
    } else {
        const int *x, *y;
        int *z;
        x = INTEGER(sx);
        y = INTEGER(sy);
        z = INTEGER(sz);
        int z0 = NA_INTEGER;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i;
            } else if (x[i] > y[j]) {
                if (y[j] != z0) { z0 = y[j]; z[k++] = z0; }
                ++j;
            } else {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i; ++j;
            }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
        for (; j < ny; ++j) if (y[j] != z0) { z0 = y[j]; z[k++] = z0; }
    }

    setAttrib(sz, R_ClassSymbol, getAttrib(sx, R_ClassSymbol));
    UNPROTECT(1);
    return sz;
}


// setdiff_ord
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
setdiff_ord(SEXP sx, SEXP sy)
{
    size_t nx = XLENGTH(sx), ny = XLENGTH(sy);
    int real = isReal(sx);
    size_t nz = 0, i = 0, j = 0, k;

    // determine result size
    if (real) {
        const double *x, *y;
        x = REAL(sx);
        y = REAL(sy);
        double z0 = 1. / 0.;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i;
            } else if (x[i] > y[j]) ++j; else { z0 = x[i]; ++i; ++j; }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; ++nz; }
    } else {
        const int *x, *y;
        x = INTEGER(sx);
        y = INTEGER(sy);
        int z0 = NA_INTEGER;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; ++nz; }
                ++i;
            } else if (x[i] > y[j]) ++j; else { z0 = x[i]; ++i; ++j; }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; ++nz; }
    }

    // alloc result
    SEXP sz = PROTECT(allocVector(real ? REALSXP : INTSXP, nz));
    i = 0, j = 0, k = 0;

    // construct result
    if (real) {
        const double *x, *y;
        double *z;
        x = REAL(sx);
        y = REAL(sy);
        z = REAL(sz);
        double z0 = 1. / 0.;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i;
            } else if (x[i] > y[j]) ++j; else { z0 = x[i]; ++i; ++j; }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
    } else {
        const int *x, *y;
        int *z;
        x = INTEGER(sx);
        y = INTEGER(sy);
        z = INTEGER(sz);
        int z0 = NA_INTEGER;
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
                ++i;
            } else if (x[i] > y[j]) ++j; else { z0 = x[i]; ++i; ++j; }
        }
        for (; i < nx; ++i) if (x[i] != z0) { z0 = x[i]; z[k++] = z0; }
    }

    setAttrib(sz, R_ClassSymbol, getAttrib(sx, R_ClassSymbol));
    UNPROTECT(1);
    return sz;
}



// match
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
match_ord(SEXP sx, SEXP st, SEXP snomatch)
{
    size_t nx = XLENGTH(sx);
    SEXP sm = PROTECT(allocVector(INTSXP, nx));
    if (!nx) { UNPROTECT(1); return sm; }

    int nt = length(st);
    int nomatch = *INTEGER(snomatch);
    int real = isReal(sx);
    int *m = INTEGER(sm);
    size_t i = 0;
    int j = 0;
    if (real) {
        const double *x, *t;
        x = REAL(sx);
        t = REAL(st);
        while ((i < nx) && (j < nt)) {
            if (x[i] < t[j]) m[i++] = nomatch;
            else if (x[i] > t[j]) j++;
            else { m[i++] = j + 1; }
        }
    } else {
        const int *x, *t;
        x = INTEGER(sx);
        t = INTEGER(st);
        while ((i < nx) && (j < nt)) {
            if (x[i] < t[j]) m[i++] = nomatch;
            else if (x[i] > t[j]) j++;
            else { m[i++] = j + 1; }
        }
    }
    for (; i < nx; ++i) m[i] = nomatch;


    UNPROTECT(1);
    return sm;
}


// inner join
// ==================================================================
TIND__ATTRIBUTE_FUNCTION
SEXP
merge_in_ord(SEXP sx, SEXP sy)
{
    size_t nx = XLENGTH(sx), ny = XLENGTH(sy);
    int real = isReal(sx);
    size_t nz = 0, i = 0, j = 0;

    // determine result size
    if (real) {
        const double *x, *y;
        x = REAL(sx);
        y = REAL(sy);
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else { ++i; ++j; ++nz; }
        }
    } else {
        const int *x, *y;
        x = INTEGER(sx);
        y = INTEGER(sy);
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else { ++i; ++j; ++nz; }
        }
    }

    // alloc result
    SEXP sind = PROTECT(allocVector(real ? REALSXP : INTSXP, nz));
    SEXP sxi = PROTECT(allocVector(INTSXP, nz));
    SEXP syi = PROTECT(allocVector(INTSXP, nz));
    int *xi = INTEGER(sxi), *yi = INTEGER(syi);
    i = 0, j = 0;
    size_t k = 0;

    // construct result
    if (real) {
        const double *x, *y;
        double *ind;
        x = REAL(sx);
        y = REAL(sy);
        ind = REAL(sind);
        while (k < nz) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else {
                ind[k] = x[i];
                xi[k] = i + 1;
                yi[k] = j + 1;
                ++i; ++j; ++k;
            }
        }
    } else {
        const int *x, *y;
        int *ind;
        x = INTEGER(sx);
        y = INTEGER(sy);
        ind = INTEGER(sind);
        while (k < nz) {
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else {
                ind[k] = x[i];
                xi[k] = i + 1;
                yi[k] = j + 1;
                ++i; ++j; ++k;
            }
        }
    }

    SEXP sres = PROTECT(allocVector(VECSXP, 3));
    setAttrib(sind, R_ClassSymbol, getAttrib(sx, R_ClassSymbol));
    SET_VECTOR_ELT(sres, 0, sind);
    SET_VECTOR_ELT(sres, 1, sxi);
    SET_VECTOR_ELT(sres, 2, syi);
    SEXP snms = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(snms, 0, mkChar("index"));
    SET_STRING_ELT(snms, 1, mkChar("xi"));
    SET_STRING_ELT(snms, 2, mkChar("yi"));
    setAttrib(sres, R_NamesSymbol, snms);
    UNPROTECT(5);
    return sres;
}


// outer join
// ==================================================================
TIND__ATTRIBUTE_FUNCTION
SEXP
merge_out_ord(SEXP sx, SEXP sy)
{
    size_t nx = XLENGTH(sx), ny = XLENGTH(sy);
    int real = isReal(sx);
    size_t nz = 0, i = 0, j = 0;

    // determine result size
    if (real) {
        const double *x, *y;
        x = REAL(sx);
        y = REAL(sy);
        while ((i < nx) && (j < ny)) {
            ++nz;
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else { ++i; ++j; }
        }
    } else {
        const int *x, *y;
        x = INTEGER(sx);
        y = INTEGER(sy);
        while ((i < nx) && (j < ny)) {
            ++nz;
            if (x[i] < y[j]) ++i;
            else if (x[i] > y[j]) ++j;
            else { ++i; ++j; }
        }
    }
    nz += (nx - i) + (ny - j);

    // alloc result
    SEXP sind = PROTECT(allocVector(real ? REALSXP : INTSXP, nz));
    SEXP sxi = PROTECT(allocVector(INTSXP, nz));
    SEXP syi = PROTECT(allocVector(INTSXP, nz));
    int *xi = INTEGER(sxi), *yi = INTEGER(syi);
    i = 0, j = 0;
    size_t k = 0;

    // construct result
    if (real) {
        const double *x, *y;
        double *ind;
        x = REAL(sx);
        y = REAL(sy);
        ind = REAL(sind);
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                ind[k] = x[i];
                xi[k] = i + 1;
                yi[k] = NA_INTEGER;
                ++i;
            } else if (x[i] > y[j]) {
                ind[k] = y[j];
                yi[k] = j + 1;
                xi[k] = NA_INTEGER;
                ++j;
            } else {
                ind[k] = x[i];
                xi[k] = i + 1;
                yi[k] = j + 1;
                ++i; ++j;
            }
            ++k;
        }
        for (; i < nx; ++i) {
            ind[k] = x[i];
            xi[k] = i + 1;
            yi[k] = NA_INTEGER;
            ++k;
        }
        for (; j < ny; ++j) {
            ind[k] = y[j];
            yi[k] = j + 1;
            xi[k] = NA_INTEGER;
            ++k;
        }
    } else {
        const int *x, *y;
        int *ind;
        x = INTEGER(sx);
        y = INTEGER(sy);
        ind = INTEGER(sind);
        while ((i < nx) && (j < ny)) {
            if (x[i] < y[j]) {
                ind[k] = x[i];
                xi[k] = i + 1;
                yi[k] = NA_INTEGER;
                ++i;
            } else if (x[i] > y[j]) {
                ind[k] = y[j];
                yi[k] = j + 1;
                xi[k] = NA_INTEGER;
                ++j;
            } else {
                ind[k] = x[i];
                xi[k] = i + 1;
                yi[k] = j + 1;
                ++i; ++j;
            }
            ++k;
        }
        for (; i < nx; ++i) {
            ind[k] = x[i];
            xi[k] = i + 1;
            yi[k] = NA_INTEGER;
            ++k;
        }
        for (; j < ny; ++j) {
            ind[k] = y[j];
            yi[k] = j + 1;
            xi[k] = NA_INTEGER;
            ++k;
        }
    }

    SEXP sres = PROTECT(allocVector(VECSXP, 3));
    setAttrib(sind, R_ClassSymbol, getAttrib(sx, R_ClassSymbol));
    SET_VECTOR_ELT(sres, 0, sind);
    SET_VECTOR_ELT(sres, 1, sxi);
    SET_VECTOR_ELT(sres, 2, syi);
    SEXP snms = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(snms, 0, mkChar("index"));
    SET_STRING_ELT(snms, 1, mkChar("xi"));
    SET_STRING_ELT(snms, 2, mkChar("yi"));
    setAttrib(sres, R_NamesSymbol, snms);
    UNPROTECT(5);
    return sres;
}

