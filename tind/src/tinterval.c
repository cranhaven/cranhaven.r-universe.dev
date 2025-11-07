/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ********************************
 * tinterval - intersect, match *
 ********************************
 */


#include "tinterval.h"
#include "compiler.h"



// inlines - comparison operators handling NAs
// ==================================================================

// integers
static inline
TIND__ATTRIBUTE_INLINE
int
x_gt_y_i(int x, int y)
{
    if ((y == NA_INTEGER) || (x == NA_INTEGER)) return 0;
    return x > y;
}


static inline
TIND__ATTRIBUTE_INLINE
int
min_i(int x, int y)
{
    if (y == NA_INTEGER) return x;
    if (x == NA_INTEGER) return y;
    return (x >= y) ? y : x;
}


static inline
TIND__ATTRIBUTE_INLINE
int
max_i(int x, int y)
{
    if (y == NA_INTEGER) return x;
    if (x == NA_INTEGER) return y;
    return (x >= y) ? x : y;
}


// doubles
static inline
TIND__ATTRIBUTE_INLINE
int
x_gt_y_d(double x, double y)
{
    if (!(R_FINITE(y)) || !(R_FINITE(x))) return 0;
    return x > y;
}


static inline
TIND__ATTRIBUTE_INLINE
int
x_ge_y_d(double x, double y)
{
    if (!(R_FINITE(y)) || !(R_FINITE(x))) return 0;
    return x >= y;
}


static inline
TIND__ATTRIBUTE_INLINE
double
min_d(double x, double y)
{
    if (!(R_FINITE(y))) return x;
    if (!(R_FINITE(x))) return y;
    return (x >= y) ? y : x;
}


static inline
TIND__ATTRIBUTE_INLINE
double
max_d(double x, double y)
{
    if (!(R_FINITE(y))) return x;
    if (!(R_FINITE(x))) return y;
    return (x >= y) ? x : y;
}


// intersect
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
intersect_tint(SEXP sxs, SEXP sxe, SEXP sys, SEXP sye)
{
    size_t nx = XLENGTH(sxs), ny = XLENGTH(sys);
    int real = isReal(sxs);

    // determine result size
    size_t nz = 0, i = 0, j = 0;
    if (real) {
        const double *xs = REAL(sxs);
        const double *xe = REAL(sxe);
        const double *ys = REAL(sys);
        const double *ye = REAL(sye);
        while ((i < nx) && (j < ny)) {
            if (x_ge_y_d(xs[i], ye[j])) ++j;
            else if (x_ge_y_d(ys[j], xe[i])) ++i;
            else {
                ++nz;
                double ze = min_d(ye[j], xe[i]);
                if (!R_FINITE(ze)) break;
                if (ye[j] == ze) ++j;
                if (xe[i] == ze) ++i;
            }
        }
    } else {
        const int *xs = INTEGER(sxs);
        const int *xe = INTEGER(sxe);
        const int *ys = INTEGER(sys);
        const int *ye = INTEGER(sye);
        while ((i < nx) && (j < ny)) {
            if (x_gt_y_i(xs[i], ye[j])) ++j;
            else if (x_gt_y_i(ys[j], xe[i])) ++i;
            else {
                int ze = min_i(ye[j], xe[i]);
                if (ye[j] == ze) ++j;
                if (xe[i] == ze) ++i;
                ++nz;
            }
        }
    }

    // alloc result
    SEXP sz, szs, sze;
    szs = PROTECT(allocVector(real ? REALSXP : INTSXP, nz));
    sze = PROTECT(allocVector(real ? REALSXP : INTSXP, nz));

    // finish
    size_t k = 0;
    i = j = 0;
    if (real) {
        const double *xs = REAL(sxs);
        const double *xe = REAL(sxe);
        const double *ys = REAL(sys);
        const double *ye = REAL(sye);
        double *zs = REAL(szs);
        double *ze = REAL(sze);
        while (k < nz) {
            if (x_ge_y_d(xs[i], ye[j])) ++j;
            else if (x_ge_y_d(ys[j], xe[i])) ++i;
            else {
                zs[k] = max_d(ys[j], xs[i]);
                ze[k] = min_d(ye[j], xe[i]);
                if (!R_FINITE(ze[k])) break;
                if (ye[j] == ze[k]) ++j;
                if (xe[i] == ze[k]) ++i;
                ++k;
            }
        }
    } else {
        const int *xs = INTEGER(sxs);
        const int *xe = INTEGER(sxe);
        const int *ys = INTEGER(sys);
        const int *ye = INTEGER(sye);
        int *zs = INTEGER(szs);
        int *ze = INTEGER(sze);
        while (k < nz) {
            if (x_gt_y_i(xs[i], ye[j])) ++j;
            else if (x_gt_y_i(ys[j], xe[i])) ++i;
            else {
                zs[k] = max_i(ys[j], xs[i]);
                ze[k] = min_i(ye[j], xe[i]);
                if (ye[j] == ze[k]) ++j;
                if (xe[i] == ze[k]) ++i;
                ++k;
            }
        }
    }

    sz = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(sz, 0, szs);
    SET_VECTOR_ELT(sz, 1, sze);
    UNPROTECT(3);
    return sz;
}


// match
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
match_tint(SEXP sx, SEXP sts, SEXP ste, SEXP sno)
{
    size_t nx = XLENGTH(sx);
    SEXP sm = PROTECT(allocVector(INTSXP, nx));
    if (!nx) { UNPROTECT(1); return sm; }

    size_t nt = XLENGTH(sts), i, j;
    int *m = INTEGER(sm);
    int no = *INTEGER(sno);

    for (i = 0; i < nx; ++i) m[i] = no;
    if (!nt) { UNPROTECT(1); return sm; }

    int real = isReal(sx);

    if (real) {
        const double *x = REAL(sx);
        const double *ts = REAL(sts);
        const double *te = REAL(ste);
        for (i = 0; i < nx; ++i) {
            double xi = x[i];
            // NAs are _never_ matched
            if (!(R_FINITE(xi))) continue;
            for (j = 0; j < nt; ++j) {
                if (x_ge_y_d(xi, te[j])) continue;
                if (x_gt_y_d(ts[j], xi)) continue;
                m[i] = j + 1;
                break;
            }
        }
    } else {
        const int *x = INTEGER(sx);
        const int *ts = INTEGER(sts);
        const int *te = INTEGER(ste);
        for (i = 0; i < nx; ++i) {
            int xi = x[i];
            // NAs are _never_ matched
            if (xi == NA_INTEGER) continue;
            for (j = 0; j < nt; ++j) {
                if (x_gt_y_i(xi, te[j])) continue;
                if (x_gt_y_i(ts[j], xi)) continue;
                m[i] = j + 1;
                break;
            }
        }
    }

    UNPROTECT(1);
    return sm;
}


// optimised impementation of %in_t% for ordered indices assuming unique
// representation of time intervals
// ==================================================================

// integers
TIND__ATTRIBUTE_FUNCTION
SEXP
in_tint_ord(SEXP sx, SEXP sts, SEXP ste)
{
    int nx, nt;

    nx = length(sx);
    SEXP sin = PROTECT(allocVector(LGLSXP, nx));
    if (!nx) { UNPROTECT(1); return sin; }

    nt = length(sts);
    int *in = INTEGER(sin);
    int real = isReal(sx);

    size_t i = 0, j = 0;
    if (real) {
        const double *ts = REAL(sts);
        const double *te = REAL(ste);
        const double *x = REAL(sx);
        while ((i < nx) && (j < nt)) {
            if (x_ge_y_d(x[i], te[j])) ++j;
            else if (x_gt_y_d(ts[j], x[i])) in[i++] = 0;
            else in[i++] = 1;
        }
    } else {
        const int *ts = INTEGER(sts);
        const int *te = INTEGER(ste);
        const int *x = INTEGER(sx);
        while ((i < nx) && (j < nt)) {
            if (x_gt_y_i(x[i], te[j])) ++j;
            else if (x_gt_y_i(ts[j], x[i])) in[i++] = 0;
            else in[i++] = 1;
        }
    }
    for (; i < nx; ++i) in[i] = 0;

    UNPROTECT(1);
    return sin;
}

