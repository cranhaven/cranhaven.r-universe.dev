/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ******************************
 * resolution of time indices *
 ******************************
 */


#include "resolution.h"
#include "calendar.h"
#include "compiler.h"


// years, ..., days
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
res_y(SEXP sx)
{
    size_t i, n;
    int res[12] = { 1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000 };
    int r = 11, allna = 1;
    const int *x;

    n = XLENGTH(sx);
    x = INTEGER(sx);

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) continue;
        if (allna) allna = 0;
        int cr = res[r];
        if (!(x[i] % cr)) continue;
        while (--r && ((x[i] % res[r]) || (cr % res[r])));
        if (!r) break;
    }

    return ScalarInteger(!allna && (n > 1) ? res[r] : 1);
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_q(SEXP sx)
{
    size_t i, n;
    int res[3] = { 1, 2, 4 };
    int r = 2, allna = 1;
    const int *x;

    n = XLENGTH(sx);
    x = INTEGER(sx);

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) continue;
        if (allna) allna = 0;
        int cr = res[r], qq = x[i] % 4;
        if (!(qq % cr)) continue;
        while (--r && (qq % res[r]));
        if (!r) break;
    }

    return ScalarInteger(!allna && (n > 1) ? res[r] : 1);
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_m(SEXP sx)
{
    size_t i, n;
    int res[6] = { 1, 2, 3, 4, 6, 12 };
    int r = 5, allna = 1;
    const int *x;

    n = XLENGTH(sx);
    x = INTEGER(sx);

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) continue;
        if (allna) allna = 0;
        int cr = res[r], mm = x[i] % 12;
        if (!(mm % cr)) continue;
        while (--r && ((mm % res[r]) || (cr % res[r])));
        if (!r) break;
    }

    return ScalarInteger(!allna && (n > 1) ? res[r] : 1);
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_w(SEXP sx)
{
    size_t i, n;
    int res[6] = { 1, 2, 4, 13, 26, 52 };
    int r = 5, allna = 1;
    const int *x;

    n = XLENGTH(sx);
    x = INTEGER(sx);

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) continue;
        if (allna) allna = 0;
        int yy, ww;
        w2yw(x[i], &yy, &ww);
        --ww;
        if (ww == 52) return ScalarInteger(1);
        int cr = res[r];
        if (!(ww % cr)) continue;
        while (--r && ((ww % res[r]) || (cr % res[r])));
        if (!r) break;
    }

    return ScalarInteger(!allna && (n > 1) ? res[r] : 1);
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_d(SEXP sx)
{
    size_t i, n;
    int res[4] = { 1, 7, 15, 30 };
    int r, r0 = 3, r1 = 1, allna = 1;
    const int *x;

    n = XLENGTH(sx);
    x = INTEGER(sx);

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) continue;
        if (allna) allna = 0;
        int yy, mm, dd, dw;
        d2ymd(x[i], &yy, &mm, &dd);
        if (r0) {
            if (dd == 16) r0 = 2; else if (dd != 1) r0 = 0;
        }
        if (r1) {
            dw = dayofweek(x[i]);
            if (dw != 1) r1 = 0;
        }
        if (!r0 && !r1) break;
    }

    r = (r1 >= r0) ? r1 : r0;

    return ScalarInteger(!allna && (n > 1) ? res[r] : 1);
}


// time, date-time
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
res_s(SEXP sx)
{
    size_t i, n;
    int res[12] = { 1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60 };
    int r = 11, allna = 1;
    const double *x;

    n = XLENGTH(sx);
    x = REAL(sx);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) continue;
        if (allna) allna = 0;
        if (((int) round((x[i] - floor(x[i])) * 1E6)) % 1000000)
            return ScalarInteger(0);
        int cr = res[r], s;
        double ds;
        ds = x[i] / 60.;
        s = (int) round((ds - floor(ds)) * 60);
        if (!(s % cr)) continue;
        while (--r && ((s % res[r]) || (cr % res[r])));
    }

    return ScalarReal((double) (allna ? 1 : ((n > 1) || (r == 11)) ? res[r] : 1));
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_subs(SEXP sx)
{
    size_t i, n;
    int pow10[6] = { 1, 10, 100, 1000, 10000, 100000 };
    int mult[3] = { 1, 2, 5 };
    int p = 5, m = 2;
    const double *x;

    n = XLENGTH(sx);
    x = REAL(sx);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) continue;
        double ds = x[i] - floor(x[i]);
        int ss = (int) round(ds * 1E6);
        while ((p || m) && (ss % (pow10[p] * mult[m]))) {
            if (m) --m; else { --p; m = 2; }
        }
        if (!p && !m) break;
    }

    return ScalarReal((double) mult[m] * (double) pow10[p] * 1E-6);
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_min(SEXP sx)
{
    size_t i, n;
    int res[12] = { 1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60 };
    int r = 11;
    const double *x;

    n = XLENGTH(sx);
    x = REAL(sx);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) continue;
        int cr = res[r], m = (int) x[i] / 60;
        if (!(m % cr)) continue;
        while (--r && ((m % res[r]) || (cr % res[r])));
        if (!r) break;
    }

    return ScalarInteger(((n > 1) || (r == 11)) ? res[r] : 1);
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_h(SEXP sx)
{
    size_t i, n;
    int res[8] = { 1, 2, 3, 4, 6, 8, 12, 24 };
    int r = 7;
    const double *x;

    n = XLENGTH(sx);
    x = REAL(sx);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) continue;
        int cr = res[r], h = (int) x[i] / 3600;
        if (!(h % cr)) continue;
        while (--r && ((h % res[r]) || (cr % res[r])));
        if (!r) break;
    }

    return ScalarInteger((n > 1) ? res[r] : 1);
}


// time unit for time differences
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
tunit(SEXP sx)
{
    size_t i, n;
    double res[3] = { 1., 60., 3600. };
    int r = 2, all0na = 1;
    const double *x;

    n = XLENGTH(sx);
    x = REAL(sx);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) continue;
        double xi = fabs(x[i]);
        if (xi == 0.) continue;
        if (all0na) all0na = 0;
        if (((int) round((xi - floor(xi)) * 1E6)) % 1000000) {
            r = 0;
            break;
        }
        while (r) {
            double rx;
            rx = xi / res[r];
            if (rx - floor(rx) > 1E-4) --r; else break;
        }
        if (!r) break;
    }

    if (all0na) r = 0;

    if (r == 2) return mkString("h");
    if (r == 1) return mkString("min");
    return mkString("s");
}


// integer indices - resolution is the greatest common divisor
// ==================================================================

static inline
TIND__ATTRIBUTE_INLINE
int
gcd(int a, int b)
{
    a = abs(a);
    b = abs(b);
    if (b > a) {
        int c = a;
        a = b;
        b = c;
    }
    // Euclidean algorithm
    while (b) {
        int r = a % b;
        a = b;
        b = r;
    }

    return a;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
res_i(SEXP sx)
{
    size_t i, n;
    int res = 0;
    const int *x;

    n = XLENGTH(sx);
    x = INTEGER(sx);

    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) continue;
        res = gcd(x[i], res);
        if (res == 1) break;
    }

    return ScalarInteger(((n == 1) || (res < 2)) ? 1 : res);
}

