/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 *************
 * date-time *
 *************
 */


#include "datetime.h"
#include "calendar.h"


// validation
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
validate_t(SEXP sx)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    double *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(REALSXP, n));
    y = REAL(sy);

    for (i = 0; i < n; ++i)
        y[i] = (R_FINITE(x[i])) && (VALID_T_MIN <= x[i]) &&
               (x[i] <= VALID_T_MAX) ? round6(x[i]) : NA_REAL;

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_h(SEXP sx)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    double *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(REALSXP, n));
    y = REAL(sy);

    for (i = 0; i < n; ++i)
        y[i] = (R_FINITE(x[i])) && (0. <= x[i]) &&
               (x[i] <= 86400.) ? round6(x[i]) : NA_REAL;

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_hms(SEXP sh, SEXP sm, SEXP ss)
{
    size_t i, j, k, nh, nm, ns, nr;
    const int *h, *m;
    const double *s;
    double *r;
    SEXP sr;

    nh = XLENGTH(sh);
    nm = XLENGTH(sm);
    ns = XLENGTH(ss);

    nr = (nh >= nm) ? ((nh >= ns) ? nh : ns) : ((nm >= ns) ? nm : ns);
    sr = PROTECT(allocVector(REALSXP, nr));
    h = INTEGER(sh);
    m = INTEGER(sm);
    s = REAL(ss);
    r = REAL(sr);

    if (nh == nm && nm == ns) {
        for (i = 0; i < nr; ++i) {
            if (h[i] == NA_INTEGER || m[i] == NA_INTEGER || !(R_FINITE(s[i]))) {
                r[i] = NA_REAL; continue;
            }
            r[i] = validatehms(h[i], m[i], s[i]);
        }
    } else if (nm == 1 && ns == 1) {
        if (*m == NA_INTEGER || !(R_FINITE(*s))) {
            for (i = 0; i < nr; ++i) r[i] = NA_REAL;
        } else for (i = 0; i < nr; ++i) {
            if (h[i] == NA_INTEGER) { r[i] = NA_REAL; continue; }
            r[i] = validatehms(h[i], *m, *s);
        }
    } else if (nh == 1 && ns == 1) {
        if (*h == NA_INTEGER || !(R_FINITE(*s))) {
            for (i = 0; i < nr; ++i) r[i] = NA_REAL;
        } else for (i = 0; i < nr; ++i) {
            if (m[i] == NA_INTEGER) { r[i] = NA_REAL; continue; }
            r[i] = validatehms(*h, m[i], *s);
        }
    } else if (nh == 1 && nm == 1) {
        if (*h == NA_INTEGER || *m == NA_INTEGER) {
            for (i = 0; i < nr; ++i) r[i] = NA_REAL;
        } else for (i = 0; i < nr; ++i) {
            if (!(R_FINITE(s[i]))) { r[i] = NA_REAL; continue; }
            r[i] = validatehms(*h, *m, s[i]);
        }
    } else if (nh == nr) {
        for (i = 0; i < nr; ++i) {
            j = i % nm;
            k = i % ns;
            if (h[i] == NA_INTEGER || m[j] == NA_INTEGER || !(R_FINITE(s[k]))) {
                r[i] = NA_REAL; continue;
            }
            r[i] = validatehms(h[i], m[j], s[k]);
        }
    } else if (nm == nr) {
        for (j = 0; j < nr; ++j) {
            i = j % nh;
            k = j % ns;
            if (h[i] == NA_INTEGER || m[j] == NA_INTEGER || !(R_FINITE(s[k]))) {
                r[j] = NA_REAL; continue;
            }
            r[j] = validatehms(h[i], m[j], s[k]);
        }
    } else {
        for (k = 0; k < nr; ++k) {
            i = k % nh;
            j = k % nm;
            if (h[i] == NA_INTEGER || m[j] == NA_INTEGER || !(R_FINITE(s[k]))) {
                r[k] = NA_REAL; continue;
            }
            r[k] = validatehms(h[i], m[j], s[k]);
        }
    }

    if (nh == nr) {
        SEXP nmsh = getAttrib(sh, R_NamesSymbol);
        if ((nr > nm) && (nr > ns)) {
            setAttrib(sr, R_NamesSymbol, nmsh);
        } else if ((nr == nm) && (nr > ns)) {
            if (!isNull(nmsh)) setAttrib(sr, R_NamesSymbol, nmsh);
            else setAttrib(sr, R_NamesSymbol, getAttrib(sm, R_NamesSymbol));
        } else if ((nr > nm) && (nr == ns)) {
            if (!isNull(nmsh)) setAttrib(sr, R_NamesSymbol, nmsh);
            else setAttrib(sr, R_NamesSymbol, getAttrib(ss, R_NamesSymbol));
        } else {
            if (!isNull(nmsh)) setAttrib(sr, R_NamesSymbol, nmsh);
            else {
                SEXP nmsm = getAttrib(sm, R_NamesSymbol);
                if (!isNull(nmsm)) setAttrib(sr, R_NamesSymbol, nmsm);
                else setAttrib(sr, R_NamesSymbol, getAttrib(ss, R_NamesSymbol));
            }
        }
    } else if (nm == nr) {
        SEXP nmsm = getAttrib(sm, R_NamesSymbol);
        if (nr > ns) {
            setAttrib(sr, R_NamesSymbol, nmsm);
        } else {
            if (!isNull(nmsm)) setAttrib(sr, R_NamesSymbol, nmsm);
            else setAttrib(sr, R_NamesSymbol, getAttrib(ss, R_NamesSymbol));
        }
    } else {
        setAttrib(sr, R_NamesSymbol, getAttrib(ss, R_NamesSymbol));
    }
    UNPROTECT(1);
    return sr;
}



// POSIXlt conversion
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
plt_ymd2d(SEXP sy, SEXP sm, SEXP sd, SEXP snms)
{
    size_t i, n = XLENGTH(sy);
    const int *y, *m, *d;
    int *r;
    SEXP sr;

    y = INTEGER(sy);
    m = INTEGER(sm);
    d = INTEGER(sd);
    sr = PROTECT(allocVector(INTSXP, n));
    r = INTEGER(sr);

    for (i = 0; i < n; ++i) {
        if (y[i] == NA_INTEGER) { r[i] = NA_INTEGER; continue; }
        int yy = y[i], mm = m[i] + 1;
        int a = (14 - mm) / 12;
        yy += 6700 - a;
        mm += 12 * a - 3;
        r[i] = d[i] + (153 * mm + 2) / 5 + yy * 365 + yy / 4 - yy / 100 + yy / 400 - 2472633;
    }

    setAttrib(sr, R_NamesSymbol, snms);
    UNPROTECT(1);
    return sr;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
plt_hms2h(SEXP sh, SEXP sm, SEXP ss, SEXP snms)
{
    size_t i, n = XLENGTH(sh);
    const int *h, *m;
    const double *s;
    double *r;
    SEXP sr;

    h = INTEGER(sh);
    m = INTEGER(sm);
    s = REAL(ss);
    sr = PROTECT(allocVector(REALSXP, n));
    r = REAL(sr);

    for (i = 0; i < n; ++i) {
        if (h[i] == NA_INTEGER) { r[i] = NA_REAL; continue; }
        r[i] = 3600 * h[i] + 60 * m[i] + round6(s[i]);
    }

    setAttrib(sr, R_NamesSymbol, snms);
    UNPROTECT(1);
    return sr;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
plt_midnightdiff(SEXP sd, SEXP sy, SEXP sm, SEXP sd1, SEXP sh, SEXP smin, SEXP ss)
{
    size_t i, n = XLENGTH(sd);
    const int *d, *y, *m, *d1, *h, *min;
    const double *s;
    double *r;
    SEXP sr;

    d = INTEGER(sd);
    y = INTEGER(sy);
    m = INTEGER(sm);
    d1 = INTEGER(sd1);
    h = INTEGER(sh);
    min = INTEGER(smin);
    s = REAL(ss);
    sr = PROTECT(allocVector(REALSXP, n));
    r = REAL(sr);

    for (i = 0; i < n; ++i) {
        r[i] = 3600 * h[i] + 60 * min[i] + round6(s[i]);
        int yy = y[i], mm = m[i] + 1;
        int a = (14 - mm) / 12;
        yy += 6700 - a;
        mm += 12 * a - 3;
        int dd1 = d1[i] + (153 * mm + 2) / 5 + yy * 365 + yy / 4 - yy / 100 + yy / 400 - 2472633;
        if (dd1 != d[i]) r[i] -= 86400;
    }

    UNPROTECT(1);
    return sr;
}



// auxiliary function used by dhz2t
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
dh2t_aux(SEXP st0, SEXP st1, SEXP sh)
{
    size_t i, n = XLENGTH(st0);
    const double *t0 = REAL(st0), *t1 = REAL(st1), *h = REAL(sh);

    SEXP st = PROTECT(allocVector(REALSXP, n));
    double *t = REAL(st);
    SEXP schk = PROTECT(allocVector(LGLSXP, n));
    int *chk = INTEGER(schk);

    for (i = 0; i < n; ++i) {
        if (!R_FINITE(t0[i]) || !R_FINITE(h[i])) {
            chk[i] = 0;
            t[i] = NA_REAL;
            continue;
        }
        if (h[i] == 86400) {
            chk[i] = 0;
            t[i] = t1[i];
            continue;
        }
        chk[i] = (t1[i] - t0[i] == 86400) ? 0 : 1;
        t[i] = t0[i] + h[i];
    }

    SEXP sres = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(sres, 0, st);
    SET_VECTOR_ELT(sres, 1, schk);
    UNPROTECT(3);
    return sres;
}



// time components (h, m, s)
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
h2hour(SEXP sx)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    int *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(INTSXP, n));
    y = INTEGER(sy);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) { y[i] = NA_INTEGER; continue; }
        y[i] = (int) floor(x[i] / 3600.);
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
h2min(SEXP sx)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    int *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(INTSXP, n));
    y = INTEGER(sy);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) { y[i] = NA_INTEGER; continue; }
        y[i] = ((int) floor(x[i] / 60.)) % 60;
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
h2sec(SEXP sx)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    double *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(REALSXP, n));
    y = REAL(sy);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) { y[i] = NA_REAL; continue; }
        y[i] = x[i] - floor(x[i] / 60.) * 60.;
        y[i] = round6(y[i]);
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}



// floor, ceiling
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
floor_h(SEXP sx, SEXP snun)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    double *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(REALSXP, n));
    y = REAL(sy);
    double nun = *REAL(snun);
    double rnun1e6 = round(nun * 1e6);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) { y[i] = NA_REAL; continue; }
        y[i] = floor(round(x[i] * 1e6) / rnun1e6) * nun;
        y[i] = round6(y[i]);
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}


TIND__ATTRIBUTE_FUNCTION
SEXP
ceiling_h(SEXP sx, SEXP snun)
{
    size_t i, n = XLENGTH(sx);
    const double *x;
    double *y;
    SEXP sy;

    x = REAL(sx);
    sy = PROTECT(allocVector(REALSXP, n));
    y = REAL(sy);
    double nun = *REAL(snun);
    double rnun1e6 = round(nun * 1e6);

    for (i = 0; i < n; ++i) {
        if (!(R_FINITE(x[i]))) { y[i] = NA_REAL; continue; }
        double xx = round(x[i] * 1e6) / rnun1e6;
        double fx = floor(xx);
        if (xx != fx) fx += 1.;
        y[i] = round6(fx * nun);
    }

    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol));
    UNPROTECT(1);
    return sy;
}

