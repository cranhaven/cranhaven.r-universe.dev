/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ***********************************************************************
 * calendrical computations (years, quarters, months, weeks, and days) *
 ***********************************************************************
 */


#include "calendar.h"


// ==================================================================
// basic ops
// ==================================================================

#define I2I_START_ \
    size_t i, n; \
    const int *x; \
    int *y; \
    SEXP sy; \
 \
    n = XLENGTH(sx); \
    x = INTEGER(sx); \
    sy = PROTECT(allocVector(INTSXP, n)); \
    y = INTEGER(sy);

#define I2I_END_ \
    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol)); \
    UNPROTECT(1); \
    return sy;

#define I2L_START_ \
    size_t i, n; \
    const int *x; \
    int *y; \
    SEXP sy; \
 \
    n = XLENGTH(sx); \
    x = INTEGER(sx); \
    sy = PROTECT(allocVector(LGLSXP, n)); \
    y = INTEGER(sy); \

#define I2L_END_ I2I_END_


// years
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
validate_y(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) y[i] = validy(x[i]) ? x[i] : NA_INTEGER;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
is_leap_year(SEXP sx)
{
    I2L_START_
    for (i = 0; i < n; ++i)
        y[i] = validy(x[i]) ? isleapyear(x[i]) : NA_LOGICAL;
    I2L_END_
}


// quarters
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
validate_q(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = ((x[i] >= VALID_Q_MIN) && (x[i] <= VALID_Q_MAX)) ? x[i] : NA_INTEGER;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
q2y(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] / 4;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
y2q(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] * 4;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
q2qrtr(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] % 4 + 1;
    I2I_END_
}


// months
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
validate_m(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = ((x[i] >= VALID_M_MIN) && (x[i] <= VALID_M_MAX)) ? x[i] : NA_INTEGER;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
m2y(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] / 12;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
y2m(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] * 12;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
m2q(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] / 3;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
q2m(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] * 3;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
m2mnth(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] % 12 + 1;
    I2I_END_
}


// weeks
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
weeks_in_year(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : weeksinyear(x[i]);
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_w(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = ((x[i] >= VALID_W_MIN) && (x[i] <= VALID_W_MAX)) ? x[i] : NA_INTEGER;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
w2week(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy;
        w2yw(x[i], &yy, y + i);
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
w2y(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int ww;
        w2yw(x[i], y + i, &ww);
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
y2w(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = validateyw(x[i], 1);
    I2I_END_
}


// days
// ==================================================================

TIND__ATTRIBUTE_FUNCTION
SEXP
days_in_quarter(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int qq;
        qq = x[i] % 4;
        if (qq >= 2) { y[i] = 92; continue; }
        if (qq == 1) { y[i] = 91; continue; }
        y[i] = 90 + isleapyear(x[i] / 4);
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
days_in_month(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        y[i] = daysinmonth(x[i] / 12, x[i] % 12 + 1);
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
validate_d(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = ((x[i] >= VALID_D_MIN) && (x[i] <= VALID_D_MAX)) ? x[i] : NA_INTEGER;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
day_of_year(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : dayofyear(x[i]);
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
day_of_week(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : dayofweek(x[i]);
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2day(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy, mm, dd;
        d2ymd(x[i], &yy, &mm, &dd);
        y[i] = dd;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2y(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy, mm, dd;
        d2ymd(x[i], &yy, &mm, &dd);
        y[i] = yy;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
y2d(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy = x[i] + 4799;
        y[i] = yy * 365 + yy / 4 - yy / 100 + yy / 400 - 2472326;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2q(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy, mm, dd;
        d2ymd(x[i], &yy, &mm, &dd);
        y[i] = yy * 4 + (mm - 1) / 3;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
q2d(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy = x[i] / 4;
        int mm = 3 * (x[i] % 4) + 1;
        int a = (14 - mm) / 12;
        yy += 4800 - a;
        mm += 12 * a - 3;
        y[i] = (153 * mm + 2) / 5 + yy * 365 + yy / 4 - yy / 100 + yy / 400 - 2472632;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2m(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy, mm, dd;
        d2ymd(x[i], &yy, &mm, &dd);
        y[i] = yy * 12 + mm - 1;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
m2d(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy = x[i] / 12;
        int mm = x[i] % 12 + 1;
        int a = (14 - mm) / 12;
        yy += 4800 - a;
        mm += 12 * a - 3;
        y[i] = (153 * mm + 2) / 5 + yy * 365 + yy / 4 - yy / 100 + yy / 400 - 2472632;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
last_day_in_month(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy = x[i] / 12;
        int mm = x[i] % 12 + 1;
        int dim = daysinmonth(yy, mm);
        int a = (14 - mm) / 12;
        yy += 4800 - a;
        mm += 12 * a - 3;
        y[i] = (153 * mm + 2) / 5 + yy * 365 + yy / 4 - yy / 100 + yy / 400 +
               dim - 2472633;
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
w2d(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : 7 * x[i] - 3;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2w(SEXP sx)
{
    I2I_START_
    int VALID_D2W_MIN = 7 * VALID_W_MIN - 3;
    for (i = 0; i < n; ++i) {
        if (x[i] < VALID_D2W_MIN) { y[i] = NA_INTEGER; continue; }
        y[i] = div_(x[i] + 3, 7);
    }
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2jdn(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] + 2440588;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
jdn2d(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i)
        y[i] = (x[i] == NA_INTEGER) ? NA_INTEGER : x[i] - 2440588;
    I2I_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
easter(SEXP sx)
{
    I2I_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        int yy, gg, cc, hh, ii, jj, ll, mm, dd;
        yy = x[i];
        gg = yy % 19;
        cc = yy / 100;
        hh = (cc - cc / 4 - (8 * cc + 13) / 25 + 19 * gg + 15) % 30;
        ii = hh - (hh / 28) * (1 - (29 / (hh + 1)) * ((21 - gg) / 11));
        jj = (yy + yy / 4 + ii + 2 - cc + cc / 4) % 7;
        ll = ii - jj;
        mm = 3 + (ll + 40) / 44;
        dd = ll + 28 - 31 * (mm / 4);
        y[i] = ymd2d(yy, mm, dd);
    }
    I2I_END_
}


#undef I2L_START_
#undef I2L_END_
#undef I2I_START_
#undef I2I_END_



// ==================================================================
// year fractions
// ==================================================================

#define I2R_START_ \
    size_t i, n; \
    const int *x; \
    double *y; \
    SEXP sy; \
 \
    n = XLENGTH(sx); \
    x = INTEGER(sx); \
    sy = PROTECT(allocVector(REALSXP, n)); \
    y = REAL(sy);

#define I2R_END_ \
    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol)); \
    UNPROTECT(1); \
    return sy;


TIND__ATTRIBUTE_FUNCTION
SEXP yqm2yf(SEXP sx, SEXP stype)
{
    I2R_START_
    char type = *CHAR(STRING_ELT(stype, 0));
    switch (type) {
        case 'y':
            for (i = 0; i < n; ++i)
                y[i] = (x[i] == NA_INTEGER) ? NA_REAL : (double) x[i];
            break;
        case 'q':
            for (i = 0; i < n; ++i)
                y[i] = (x[i] == NA_INTEGER) ? NA_REAL : (double) x[i] / 4.;
            break;
        case 'm':
            for (i = 0; i < n; ++i)
                y[i] = (x[i] == NA_INTEGER) ? NA_REAL : (double) x[i] / 12.;
            break;
    }
    I2R_END_
}




TIND__ATTRIBUTE_FUNCTION
SEXP
w2yf(SEXP sx)
{
    I2R_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = NA_REAL; continue; }
        int yy, ww;
        w2yw(x[i], &yy, &ww);
        y[i] = (double) (ww - 1) / (double) weeksinyear(yy) + (double) yy;
    }
    I2R_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
d2yf(SEXP sx)
{
    I2R_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = NA_REAL; continue; }
        int yy, mm, dd, doy;
        d2ymd(x[i], &yy, &mm, &dd);
        if (mm == 1) {
            doy = dd - 1;
        } else if (mm == 2) {
            doy = dd + 30;
        } else {
            doy = 58 + isleapyear(yy) + dd + (153 * (mm - 3) + 2) / 5;
        }
        y[i] = (double) doy / (double) (365 + isleapyear(yy)) + (double) yy;
    }
    I2R_END_
}


#undef I2R_START_
#undef I2R_END_


// ==================================================================
// floor, ceiling
// ==================================================================

#define FC_START_ \
    size_t i, n; \
    const int *x; \
    int *y, a; \
    SEXP sy; \
 \
    n = XLENGTH(sx); \
    x = INTEGER(sx); \
    a = *INTEGER(sa); \
    if (a == 1) return sx; \
    sy = PROTECT(allocVector(INTSXP, n)); \
    y = INTEGER(sy);

#define FC_END_ \
    setAttrib(sy, R_NamesSymbol, getAttrib(sx, R_NamesSymbol)); \
    UNPROTECT(1); \
    return sy;


TIND__ATTRIBUTE_FUNCTION
SEXP
floor_yqm(SEXP sx, SEXP sa)
{
    FC_START_
    for (i = 0; i < n; ++i) {
        if (x[i] == NA_INTEGER) { y[i] = x[i]; continue; }
        y[i] = div_(x[i], a);
        y[i] *= a;
    }
    FC_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
ceiling_y(SEXP sx, SEXP sa)
{
    FC_START_
    for (i = 0; i < n; ++i) {
        if ((x[i] == NA_INTEGER) || !(x[i] % a)) { y[i] = x[i]; continue; }
        y[i] = div_(x[i], a);
        ++y[i];
        y[i] *= a;
        if (y[i] > VALID_Y_MAX) y[i] = NA_INTEGER;
    }
    FC_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
ceiling_q(SEXP sx, SEXP sa)
{
    FC_START_
    for (i = 0; i < n; ++i) {
        if ((x[i] == NA_INTEGER) || !(x[i] % a)) { y[i] = x[i]; continue; }
        y[i] = div_(x[i], a);
        ++y[i];
        y[i] *= a;
        if (y[i] > VALID_Q_MAX) y[i] = NA_INTEGER;
    }
    FC_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
ceiling_m(SEXP sx, SEXP sa)
{
    FC_START_
    for (i = 0; i < n; ++i) {
        if ((x[i] == NA_INTEGER) || !(x[i] % a)) { y[i] = x[i]; continue; }
        y[i] = div_(x[i], a);
        ++y[i];
        y[i] *= a;
        if (y[i] > VALID_M_MAX) y[i] = NA_INTEGER;
    }
    FC_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
floor_w(SEXP sx, SEXP sa)
{
    FC_START_
    for (i = 0; i < n; ++i) {
        y[i] = x[i];
        if (y[i] == NA_INTEGER) continue;
        int yy, ww, nw;
        w2yw(y[i], &yy, &ww);
        --ww;
        nw = (ww == 52) ? (ww - 1) / a * a : ww / a * a;
        y[i] -= ww - nw;
    }
    FC_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
ceiling_w(SEXP sx, SEXP sa)
{
    FC_START_
    for (i = 0; i < n; ++i) {
        y[i] = x[i];
        if (y[i] == NA_INTEGER) continue;
        int yy, ww, nw;
        w2yw(y[i], &yy, &ww);
        --ww;
        nw = (ww == 52) ? 53 : ((ww % a) ? (ww / a + 1) * a : ww);
        if ((nw == 52) && (weeksinyear(yy) == 53)) ++nw;
        y[i] += nw - ww;
        if (y[i] > VALID_W_MAX) y[i] = NA_INTEGER;
    }
    FC_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
floor_d(SEXP sx, SEXP sa)
{
    FC_START_
    switch (a) {
        case 2:
        case 3:
            for (i = 0; i < n; ++i) {
                y[i] = x[i];
                if (y[i] == NA_INTEGER) continue;
                int dw = mod_(y[i] + 3, 7);
                if (dw == 6) { --y[i]; --dw; }
                y[i] -= dw % a;
                if (y[i] < VALID_D_MIN) y[i] = NA_INTEGER;
            }
            break;
        case 7:
            for (i = 0; i < n; ++i) {
                y[i] = x[i];
                if (y[i] == NA_INTEGER) continue;
                y[i] -= mod_(y[i] + 3, 7);
                if (y[i] < VALID_D_MIN) y[i] = NA_INTEGER;
            }
            break;
        case 15:
        case 30:
            for (i = 0; i < n; ++i) {
                y[i] = x[i];
                if (y[i] == NA_INTEGER) continue;
                int yy, mm, dd, nd;
                d2ymd(y[i], &yy, &mm, &dd);
                nd = ((a == 15) && (dd >= 16)) ? 16 : 1;
                y[i] -= dd - nd;
            }
            break;
    }
    FC_END_
}


TIND__ATTRIBUTE_FUNCTION
SEXP
ceiling_d(SEXP sx, SEXP sa)
{
    FC_START_
    switch (a) {
        case 2:
        case 3:
            for (i = 0; i < n; ++i) {
                y[i] = x[i];
                if (x[i] == NA_INTEGER) continue;
                int dw = mod_(x[i] + 3, 7);
                if (!dw) continue;
                y[i] += (6 - dw) % a;
                if ((dw > 4) || ((a == 3) && (dw == 4))) ++y[i];
                if (y[i] > VALID_D_MAX) y[i] = NA_INTEGER;
            }
            break;
        case 7:
            for (i = 0; i < n; ++i) {
                y[i] = x[i];
                if (x[i] == NA_INTEGER) continue;
                int dw = mod_(x[i] + 3, 7);
                if (!dw) continue;
                y[i] += 7 - dw;
                if (y[i] > VALID_D_MAX) y[i] = NA_INTEGER;
            }
            break;
        case 15:
        case 30:
            for (i = 0; i < n; ++i) {
                y[i] = x[i];
                if (x[i] == NA_INTEGER) continue;
                int yy, mm, dd;
                d2ymd(x[i], &yy, &mm, &dd);
                if (dd == 1) continue;
                int nd = ((a == 15) && (dd <= 16)) ? 16 : daysinmonth(yy, mm) + 1;
                y[i] += nd - dd;
            }
            break;
    }
    FC_END_
}



#undef FC_START_
#undef FC_END_


// ==================================================================
// 2-argument operations
// ==================================================================

#define BINOP_IMPL(NAME) \
TIND__ATTRIBUTE_FUNCTION \
SEXP \
NAME(SEXP sx, SEXP sy) \
{ \
    size_t i, j, nx, ny, nz; \
    int *x, *y, *z; \
    SEXP sz; \
 \
    nx = XLENGTH(sx); \
    ny = XLENGTH(sy); \
    nz = (nx > ny) ? nx : ny; \
    sz = PROTECT(allocVector(INTSXP, nz)); \
    x = INTEGER(sx); \
    y = INTEGER(sy); \
    z = INTEGER(sz); \
 \
    if (nx == ny) { \
        for (i = 0; i < nz; ++i) { \
            if (x[i] == NA_INTEGER || y[i] == NA_INTEGER) { z[i] = NA_INTEGER; continue; } \
            z[i] = NAME##_(x[i], y[i]); \
        } \
    } else if (nx == 1) { \
        if (*x == NA_INTEGER) { \
            for (i = 0; i < nz; ++i) z[i] = NA_INTEGER; \
        } else for (i = 0; i < nz; ++i) { \
            if (y[i] == NA_INTEGER) { z[i] = NA_INTEGER; continue; } \
            z[i] = NAME##_(*x, y[i]); \
        } \
    } else if (ny == 1) { \
        if (*y == NA_INTEGER) { \
            for (i = 0; i < nz; ++i) z[i] = NA_INTEGER; \
        } else for (i = 0; i < nz; ++i) { \
            if (x[i] == NA_INTEGER) { z[i] = NA_INTEGER; continue; } \
            z[i] = NAME##_(x[i], *y); \
        } \
    } else if (nx > ny) { \
        for (i = 0; i < nz; ++i) { \
            j = i % ny; \
            if (x[i] == NA_INTEGER || y[j] == NA_INTEGER) { z[i] = NA_INTEGER; continue; } \
            z[i] = NAME##_(x[i], y[j]); \
        } \
    } else { \
        for (j = 0; j < nz; ++j) { \
            i = j % nx; \
            if (x[i] == NA_INTEGER || y[j] == NA_INTEGER) { z[j] = NA_INTEGER; continue; } \
            z[j] = NAME##_(x[i], y[j]); \
        } \
    } \
 \
    if (nx > ny) { \
        setAttrib(sz, R_NamesSymbol, getAttrib(sx, R_NamesSymbol)); \
    } else if (nx < ny) { \
        setAttrib(sz, R_NamesSymbol, getAttrib(sy, R_NamesSymbol)); \
    } else { \
        if (!isNull(getAttrib(sx, R_NamesSymbol))) \
            setAttrib(sz, R_NamesSymbol, getAttrib(sx, R_NamesSymbol)); \
        else \
            setAttrib(sz, R_NamesSymbol, getAttrib(sy, R_NamesSymbol)); \
    } \
    UNPROTECT(1); \
    return sz; \
}


// quarters
// ==================================================================

#ifdef validate_yq_
#undef validate_yq_
#endif /* validate_yq_ */
#define validate_yq_ validateyq
BINOP_IMPL(validate_yq)
#undef validate_yq_


// months
// ==================================================================

#ifdef validate_ym_
#undef validate_ym_
#endif /* validate_ym_ */
#define validate_ym_ validateym
BINOP_IMPL(validate_ym)
#undef validate_ym_


// weeks
// ==================================================================

#ifdef validate_yw_
#undef validate_yw_
#endif /* validate_yw_ */
#define validate_yw_ validateyw
BINOP_IMPL(validate_yw)
#undef validate_yw_



static inline
TIND__ATTRIBUTE_INLINE
int
inc_w_by_y_(int w, int by)
{
    int yy, ww, ny, nw;
    w2yw(w, &yy, &ww);
    ny = yy + by;
    if (!validy(ny)) return NA_INTEGER;
    nw = (weeksinyear(ny) < ww) ? 52 : ww;
    return validateyw(ny, nw);
}


BINOP_IMPL(inc_w_by_y)


// days
// ==================================================================

#ifdef validate_yj_
#undef validate_yj_
#endif /* validate_yj_ */
#define validate_yj_ validateyj
BINOP_IMPL(validate_yj)
#undef validate_yj_


static inline
TIND__ATTRIBUTE_INLINE
int
inc_d_by_m_(int d, int by)
{
    int yy, mm, dd, nym, dim, ny, nm, nd;
    d2ymd(d, &yy, &mm, &dd);
    nym = 12 * yy + mm - 1 + by;
    ny = nym / 12;
    if (!validy(ny)) return NA_INTEGER;
    nm = nym % 12 + 1;
    dim = daysinmonth(ny, nm);
    nd = (dd > dim) ? dim : dd;
    return ymd2d(ny, nm, nd);
}


BINOP_IMPL(inc_d_by_m)


static inline
TIND__ATTRIBUTE_INLINE
int
lastdwinmonth_(int dw, int m)
{
    if (dw < 1 || dw > 7) return NA_INTEGER;
    int yy, mm, dd, dw0;
    yy = m / 12;
    mm = m % 12 + 1;
    dd = ymd2d(yy, mm, daysinmonth(yy, mm));
    dw0 = dayofweek(dd);
    return (dw0 >= dw) ? dd - dw0 + dw : dd - dw0 + dw - 7;
}

BINOP_IMPL(lastdwinmonth)


#undef BINOP_IMPL


// ==================================================================
// 3-argument operations
// ==================================================================

#define THREEARGOP_IMPL(NAME) \
TIND__ATTRIBUTE_FUNCTION \
SEXP \
NAME(SEXP sx, SEXP sy, SEXP sz) \
{ \
    size_t i, j, k, nx, ny, nz, nr; \
    int *x, *y, *z, *r; \
    SEXP sr; \
 \
    nx = XLENGTH(sx); \
    ny = XLENGTH(sy); \
    nz = XLENGTH(sz); \
 \
    nr = (nx >= ny) ? ((nx >= nz) ? nx : nz) : ((ny >= nz) ? ny : nz); \
    sr = PROTECT(allocVector(INTSXP, nr)); \
    x = INTEGER(sx); \
    y = INTEGER(sy); \
    z = INTEGER(sz); \
    r = INTEGER(sr); \
 \
    if (nx == ny && ny == nz) { \
        for (i = 0; i < nr; ++i) { \
            if (x[i] == NA_INTEGER || y[i] == NA_INTEGER || z[i] == NA_INTEGER) { \
                r[i] = NA_INTEGER; continue; \
            } \
            r[i] = NAME##_(x[i], y[i], z[i]); \
        } \
    } else if (ny == 1 && nz == 1) { \
        if (*y == NA_INTEGER || *z == NA_INTEGER) { \
            for (i = 0; i < nr; ++i) r[i] = NA_INTEGER; \
        } else for (i = 0; i < nr; ++i) { \
            if (x[i] == NA_INTEGER) { r[i] = NA_INTEGER; continue; } \
            r[i] = NAME##_(x[i], *y, *z); \
        } \
    } else if (nx == 1 && nz == 1) { \
        if (*x == NA_INTEGER || *z == NA_INTEGER) { \
            for (i = 0; i < nr; ++i) r[i] = NA_INTEGER; \
        } else for (i = 0; i < nr; ++i) { \
            if (y[i] == NA_INTEGER) { r[i] = NA_INTEGER; continue; } \
            r[i] = NAME##_(*x, y[i], *z); \
        } \
    } else if (nx == 1 && ny == 1) { \
        if (*x == NA_INTEGER || *y == NA_INTEGER) { \
            for (i = 0; i < nr; ++i) r[i] = NA_INTEGER; \
        } else for (i = 0; i < nr; ++i) { \
            if (z[i] == NA_INTEGER) { r[i] = NA_INTEGER; continue; } \
            r[i] = NAME##_(*x, *y, z[i]); \
        } \
    } else if (nx == nr) { \
        for (i = 0; i < nr; ++i) { \
            j = i % ny; \
            k = i % nz; \
            if (x[i] == NA_INTEGER || y[j] == NA_INTEGER || z[k] == NA_INTEGER) { \
                r[i] = NA_INTEGER; continue; \
            } \
            r[i] = NAME##_(x[i], y[j], z[k]); \
        } \
    } else if (ny == nr) { \
        for (j = 0; j < nr; ++j) { \
            i = j % nx; \
            k = j % nz; \
            if (x[i] == NA_INTEGER || y[j] == NA_INTEGER || z[k] == NA_INTEGER) { \
                r[j] = NA_INTEGER; continue; \
            } \
            r[j] = NAME##_(x[i], y[j], z[k]); \
        } \
    } else { \
        for (k = 0; k < nr; ++k) { \
            i = k % nx; \
            j = k % ny; \
            if (x[i] == NA_INTEGER || y[j] == NA_INTEGER || z[k] == NA_INTEGER) { \
                r[k] = NA_INTEGER; continue; \
            } \
            r[k] = NAME##_(x[i], y[j], z[k]); \
        } \
    } \
 \
    if (nx == nr) { \
        SEXP nmsx = getAttrib(sx, R_NamesSymbol); \
        if ((nr > ny) && (nr > nz)) { \
            setAttrib(sr, R_NamesSymbol, nmsx); \
        } else if ((nr == ny) && (nr > nz)) { \
            if (!isNull(nmsx)) setAttrib(sr, R_NamesSymbol, nmsx); \
            else setAttrib(sr, R_NamesSymbol, getAttrib(sy, R_NamesSymbol)); \
        } else if ((nr > ny) && (nr == nz)) { \
            if (!isNull(nmsx)) setAttrib(sr, R_NamesSymbol, nmsx); \
            else setAttrib(sr, R_NamesSymbol, getAttrib(sz, R_NamesSymbol)); \
        } else { \
            if (!isNull(nmsx)) setAttrib(sr, R_NamesSymbol, nmsx); \
            else { \
                SEXP nmsy = getAttrib(sy, R_NamesSymbol); \
                if (!isNull(nmsy)) setAttrib(sr, R_NamesSymbol, nmsy); \
                else setAttrib(sr, R_NamesSymbol, getAttrib(sz, R_NamesSymbol)); \
            } \
        } \
    } else if (ny == nr) { \
        SEXP nmsy = getAttrib(sy, R_NamesSymbol); \
        if (nr > nz) { \
            setAttrib(sr, R_NamesSymbol, nmsy); \
        } else { \
            if (!isNull(nmsy)) setAttrib(sr, R_NamesSymbol, nmsy); \
            else setAttrib(sr, R_NamesSymbol, getAttrib(sz, R_NamesSymbol)); \
        } \
    } else { \
        setAttrib(sr, R_NamesSymbol, getAttrib(sz, R_NamesSymbol)); \
    } \
    UNPROTECT(1); \
    return sr; \
}



#ifdef validate_ymd_
#undef validate_ymd_
#endif /* validate_ymd_ */
#define validate_ymd_ validateymd
THREEARGOP_IMPL(validate_ymd)
#undef validate_ymd_



#ifdef validate_ywu_
#undef validate_ywu_
#endif /* validate_ywu_ */
#define validate_ywu_ validateywu
THREEARGOP_IMPL(validate_ywu)
#undef validate_ywu_



static inline
TIND__ATTRIBUTE_INLINE
int
nthdwinmonth_(int nth, int dw, int m)
{
    if (nth < 1 || nth > 5 || dw < 1 || dw > 7) return NA_INTEGER;
    int yy, mm, dd0, dd, dw0;
    yy = m / 12;
    mm = m % 12 + 1;
    dd0 = ymd2d(yy, mm, 1);
    dw0 = dayofweek(dd0);
    dd = (dw >= dw0) ? dw - dw0 : dw - dw0 + 7;
    dd += 7 * (nth - 1);
    if (nth == 5 && (dd + 1 > daysinmonth(yy, mm))) return NA_INTEGER;
    return dd0 + dd;
}

THREEARGOP_IMPL(nthdwinmonth)


#undef THREEARGOP_IMPL

