/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ***********************************************************************
 * calendrical computations (years, quarters, months, weeks, and days) *
 ***********************************************************************
 */


#include <R.h>
#include <Rinternals.h>
#include "compiler.h"


#ifndef CALENDAR_H

#define CALENDAR_H


// declarations
// ==================================================================

SEXP validate_y(SEXP);
SEXP is_leap_year(SEXP);
SEXP validate_q(SEXP);
SEXP q2y(SEXP);
SEXP y2q(SEXP);
SEXP q2qrtr(SEXP);
SEXP validate_m(SEXP);
SEXP m2y(SEXP);
SEXP y2m(SEXP);
SEXP m2q(SEXP);
SEXP q2m(SEXP);
SEXP m2mnth(SEXP);
SEXP weeks_in_year(SEXP);
SEXP validate_w(SEXP);
SEXP w2week(SEXP);
SEXP w2y(SEXP);
SEXP y2w(SEXP);
SEXP days_in_quarter(SEXP);
SEXP days_in_month(SEXP);
SEXP validate_d(SEXP);
SEXP day_of_year(SEXP);
SEXP day_of_week(SEXP);
SEXP d2day(SEXP);
SEXP d2y(SEXP);
SEXP y2d(SEXP);
SEXP d2q(SEXP);
SEXP q2d(SEXP);
SEXP d2m(SEXP);
SEXP m2d(SEXP);
SEXP last_day_in_month(SEXP);
SEXP w2d(SEXP);
SEXP d2w(SEXP);
SEXP d2jdn(SEXP);
SEXP jdn2d(SEXP);
SEXP easter(SEXP);
SEXP yqm2yf(SEXP, SEXP);
SEXP w2yf(SEXP);
SEXP d2yf(SEXP);
SEXP floor_yqm(SEXP, SEXP);
SEXP ceiling_y(SEXP, SEXP);
SEXP ceiling_q(SEXP, SEXP);
SEXP ceiling_m(SEXP, SEXP);
SEXP floor_w(SEXP, SEXP);
SEXP ceiling_w(SEXP, SEXP);
SEXP floor_d(SEXP, SEXP);
SEXP ceiling_d(SEXP, SEXP);
SEXP validate_yq(SEXP, SEXP);
SEXP validate_ym(SEXP, SEXP);
SEXP validate_yw(SEXP, SEXP);
SEXP inc_w_by_y(SEXP, SEXP);
SEXP validate_yj(SEXP, SEXP);
SEXP inc_d_by_m(SEXP, SEXP);
SEXP lastdwinmonth(SEXP, SEXP);
SEXP validate_ymd(SEXP, SEXP, SEXP);
SEXP validate_ywu(SEXP, SEXP, SEXP);
SEXP nthdwinmonth(SEXP, SEXP, SEXP);



// defines - valid ranges
// ==================================================================

#define VALID_Y_MIN 0
#define VALID_Y_MAX 9999
#define VALID_Q_MIN 0
#define VALID_Q_MAX 39999
#define VALID_M_MIN 0
#define VALID_M_MAX 119999
#define VALID_W_MIN -102789
#define VALID_W_MAX 418985
#define VALID_D_MIN -719528
#define VALID_D_MAX 2932896



// inlines - floored mod and div
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
int
div_(int a, int b)
{
    int q = a / b;
    int r = a % b;
    return (r >= 0) ? q : --q;
}


inline
TIND__ATTRIBUTE_INLINE
int
mod_(int a, int b)
{
    int r = a % b;
    return (r >= 0) ? r : r + b;
}


// inlines - years
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
int
validy(int y)
{
    return (y >= VALID_Y_MIN) && (y <= VALID_Y_MAX);
}


inline
TIND__ATTRIBUTE_INLINE
int
isleapyear(int y)
{
    return (!(y % 4) && ((y % 100) || !(y % 400))) ? 1 : 0;
}


// inlines - quarters
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
int
validyq(int y, int q)
{
    return validy(y) && (q > 0) && (q <= 4);
}


inline
TIND__ATTRIBUTE_INLINE
int
validateyq(int y, int q)
{
    return validyq(y, q) ? 4 * y + q - 1: NA_INTEGER;
}


// inlines - months
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
int
validym(int y, int m)
{
    return validy(y) && (m > 0) && (m <= 12);
}


inline
TIND__ATTRIBUTE_INLINE
int
validateym(int y, int m)
{
    return validym(y, m) ? 12 * y + m - 1 : NA_INTEGER;
}


// inlines - weeks
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
int
weeksinyear(int y)
{
    if (!y) return 52;
    int dw;
    --y;
    dw = 1 + (y + y / 4 - y / 100 + y / 400) % 7;
    ++y;
    return ((dw == 4) || (isleapyear(y) && (dw == 3))) ? 53 : 52;
}


inline
TIND__ATTRIBUTE_INLINE
int
validateyw(int y, int w)
{
    if ((y < VALID_Y_MIN) || (y > VALID_Y_MAX) || (w <= 0)) return NA_INTEGER;
    int dw, winy;
    if (y) {
        --y;
        dw = 1 + (y + y / 4 - y / 100 + y / 400) % 7;
        ++y;
        winy = ((dw == 4) || (isleapyear(y) && (dw == 3))) ? 53 : 52;
    } else {
        dw = 6;
        winy = 52;
    }
    if (w > winy) return NA_INTEGER;
    y += 4799;
    int dy = y * 365 + y / 4 - y / 100 + y / 400 - 2472322 - dw;
    return (dw <= 4) ? dy / 7 + w - 1 : dy / 7 + w;
}


inline
TIND__ATTRIBUTE_INLINE
void
w2yw(int x, int *y, int *w)
{
    int a, b, c, e, d, m, yy;
    a = 7 * x + 2472629;
    b = (4 * a + 3) / 146097;
    c = a - (b * 146097) / 4;
    d = (4 * c + 3) / 1461;
    e = c - (1461 * d) / 4;
    m = (5 * e + 2) / 153;
    yy = b * 100 + d - 4800 + m / 10;
    d = e - (153 * m + 2) / 5 + 1;
    m += 3 - 12 * (m / 10);
    if ((m == 12) && (d > 28)) ++yy;
    *y = yy;
    int dw;
    if (yy) {
        --yy;
        dw = 1 + (yy + yy / 4 - yy / 100 + yy / 400) % 7;
        ++yy;
    } else {
        dw = 6;
    }
    yy += 4799;
    int dy = yy * 365 + yy / 4 - yy / 100 + yy / 400 - 2472322 - dw;
    int wy = (dw <= 4) ? dy / 7 - 1 : dy / 7;
    *w = x - wy;
}


// inlines - days
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
int
daysinmonth(int y, int m)
{
    if (m == 2) return 28 + isleapyear(y);
    if ((m == 4) || (m == 6) || (m == 9) || (m == 11)) return 30;
    return 31;
}


inline
TIND__ATTRIBUTE_INLINE
int
validymd(int y, int m, int d)
{
    return (y >= 0) && (y <= 9999) && (m > 0) && (m <= 12) && (d > 0) &&
           (d <= daysinmonth(y, m));
}


inline
TIND__ATTRIBUTE_INLINE
int
ymd2d(int y, int m, int d)
{
    int a = (14 - m) / 12;
    y += 4800 - a;
    m += 12 * a - 3;
    return d + (153 * m + 2) / 5 + y * 365 + y / 4 - y / 100 + y / 400 - 2472633;
}


inline
TIND__ATTRIBUTE_INLINE
int
validateymd(int y, int m, int d)
{
    return validymd(y, m, d) ? ymd2d(y, m, d) : NA_INTEGER;
}


inline
TIND__ATTRIBUTE_INLINE
void
d2ymd(int x, int *y, int *m, int *d)
{
    int a, b, c, e;
    a = x + 2472632;
    b = (4 * a + 3) / 146097;
    c = a - (b * 146097) / 4;
    *d = (4 * c + 3) / 1461;
    e = c - (1461 * *d) / 4;
    *m = (5 * e + 2) / 153;
    *y = b * 100 + *d - 4800 + *m / 10;
    *d = e - (153 * *m + 2) / 5 + 1;
    *m += 3 - 12 * (*m / 10);
}


inline
TIND__ATTRIBUTE_INLINE
int
dayofyear(int x)
{
    int a, b, c, e, y, m, d;
    a = x + 2472632;
    b = (4 * a + 3) / 146097;
    c = a - (b * 146097) / 4;
    d = (4 * c + 3) / 1461;
    e = c - (1461 * d) / 4;
    m = (5 * e + 2) / 153;
    y = b * 100 + d - 1 + m / 10;
    y = y * 365 + y / 4 - y / 100 + y / 400;
    return 2472327 + x - y;
}


inline
TIND__ATTRIBUTE_INLINE
int
dayofweek(int x)
{
    return mod_(x + 3, 7) + 1;
}


inline
TIND__ATTRIBUTE_INLINE
int
nthdayofyear(int n, int y)
{
    y += 4799;
    return n + y * 365 + y / 4 - y / 100 + y / 400 - 2472327;
}


inline
TIND__ATTRIBUTE_INLINE
int
validyj(int y, int j)
{
    return ((y >= 0) && (y <= 9999) && (j > 0) && (j <= 365 + isleapyear(y)));
}


inline
TIND__ATTRIBUTE_INLINE
int
validateyj(int y, int j)
{
    return validyj(y, j) ? nthdayofyear(j, y) : NA_INTEGER;
}


inline
TIND__ATTRIBUTE_INLINE
int
validateywu(int y, int w, int u)
{
    if ((u < 1) || (u > 7)) return NA_INTEGER;
    int ww = validateyw(y, w);
    if (ww == NA_INTEGER) return NA_INTEGER;
    int dd = 7 * ww + u - 4;
    return (dd <= VALID_D_MAX) ? dd : NA_INTEGER;
}


#endif /* CALENDAR_H */

