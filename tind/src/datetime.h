/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 *************
 * date-time *
 *************
 */


#include "compiler.h"
#include <Rinternals.h>
#include "compiler.h"


#ifndef DATETIME_H

#define DATETIME_H


// declarations
// ==================================================================

SEXP validate_t(SEXP);
SEXP validate_h(SEXP);
SEXP validate_hms(SEXP, SEXP, SEXP);
SEXP plt_ymd2d(SEXP, SEXP, SEXP, SEXP);
SEXP plt_hms2h(SEXP, SEXP, SEXP, SEXP);
SEXP plt_midnightdiff(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP dh2t_aux(SEXP, SEXP, SEXP);
SEXP h2hour(SEXP);
SEXP h2min(SEXP);
SEXP h2sec(SEXP);
SEXP floor_h(SEXP, SEXP);
SEXP ceiling_h(SEXP, SEXP);


// defines - valid ranges
// ==================================================================

#define VALID_T_MIN -62167165200. // 0000-01-01 15:00:00 UTC
#define VALID_T_MAX 253402246800. // 9999-12-31 09:00:00 UTC


// inlines
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
double
round6(double x)
{
    double fl = floor(x);
    double rem = x - fl;
    return fl + round(rem * 1e6) * 1e-6;
}


inline
TIND__ATTRIBUTE_INLINE
double
validhms(int h, int m, double s)
{
    if (h < 0 || h > 24 || m < 0 || m >= 60) return 0;
    if (h == 24 && (m || s != 0.)) return 0;
    // NOTE: as in POSIX, leap seconds are ignored
    return (s >= 0. && s < 60);
}


inline
TIND__ATTRIBUTE_INLINE
double
validatehms(int h, int m, double s)
{
    return validhms(h, m, s) ? (3600 * h + 60 * m) + round6(s) : NA_REAL;
}


// 12 hour clock and AM/PM indicator (I, p -> H)
// p == 1 - AM, p == 2 PM
inline
TIND__ATTRIBUTE_INLINE
int
validateip(int I, int p)
{
    if (I < 1 || I > 12) return NA_INTEGER;
    if (I == 12) return (p == 2) ? 12 : 0;
    return (p == 2) ? I + 12 : I;
}


// UTC offset +-HH
inline
TIND__ATTRIBUTE_INLINE
int
validatezh(int z)
{
    if ((z < -15) || (z > 15)) return NA_INTEGER;
    return z * 3600;
}


// UTC offset +-HHMM
inline
TIND__ATTRIBUTE_INLINE
int
validatezhm(int z)
{
    int sgn = 1, h, m;
    if (z < 0) { sgn = -1; z = -z; }
    h = z / 100; m = z % 100;
    if ((m >= 60) || (h > 15) || ((h == 15) && (m > 0))) return NA_INTEGER;
    return sgn * (h * 3600 + m * 60);
}



#endif /* DATETIME_H */

