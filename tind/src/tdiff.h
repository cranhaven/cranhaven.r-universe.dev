/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 **********************
 * tdiff - validation *
 **********************
 */

#include <R.h>
#include <Rinternals.h>
#include "compiler.h"
#include "calendar.h"
#include "datetime.h"


#ifndef TDIFF_H

#define TDIFF_H


// declarations
// ==================================================================

SEXP validate_tdiff_y(SEXP);
SEXP validate_tdiff_q(SEXP);
SEXP validate_tdiff_m(SEXP);
SEXP validate_tdiff_w(SEXP);
SEXP validate_tdiff_d(SEXP);
SEXP validate_tdiff_t(SEXP);


// defines - valid ranges
// ==================================================================

#define VALID_TD_Y_MIN (VALID_Y_MIN - VALID_Y_MAX)
#define VALID_TD_Y_MAX (VALID_Y_MAX - VALID_Y_MIN + 1)
#define VALID_TD_Q_MIN (VALID_Q_MIN - VALID_Q_MAX)
#define VALID_TD_Q_MAX (VALID_Q_MAX - VALID_Q_MIN + 1)
#define VALID_TD_M_MIN (VALID_M_MIN - VALID_M_MAX)
#define VALID_TD_M_MAX (VALID_M_MAX - VALID_M_MIN + 1)
#define VALID_TD_W_MIN (VALID_W_MIN - VALID_W_MAX)
#define VALID_TD_W_MAX (VALID_W_MAX - VALID_W_MIN + 1)
#define VALID_TD_D_MIN (VALID_D_MIN - VALID_D_MAX)
#define VALID_TD_D_MAX (VALID_D_MAX - VALID_D_MIN + 1)
#define VALID_TD_T_ABS (VALID_T_MAX - VALID_T_MIN)


// inlines
// ==================================================================

inline
TIND__ATTRIBUTE_INLINE
int
valid_tdiff_y(int x)
{
    return VALID_TD_Y_MIN <= x && x <= VALID_TD_Y_MAX;
}


inline
TIND__ATTRIBUTE_INLINE
int
valid_tdiff_q(int x)
{
    return VALID_TD_Q_MIN <= x && x <= VALID_TD_Q_MAX;
}


inline
TIND__ATTRIBUTE_INLINE
int
valid_tdiff_m(int x)
{
    return VALID_TD_M_MIN <= x && x <= VALID_TD_M_MAX;
}


inline
TIND__ATTRIBUTE_INLINE
int
valid_tdiff_w(int x)
{
    return VALID_TD_W_MIN <= x && x <= VALID_TD_W_MAX;
}


inline
TIND__ATTRIBUTE_INLINE
int
valid_tdiff_d(int x)
{
    return VALID_TD_D_MIN <= x && x <= VALID_TD_D_MAX;
}


inline
TIND__ATTRIBUTE_INLINE
int
valid_tdiff_t(double x)
{
    return (R_FINITE(x)) && (fabs(x) <= VALID_TD_T_ABS);
}


#endif /* TDIFF_H */

