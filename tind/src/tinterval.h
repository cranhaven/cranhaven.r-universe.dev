/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ********************************
 * tinterval - intersect, match *
 ********************************
 */


#include <R.h>
#include <Rinternals.h>


#ifndef TINTERVAL_H

#define TINTERVAL_H


// declarations
// ==================================================================

SEXP intersect_tint(SEXP, SEXP, SEXP, SEXP);
SEXP match_tint(SEXP, SEXP, SEXP, SEXP);
SEXP in_tint_ord(SEXP, SEXP, SEXP);


#endif /* TINTERVAL_H */

