/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ***********************************
 * conversion to character strings *
 ***********************************
 */


#include <R.h>
#include <Rinternals.h>


#ifndef TOCHAR_H

#define TOCHAR_H


// declarations
// ==================================================================

SEXP yqmwd2char(SEXP, SEXP);
SEXP t2char(SEXP, SEXP, SEXP, SEXP);
SEXP h2char(SEXP);
SEXP format(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP tdiff_yqmwd2char(SEXP, SEXP);
SEXP tdiff_t2char(SEXP);
SEXP aux_tdiff(SEXP, SEXP);
SEXP format_tdiff(SEXP, SEXP);
SEXP tinterval2char(SEXP);
SEXP format_tinterval(SEXP, SEXP, SEXP, SEXP);


#endif /* TOCHAR_H */

