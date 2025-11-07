/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ******************************
 * resolution of time indices *
 ******************************
 */

#include <R.h>
#include <Rinternals.h>


#ifndef RESOLUTION_H

#define RESOLUTION_H


// declarations
// ==================================================================


SEXP res_y(SEXP);
SEXP res_q(SEXP);
SEXP res_m(SEXP);
SEXP res_w(SEXP);
SEXP res_d(SEXP);
SEXP res_s(SEXP);
SEXP res_subs(SEXP);
SEXP res_min(SEXP);
SEXP res_h(SEXP);
SEXP tunit(SEXP);
SEXP res_i(SEXP);


#endif /* RESOLUTION_H */

