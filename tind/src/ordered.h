/*
 * This file is a part of ats.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ***********************************************
 * optimised routines for ordered time indices *
 ***********************************************
 */


#include <R.h>
#include <Rinternals.h>


#ifndef ORDERED_H

#define ORDERED_H


// declarations
// ==================================================================

SEXP fi2match(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP is_ordered(SEXP, SEXP);
SEXP is_regular_t(SEXP, SEXP);
SEXP is_regular_th(SEXP, SEXP);
SEXP unique_ord(SEXP);
SEXP intersect_ord(SEXP, SEXP);
SEXP union_ord(SEXP, SEXP);
SEXP setdiff_ord(SEXP, SEXP);
SEXP match_ord(SEXP, SEXP, SEXP);
SEXP merge_in_ord(SEXP, SEXP);
SEXP merge_out_ord(SEXP, SEXP);


#endif /* ORDERED_H */

