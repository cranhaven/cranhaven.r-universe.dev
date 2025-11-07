/*
 * This file is a part of ats.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 ********************************************************************
 * conversion to/from YYYYMM, YYYYMMDD, etc. numeric representation *
 ********************************************************************
 */


#include <R.h>
#include <Rinternals.h>


#ifndef NUM_H

#define NUM_H


SEXP autoparse_num(SEXP);
SEXP num2q(SEXP);
SEXP num2m(SEXP);
SEXP num2w(SEXP);
SEXP num2d(SEXP);
SEXP q2num(SEXP);
SEXP m2num(SEXP);
SEXP w2num(SEXP);
SEXP d2num(SEXP);


#endif /* NUM_H */

