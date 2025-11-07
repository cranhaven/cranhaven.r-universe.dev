/*
 * This file is a part of tind.
 *
 * Copyright (c) Grzegorz Klima 2025
 *
 *****************************
 * parsing character strings *
 *****************************
 */

#include <R.h>
#include <Rinternals.h>


#ifndef PARSE_H

#define PARSE_H


SEXP parse(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP strptind(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP parse_tdiff_yqmwd(SEXP);
SEXP parse_tdiff_t(SEXP);
SEXP parse_tinterval(SEXP, SEXP);


#endif /* PARSE_H */

