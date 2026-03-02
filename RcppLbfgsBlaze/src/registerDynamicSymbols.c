// Copyright (C)  2024  Ching-Chuan Chen
//
// This file is part of RcppLbfgsBlaze.
//
// RcppLbfgsBlaze is free software: you can redistribute it and/or modify it
// under the terms of the MIT License. You should have received
// a copy of MIT License along with RcppLbfgsBlaze.
// If not, see https://opensource.org/license/mit.

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _RcppLbfgsBlaze_fastLogisticModel(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_RcppLbfgsBlaze_fastLogisticModel", (DL_FUNC) &_RcppLbfgsBlaze_fastLogisticModel, 2},
  {NULL, NULL, 0}
};

void R_init_RcppLbfgsBlaze(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
