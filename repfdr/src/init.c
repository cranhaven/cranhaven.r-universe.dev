#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
  The following symbols/expressions for .NAME have been omitted

    _repfdr_rcpp_main

  Most likely possible values need to be added below.
*/

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP REM(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _repfdr_rcpp_main(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"REM", (DL_FUNC) &REM, 8},
    {"_repfdr_rcpp_main", (DL_FUNC) &_repfdr_rcpp_main, 7},
    {NULL, NULL, 0}
};

void R_init_repfdr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
