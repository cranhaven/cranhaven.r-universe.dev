#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP BigMatrixTAPPLY(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP RIntTAPPLY(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP RNumericTAPPLY(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"BigMatrixTAPPLY", (DL_FUNC) &BigMatrixTAPPLY, 10},
    {"RIntTAPPLY",      (DL_FUNC) &RIntTAPPLY,      10},
    {"RNumericTAPPLY",  (DL_FUNC) &RNumericTAPPLY,  10},
    {NULL, NULL, 0}
};

void R_init_bigtabulate(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
