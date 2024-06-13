#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> 
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP singcheckQR(SEXP);
extern SEXP updateQR(SEXP, SEXP, SEXP, SEXP, SEXP);

/* .Fortran calls */
extern void F77_NAME(regcf)(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CallMethodDef CallEntries[] = {
    {"singcheckQR", (DL_FUNC) &singcheckQR, 1},
    {"updateQR",    (DL_FUNC) &updateQR,    5},
    {NULL, NULL, 0}
};

static const R_FortranMethodDef FortranEntries[] = {
    {"regcf", (DL_FUNC) &F77_NAME(regcf), 9},
    {NULL, NULL, 0}
};

void R_init_biglmm(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
