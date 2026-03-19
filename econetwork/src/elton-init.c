#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _econetwork_elgrincore(SEXP, SEXP, SEXP, SEXP);
extern SEXP _econetwork_elgrinsimcore(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_econetwork_elgrincore", (DL_FUNC) &_econetwork_elgrincore, 4},
    {"_econetwork_elgrinsimcore", (DL_FUNC) &_econetwork_elgrinsimcore, 10},
    {NULL, NULL, 0}
};

void R_init_econetwork(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

