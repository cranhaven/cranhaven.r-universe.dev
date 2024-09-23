#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP SSL_gaussian(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP standardize(SEXP);


static const R_CallMethodDef CallEntries[] = {
    {"SSL_gaussian", (DL_FUNC) &SSL_gaussian, 14},
    {"standardize",  (DL_FUNC) &standardize,   1},
    {NULL, NULL, 0}
};

void R_init_SSLASSO(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
