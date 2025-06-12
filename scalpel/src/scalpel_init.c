#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP ccl(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"ccl", (DL_FUNC) &ccl, 1},
    {NULL, NULL, 0}
};

void R_init_scalpel(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
