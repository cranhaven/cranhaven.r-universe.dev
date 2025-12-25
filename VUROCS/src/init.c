#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _VUROCS_funC(SEXP);
extern SEXP _VUROCS_funC1(SEXP);
extern SEXP _VUROCS_funC2(SEXP);
extern SEXP _VUROCS_funD(SEXP);
extern SEXP _VUROCS_funD1(SEXP);
extern SEXP _VUROCS_funD2(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_VUROCS_funC",                  (DL_FUNC) &_VUROCS_funC,                  1},
    {"_VUROCS_funC1",                 (DL_FUNC) &_VUROCS_funC1,                 1},
    {"_VUROCS_funC2",                 (DL_FUNC) &_VUROCS_funC2,                 1},
    {"_VUROCS_funD",                  (DL_FUNC) &_VUROCS_funD,                  1},
    {"_VUROCS_funD1",                 (DL_FUNC) &_VUROCS_funD1,                 1},
    {"_VUROCS_funD2",                 (DL_FUNC) &_VUROCS_funD2,                 1},
    {NULL, NULL, 0}
};

void R_init_VUROCS(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
