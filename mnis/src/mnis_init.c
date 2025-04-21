#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _mnis_mnis_bom(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_mnis_mnis_bom", (DL_FUNC) &_mnis_mnis_bom, 1},
  {NULL, NULL, 0}
};

void R_init_mnis(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
