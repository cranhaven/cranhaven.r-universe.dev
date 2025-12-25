#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _cmenet_cme(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _cmenet_mcp(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_cmenet_cme", (DL_FUNC) &_cmenet_cme, 16},
  {"_cmenet_mcp", (DL_FUNC) &_cmenet_mcp,  3},
  {NULL, NULL, 0}
};

void R_init_cmenet(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
