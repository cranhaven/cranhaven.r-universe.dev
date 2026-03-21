#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP _HiCociety_calculate_avg_count(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_HiCociety_calculate_avg_count", (DL_FUNC) &_HiCociety_calculate_avg_count, 4},
  {NULL, NULL, 0}
};

void R_init_HiCociety(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
