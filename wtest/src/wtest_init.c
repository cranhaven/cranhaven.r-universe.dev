#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

extern SEXP table_e1(SEXP, SEXP);
extern SEXP table_e2(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
	{"table_e1", (DL_FUNC) &table_e1, 2},
	{"table_e2", (DL_FUNC) &table_e2, 3},
	{NULL, NULL, 0}
};

void R_init_wtest(DllInfo *dll)
{
	R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(dll,FALSE);
}
