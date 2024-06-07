#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <Rdefines.h>

/* .Call calls */
extern SEXP EMGLLF(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP EMGrank(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CEntries[] = {
  { "EMGLLF",  (DL_FUNC) &EMGLLF,  11 },
  { "EMGrank", (DL_FUNC) &EMGrank, 8  },
  { NULL,      NULL,               0  }
};

void R_init_valse(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
