#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP parseMiniSEED(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"parseMiniSEED", (DL_FUNC) &parseMiniSEED, 1},
    {NULL, NULL, 0}
};

void R_init_IRISSeismic(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

