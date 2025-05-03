#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP do_asPOSIXct_360(SEXP);
extern SEXP do_asPOSIXlt_360(SEXP);
extern SEXP do_formatPOSIXlt_360(SEXP, SEXP);
extern SEXP do_strptime_360(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"do_asPOSIXct_360",     (DL_FUNC) &do_asPOSIXct_360,     1},
    {"do_asPOSIXlt_360",     (DL_FUNC) &do_asPOSIXlt_360,     1},
    {"do_formatPOSIXlt_360", (DL_FUNC) &do_formatPOSIXlt_360, 2},
    {"do_strptime_360",      (DL_FUNC) &do_strptime_360,      2},
    {NULL, NULL, 0}
};

void R_init_PCICt(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
