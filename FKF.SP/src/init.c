#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP fkf_SP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fkf_SP_verbose(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fks_SP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fkfs_SP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"fkf_SP_verbose", (DL_FUNC)&fkf_SP_verbose, 9},
    {"fkf_SP", (DL_FUNC)&fkf_SP, 9},
    {"fkfs_SP", (DL_FUNC)&fkfs_SP, 9},
    {"fks_SP", (DL_FUNC)&fks_SP, 8},
    {NULL, NULL, 0}};

void R_init_fkf_SP(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
