#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _scModels_chf_1F1(SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_dpb(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_ppb(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_qpb(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_rpb(SEXP, SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_gmRNA_switch(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_gmRNA_burst(SEXP, SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_gmRNA_basic(SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_gmRNA_basic_burst(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _scModels_cpp_rInvGaus(SEXP, SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
  {"_scModels_chf_1F1", (DL_FUNC) &_scModels_chf_1F1, 3},
  {"_scModels_cpp_dpb",    (DL_FUNC) &_scModels_cpp_dpb,    5},
  {"_scModels_cpp_ppb",    (DL_FUNC) &_scModels_cpp_ppb,    6},
  {"_scModels_cpp_qpb",    (DL_FUNC) &_scModels_cpp_qpb,    6},
  {"_scModels_cpp_rpb",    (DL_FUNC) &_scModels_cpp_rpb,    4},
  {"_scModels_cpp_gmRNA_switch",       (DL_FUNC) &_scModels_cpp_gmRNA_switch,       5},
  {"_scModels_cpp_gmRNA_burst",       (DL_FUNC) &_scModels_cpp_gmRNA_burst,       4},
  {"_scModels_cpp_gmRNA_basic",       (DL_FUNC) &_scModels_cpp_gmRNA_basic,       3},
  {"_scModels_cpp_gmRNA_basic_burst",       (DL_FUNC) &_scModels_cpp_gmRNA_basic_burst,       5},
  {"_scModels_cpp_rInvGaus",    (DL_FUNC) &_scModels_cpp_rInvGaus,    3},
  {NULL, NULL, 0}
};

void R_init_scModels(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
