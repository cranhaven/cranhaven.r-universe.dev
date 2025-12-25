#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP StepSignalMargiLike_ChangePointAnalyzeNorm(SEXP, SEXP, SEXP, SEXP);
extern SEXP StepSignalMargiLike_ChangePointAnalyzeNormUnRes(SEXP, SEXP, SEXP);
extern SEXP StepSignalMargiLike_ChangePointAnalyzePoiss(SEXP, SEXP, SEXP, SEXP);
extern SEXP StepSignalMargiLike_ChangePointAnalyzePoissUnRes(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"StepSignalMargiLike_ChangePointAnalyzeNorm",       (DL_FUNC) &StepSignalMargiLike_ChangePointAnalyzeNorm,       4},
  {"StepSignalMargiLike_ChangePointAnalyzeNormUnRes",  (DL_FUNC) &StepSignalMargiLike_ChangePointAnalyzeNormUnRes,  3},
  {"StepSignalMargiLike_ChangePointAnalyzePoiss",      (DL_FUNC) &StepSignalMargiLike_ChangePointAnalyzePoiss,      4},
  {"StepSignalMargiLike_ChangePointAnalyzePoissUnRes", (DL_FUNC) &StepSignalMargiLike_ChangePointAnalyzePoissUnRes, 3},
  {NULL, NULL, 0}
};

void R_init_StepSignalMargiLike(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
