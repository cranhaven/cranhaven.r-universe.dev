#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _qris_Amat(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _qris_ghatC(SEXP, SEXP, SEXP);
extern SEXP _qris_isObj(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _qris_isObjE(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _qris_isObjL(SEXP, SEXP, SEXP, SEXP);
extern SEXP _qris_rev_isObj(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_qris_Amat",      (DL_FUNC) &_qris_Amat,      7},
    {"_qris_ghatC",     (DL_FUNC) &_qris_ghatC,     3},
    {"_qris_isObj",     (DL_FUNC) &_qris_isObj,     7},
    {"_qris_isObjE",    (DL_FUNC) &_qris_isObjE,    9},
    {"_qris_isObjL",    (DL_FUNC) &_qris_isObjL,    4},
    {"_qris_rev_isObj", (DL_FUNC) &_qris_rev_isObj, 8},
    {NULL, NULL, 0}
};

void R_init_qris(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
