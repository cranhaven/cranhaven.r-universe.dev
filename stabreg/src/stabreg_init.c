#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>



extern SEXP R_stable_pdf_fourier_integral(SEXP, SEXP, SEXP);
extern SEXP R_stable_pdf_series_infinity(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_stable_sym_pdf_fourier_integral(SEXP, SEXP);
extern SEXP R_stable_sym_pdf(SEXP, SEXP);
extern SEXP R_stable_pdf_iter_singleobs(SEXP, SEXP, SEXP);
extern SEXP R_stable_pdf_singleobs(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"R_stable_pdf_fourier_integral",     (DL_FUNC) &R_stable_pdf_fourier_integral,      3},
    {"R_stable_pdf_series_infinity",      (DL_FUNC) &R_stable_pdf_series_infinity,       4},
    {"R_stable_sym_pdf_fourier_integral", (DL_FUNC) &R_stable_sym_pdf_fourier_integral,  2},
    {"R_stable_sym_pdf",                  (DL_FUNC) &R_stable_sym_pdf,                   2},
    {"R_stable_pdf_iter_singleobs",       (DL_FUNC) &R_stable_pdf_iter_singleobs,        3},
    {"R_stable_pdf_singleobs",            (DL_FUNC) &R_stable_pdf_singleobs,             3},
    {NULL, NULL, 0}
};

void R_init_stabreg(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

