#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP concise(SEXP);
extern SEXP gillespie(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP lstrtod(SEXP);
extern SEXP r_gsl_odeiv2_outer_CRNN(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP r_gsl_odeiv2_outer_fi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP replace_pow(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"concise",                 (DL_FUNC) &concise,                  1},
    {"gillespie",               (DL_FUNC) &gillespie,                5},
    {"lstrtod",                 (DL_FUNC) &lstrtod,                  1},
    {"r_gsl_odeiv2_outer_CRNN", (DL_FUNC) &r_gsl_odeiv2_outer_CRNN, 11},
    {"r_gsl_odeiv2_outer_fi",   (DL_FUNC) &r_gsl_odeiv2_outer_fi,   10},
    {"replace_pow",             (DL_FUNC) &replace_pow,              1},
    {NULL, NULL, 0}
};

void R_init_uqsa(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
