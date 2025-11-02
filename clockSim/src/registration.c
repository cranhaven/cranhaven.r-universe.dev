#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void deriv_leloup_goldbeter_initmod_desolve(void *);
extern void deriv_leloup_goldbeter_rhs_dde(size_t, double, double *, double *, void *);
extern void deriv_leloup_goldbeter_rhs_desolve(int *, double *, double *, double *, double *, int *);
extern void discrete_leloup_goldbeter_rhs_dde(size_t, size_t, double *, double *, size_t, double *, void *);
extern void discreteNoisy_leloup_goldbeter_rhs_dde(size_t, size_t, double *, double *, size_t, double *, void *);

/* .Call calls */
extern SEXP deriv_leloup_goldbeter_contents(SEXP);
extern SEXP deriv_leloup_goldbeter_create(SEXP);
extern SEXP deriv_leloup_goldbeter_initial_conditions(SEXP, SEXP);
extern SEXP deriv_leloup_goldbeter_metadata(SEXP);
extern SEXP deriv_leloup_goldbeter_rhs_r(SEXP, SEXP, SEXP);
extern SEXP deriv_leloup_goldbeter_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP deriv_leloup_goldbeter_set_user(SEXP, SEXP);
extern SEXP discrete_leloup_goldbeter_contents(SEXP);
extern SEXP discrete_leloup_goldbeter_create(SEXP);
extern SEXP discrete_leloup_goldbeter_initial_conditions(SEXP, SEXP);
extern SEXP discrete_leloup_goldbeter_metadata(SEXP);
extern SEXP discrete_leloup_goldbeter_rhs_r(SEXP, SEXP, SEXP);
extern SEXP discrete_leloup_goldbeter_set_initial(SEXP, SEXP, SEXP);
extern SEXP discrete_leloup_goldbeter_set_user(SEXP, SEXP);
extern SEXP discreteNoisy_leloup_goldbeter_contents(SEXP);
extern SEXP discreteNoisy_leloup_goldbeter_create(SEXP);
extern SEXP discreteNoisy_leloup_goldbeter_initial_conditions(SEXP, SEXP);
extern SEXP discreteNoisy_leloup_goldbeter_metadata(SEXP);
extern SEXP discreteNoisy_leloup_goldbeter_rhs_r(SEXP, SEXP, SEXP);
extern SEXP discreteNoisy_leloup_goldbeter_set_initial(SEXP, SEXP, SEXP);
extern SEXP discreteNoisy_leloup_goldbeter_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"deriv_leloup_goldbeter_initmod_desolve", (DL_FUNC) &deriv_leloup_goldbeter_initmod_desolve, 1},
    {"deriv_leloup_goldbeter_rhs_dde",         (DL_FUNC) &deriv_leloup_goldbeter_rhs_dde,         1},
    {"deriv_leloup_goldbeter_rhs_desolve",     (DL_FUNC) &deriv_leloup_goldbeter_rhs_desolve,     1},
    {"discrete_leloup_goldbeter_rhs_dde",      (DL_FUNC) &discrete_leloup_goldbeter_rhs_dde,      1},
    {"discreteNoisy_leloup_goldbeter_rhs_dde", (DL_FUNC) &discreteNoisy_leloup_goldbeter_rhs_dde, 1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"deriv_leloup_goldbeter_contents",                   (DL_FUNC) &deriv_leloup_goldbeter_contents,                   1},
    {"deriv_leloup_goldbeter_create",                     (DL_FUNC) &deriv_leloup_goldbeter_create,                     1},
    {"deriv_leloup_goldbeter_initial_conditions",         (DL_FUNC) &deriv_leloup_goldbeter_initial_conditions,         2},
    {"deriv_leloup_goldbeter_metadata",                   (DL_FUNC) &deriv_leloup_goldbeter_metadata,                   1},
    {"deriv_leloup_goldbeter_rhs_r",                      (DL_FUNC) &deriv_leloup_goldbeter_rhs_r,                      3},
    {"deriv_leloup_goldbeter_set_initial",                (DL_FUNC) &deriv_leloup_goldbeter_set_initial,                4},
    {"deriv_leloup_goldbeter_set_user",                   (DL_FUNC) &deriv_leloup_goldbeter_set_user,                   2},
    {"discrete_leloup_goldbeter_contents",                (DL_FUNC) &discrete_leloup_goldbeter_contents,                1},
    {"discrete_leloup_goldbeter_create",                  (DL_FUNC) &discrete_leloup_goldbeter_create,                  1},
    {"discrete_leloup_goldbeter_initial_conditions",      (DL_FUNC) &discrete_leloup_goldbeter_initial_conditions,      2},
    {"discrete_leloup_goldbeter_metadata",                (DL_FUNC) &discrete_leloup_goldbeter_metadata,                1},
    {"discrete_leloup_goldbeter_rhs_r",                   (DL_FUNC) &discrete_leloup_goldbeter_rhs_r,                   3},
    {"discrete_leloup_goldbeter_set_initial",             (DL_FUNC) &discrete_leloup_goldbeter_set_initial,             4},
    {"discrete_leloup_goldbeter_set_user",                (DL_FUNC) &discrete_leloup_goldbeter_set_user,                2},
    {"discreteNoisy_leloup_goldbeter_contents",           (DL_FUNC) &discreteNoisy_leloup_goldbeter_contents,           1},
    {"discreteNoisy_leloup_goldbeter_create",             (DL_FUNC) &discreteNoisy_leloup_goldbeter_create,             1},
    {"discreteNoisy_leloup_goldbeter_initial_conditions", (DL_FUNC) &discreteNoisy_leloup_goldbeter_initial_conditions, 2},
    {"discreteNoisy_leloup_goldbeter_metadata",           (DL_FUNC) &discreteNoisy_leloup_goldbeter_metadata,           1},
    {"discreteNoisy_leloup_goldbeter_rhs_r",              (DL_FUNC) &discreteNoisy_leloup_goldbeter_rhs_r,              3},
    {"discreteNoisy_leloup_goldbeter_set_initial",        (DL_FUNC) &discreteNoisy_leloup_goldbeter_set_initial,        4},
    {"discreteNoisy_leloup_goldbeter_set_user",           (DL_FUNC) &discreteNoisy_leloup_goldbeter_set_user,           2},
    {NULL, NULL, 0}
};

void R_init_clockSim(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
