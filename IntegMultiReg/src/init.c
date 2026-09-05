/*
 * Native routine registration for the IntegMultiReg package.
 *
 * Registering the .Call entry points lets R resolve them by their
 * registered symbols (see useDynLib(..., .registration = TRUE) in
 * NAMESPACE) instead of by a run-time string search, which is both
 * faster and required for a clean `R CMD check`.
 *
 * We also switch off the GSL default error handler here.  By default
 * GSL aborts the process on a numerical error, which would crash the
 * whole R session; turning it off makes the routines return error
 * codes instead, keeping R alive.
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <gsl/gsl_errno.h>

/* Entry points implemented in the package's C sources. */
extern SEXP main_function(
    SEXP h0_R, SEXP hh_R, SEXP alpha_R, SEXP psi_R, SEXP alpha0_R, SEXP beta0_R,
    SEXP seed_R, SEXP nu_R, SEXP method1_R, SEXP n_platforms_R,
    SEXP platform_models_R, SEXP model_platforms_R, SEXP n_subgroups_R, SEXP sample_size,
    SEXP nbr_features, SEXP nbr_cov, SEXP X1_filtered, SEXP newYY_list,
    SEXP type_outcome, SEXP newCC_list, SEXP sample, SEXP burnin);

extern SEXP main_function_prediction(
    SEXP h0_R, SEXP hh_R, SEXP alpha_R, SEXP psi_R, SEXP alpha0_R, SEXP beta0_R,
    SEXP seed_R, SEXP nu_R, SEXP latent_y_R, SEXP gamma_sample_R, SEXP Theta_R,
    SEXP method1_R, SEXP n_platforms_R, SEXP platform_models_R, SEXP model_platforms_R,
    SEXP n_subgroups_R, SEXP sample_size, SEXP nbr_features, SEXP nbr_cov,
    SEXP X1_filtered, SEXP newYY_list, SEXP newCC_list, SEXP type_outcome,
    SEXP sample, SEXP kcv_R, SEXP rounds_R, SEXP max_models_R);

extern SEXP main_function_prediction_test(
    SEXP h0_R, SEXP hh_R, SEXP alpha_R, SEXP psi_R, SEXP alpha0_R, SEXP beta0_R,
    SEXP seed_R, SEXP nu_R, SEXP latent_y_R, SEXP gamma_sample_R, SEXP Theta_R,
    SEXP method1_R, SEXP n_platforms_R, SEXP platform_models_R, SEXP model_platforms_R,
    SEXP n_subgroups_R, SEXP sample_size, SEXP nbr_features, SEXP nbr_cov,
    SEXP X1_filtered, SEXP newCC_list, SEXP sample, SEXP X1test, SEXP C_test,
    SEXP samplesize_test_R, SEXP max_models_R);

static const R_CallMethodDef CallEntries[] = {
    {"mainFunction",               (DL_FUNC) &main_function,               22},
    {"mainFunctionPrediction",     (DL_FUNC) &main_function_prediction,     27},
    {"mainFunctionPredictionTest", (DL_FUNC) &main_function_prediction_test, 26},
    {NULL, NULL, 0}
};

void R_init_IntegMultiReg(DllInfo *dll)
{
    /* Keep GSL numerical errors from aborting the R session. */
    gsl_set_error_handler_off();

    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    /* Registered routines are still resolved by name through the
     * registration table, so the .Call("mainFunction", ...) form used in
     * the R sources keeps working; we only disable the fallback search
     * for unregistered symbols.  We do not force symbols, which would
     * forbid the character-string form. */
    R_useDynamicSymbols(dll, FALSE);
}
