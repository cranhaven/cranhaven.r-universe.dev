#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _RobustCalibration_Accept_proposal(SEXP);
extern SEXP _RobustCalibration_Chol_Eigen(SEXP);
extern SEXP _RobustCalibration_Get_inv_all(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Get_R_new(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Get_R_z_new(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Log_marginal_post(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Log_marginal_post_delta(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Log_marginal_post_no_discrepancy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Log_profile_lik(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Loss_function_no_discrepancy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Mogihammer(SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Sample_delta(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Sample_sigma_2_theta_m(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Sample_sigma_2_theta_m_no_discrepancy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _RobustCalibration_Update_R_inv_y(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_RobustCalibration_Accept_proposal",                       (DL_FUNC) &_RobustCalibration_Accept_proposal,                        1},
    {"_RobustCalibration_Chol_Eigen",                            (DL_FUNC) &_RobustCalibration_Chol_Eigen,                             1},
    {"_RobustCalibration_Get_inv_all",                           (DL_FUNC) &_RobustCalibration_Get_inv_all,                            8},
    {"_RobustCalibration_Get_R_new",                             (DL_FUNC) &_RobustCalibration_Get_R_new,                              6},
    {"_RobustCalibration_Get_R_z_new",                           (DL_FUNC) &_RobustCalibration_Get_R_z_new,                            7},
    {"_RobustCalibration_Log_marginal_post",                     (DL_FUNC) &_RobustCalibration_Log_marginal_post,                     13},
    {"_RobustCalibration_Log_marginal_post_delta",               (DL_FUNC) &_RobustCalibration_Log_marginal_post_delta,                7},
    {"_RobustCalibration_Log_marginal_post_no_discrepancy",      (DL_FUNC) &_RobustCalibration_Log_marginal_post_no_discrepancy,       9},
    {"_RobustCalibration_Log_profile_lik",                       (DL_FUNC) &_RobustCalibration_Log_profile_lik,                       15},
    {"_RobustCalibration_Loss_function_no_discrepancy",          (DL_FUNC) &_RobustCalibration_Loss_function_no_discrepancy,           8},
    {"_RobustCalibration_Mogihammer",                            (DL_FUNC) &_RobustCalibration_Mogihammer,                             3},
    {"_RobustCalibration_Sample_delta",                          (DL_FUNC) &_RobustCalibration_Sample_delta,                           7},
    {"_RobustCalibration_Sample_sigma_2_theta_m",                (DL_FUNC) &_RobustCalibration_Sample_sigma_2_theta_m,                10},
    {"_RobustCalibration_Sample_sigma_2_theta_m_no_discrepancy", (DL_FUNC) &_RobustCalibration_Sample_sigma_2_theta_m_no_discrepancy,  9},
    {"_RobustCalibration_Update_R_inv_y",                        (DL_FUNC) &_RobustCalibration_Update_R_inv_y,                         7},
    {NULL, NULL, 0}
};

void R_init_RobustCalibration(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
