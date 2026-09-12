#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rsolve_GaussModel(SEXP d_, SEXP mu_, SEXP sig_, SEXP data_, SEXP chat_,
  SEXP verbose_, SEXP stepvec_);
extern SEXP rsolve_ExpModel(SEXP d_, SEXP lmd_, SEXP data_, SEXP chat_, SEXP verbose_,
  SEXP stepvec_);
extern SEXP rcmp_coeff1(SEXP d_, SEXP M1_);
extern SEXP rcmp_coeff2(SEXP d_, SEXP M1_, SEXP M2_);
extern SEXP reval_GaussModel(SEXP coeff_, SEXP mu_, SEXP sig_, SEXP data_);
extern SEXP reval_ExpModel(SEXP coeff_, SEXP lmd_, SEXP data_);
extern SEXP reval_poly(SEXP coeff_, SEXP data_);
extern SEXP rpolyaxb(SEXP coeff_, SEXP c, SEXP alpha_, SEXP beta_);
extern SEXP rhistmean(SEXP data_, SEXP freq_);
extern SEXP rdatastats(SEXP data_, SEXP freq_);
extern SEXP rcdf_polygauss(SEXP coeff_, SEXP mu_, SEXP sig_, SEXP xv_);
extern SEXP rcdf_polyggamma(SEXP coeff_, SEXP alpha_, SEXP lmd_, SEXP p_, SEXP xv_);
extern SEXP rigamma(SEXP a_, SEXP x_);
extern SEXP ricgamma(SEXP a_, SEXP x_);

static const R_CallMethodDef callMethods[]  = {
  {"rsolve_GaussModel_", (DL_FUNC) &rsolve_GaussModel, 7},
  {"rsolve_ExpModel_", (DL_FUNC) &rsolve_ExpModel, 6},
  {"rcmp_coeff1_", (DL_FUNC) &rcmp_coeff1, 2},
  {"rcmp_coeff2_", (DL_FUNC) &rcmp_coeff2, 3},
  {"reval_GaussModel_", (DL_FUNC) &reval_GaussModel, 4},
  {"reval_ExpModel_", (DL_FUNC) &reval_ExpModel, 3},
  {"reval_poly_", (DL_FUNC) &reval_poly, 2},
  {"rpolyaxb_", (DL_FUNC) &rpolyaxb, 4},
  {"rhistmean_", (DL_FUNC) &rhistmean, 2},
  {"rdatastats_", (DL_FUNC) &rdatastats, 2},
  {"rcdf_polygauss_", (DL_FUNC) &rcdf_polygauss, 4},
  {"rcdf_polyggamma_", (DL_FUNC) &rcdf_polyggamma, 5},
  {"rigamma_", (DL_FUNC) &rigamma, 2},
  {"ricgamma_", (DL_FUNC) &ricgamma, 2},
  {NULL, NULL, 0}
};

void R_init_dsdp(DllInfo *info)
{
  /* Register routines, allocate resources. */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
