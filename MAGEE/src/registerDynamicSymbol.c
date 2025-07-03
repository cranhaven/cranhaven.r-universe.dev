#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP glmm_gei_bgen11(SEXP res_in, SEXP P_in, SEXP bgenfile_in, SEXP outfile_in, SEXP center_in, SEXP minmaf_in, SEXP maxmaf_in, SEXP minmac_in, SEXP missrate_in, SEXP minrsq_in, SEXP miss_method_in, SEXP nperbatch_in, 
                     SEXP ei_in, SEXP qi_in, SEXP isNullP_in, SEXP isNullEC_in, SEXP strata_in, SEXP select_in, SEXP begin_in, SEXP end_in, SEXP pos_in, SEXP nbgen_in, SEXP compression_in, SEXP metaOutput_in);

SEXP glmm_gei_bgen13(SEXP res_in, SEXP P_in, SEXP bgenfile_in, SEXP outfile_in, SEXP center_in, SEXP minmaf_in, SEXP maxmaf_in, SEXP minmac_in, SEXP missrate_in, SEXP minrsq_in, SEXP miss_method_in, SEXP nperbatch_in, 
                     SEXP ei_in, SEXP qi_in, SEXP isNullP_in, SEXP isNullEC_in, SEXP strata_in, SEXP select_in, SEXP begin_in, SEXP end_in, SEXP pos_in, SEXP nbgen_in, SEXP compression_in, SEXP metaOutput_in);

SEXP bgenHeader(SEXP bgenfile_in);

SEXP getVariantPos(SEXP bgenfile_in, SEXP offset_in, SEXP mbgen_in, SEXP nbgen_in, SEXP compression_in, SEXP layout_in, SEXP cores_in);

SEXP bgenVariantInfo(SEXP bgenfile_in, SEXP offset_in, SEXP mbgen_in, SEXP nbgen_in, SEXP layout_in, SEXP compression_in);

SEXP magee_bgen13(SEXP bgenfile_in, SEXP groupIndex_in, SEXP fbytes_in, SEXP select_in, SEXP compression_in, SEXP n_in);

SEXP magee_bgen11(SEXP bgenfile_in, SEXP groupIndex_in, SEXP fbytes_in, SEXP select_in, SEXP compression_in, SEXP nsamples_in, SEXP n_in);

SEXP clear_exptr(SEXP ptr_in);

static const R_CallMethodDef R_CallDef[]  = {
  {"clear_exptr", (DL_FUNC) &clear_exptr, 1},
  {"magee_bgen13", (DL_FUNC) &magee_bgen13, 6},
  {"magee_bgen11", (DL_FUNC) &magee_bgen11, 7},
  {"glmm_gei_bgen11", (DL_FUNC) &glmm_gei_bgen11, 24},
  {"glmm_gei_bgen13", (DL_FUNC) &glmm_gei_bgen13, 24},
  {"bgenHeader", (DL_FUNC) &bgenHeader, 1},
  {"getVariantPos", (DL_FUNC) &getVariantPos, 7},
  {"bgenVariantInfo", (DL_FUNC) &bgenVariantInfo, 6},
  {NULL, NULL, 0}
};

void R_init_GMMAT(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}

