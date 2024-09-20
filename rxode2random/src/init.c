#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "../inst/include/rxode2random.h"
#define __DOINIT__
#include "rxthreefry.h"
#include "seed.h"

SEXP _rxode2random_swapMatListWithCube_(SEXP);
SEXP _rxode2random_omegaListRse(SEXP omegaList);
SEXP _rxode2random_rinvchisq(SEXP nSEXP, SEXP nuSEXP, SEXP scaleSEXP);
SEXP _rxode2random_rLKJ1(SEXP dSEXP, SEXP etaSEXP, SEXP choleskySEXP);
SEXP _rxode2random_rLKJcv1(SEXP sdSEXP, SEXP etaSEXP);
SEXP _rxode2random_rLKJcvLsd1(SEXP logSdSEXP, SEXP logSdSDSEXP, SEXP etaSEXP);
SEXP _rxode2random_invWR1d(SEXP dSEXP, SEXP nuSEXP, SEXP omegaIsCholSEXP);
SEXP _rxode2random_rcvC1(SEXP sdEstSEXP, SEXP nuSEXP, SEXP diagXformTypeSEXP, SEXP rTypeSEXP, SEXP returnCholSEXP);
SEXP _rxode2random_cvPost_(SEXP nuSSEXP, SEXP omegaSSEXP, SEXP nSSEXP, SEXP omegaIsCholSSEXP, SEXP returnCholSSEXP, SEXP typeSSEXP, SEXP diagXformTypeSSEXP);
SEXP _rxode2random_expandTheta_(SEXP thetaSSEXP, SEXP thetaMatSSEXP, SEXP thetaLowerSSEXP, SEXP thetaUpperSSEXP, SEXP nStudSSEXP, SEXP nCoresRVSSEXP);
SEXP _rxode2random_expandPars_(SEXP objectSSEXP, SEXP paramsSSEXP, SEXP eventsSSEXP, SEXP controlSSEXP);
SEXP _rxode2random_nestingInfo_(SEXP omegaSEXP, SEXP dataSEXP);
SEXP _rxode2random_rxRmvn_(SEXP A_SEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP ncoresSEXP, SEXP isCholSEXP);
SEXP _rxode2random_rxMvnrnd(SEXP nSEXP, SEXP LSEXP, SEXP lSEXP, SEXP uSEXP, SEXP muSEXP, SEXP aSEXP, SEXP tolSEXP);
SEXP _rxode2random_rxCholperm(SEXP SigSEXP, SEXP lSEXP, SEXP uSEXP, SEXP epsSEXP);
SEXP _rxode2random_rxGradpsi(SEXP ySEXP, SEXP LSEXP, SEXP lSEXP, SEXP uSEXP);
SEXP _rxode2random_rxNleq(SEXP lSEXP, SEXP uSEXP, SEXP LSEXP);
SEXP _rxode2random_rxMvrandn_(SEXP A_SEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP ncoresSEXP, SEXP aSEXP, SEXP tolSEXP, SEXP nlTolSEXP, SEXP nlMaxiterSEXP);
SEXP _rxode2random_rxSeedEng(SEXP ncoresSEXP);
SEXP _rxode2random_rxnbinom_(SEXP sizeSEXP, SEXP muSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxgamma_(SEXP sizeSEXP, SEXP muSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxpois_(SEXP lambdaSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxnbinomMu_(SEXP sizeSEXP, SEXP muSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxbinom_(SEXP n0SEXP, SEXP probSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxcauchy_(SEXP locationSEXP, SEXP scaleSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxchisq_(SEXP dfSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxexp_(SEXP rateSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxf_(SEXP df1SEXP, SEXP df2SEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxbeta_(SEXP shape1SEXP, SEXP shape2SEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxgeom_(SEXP probSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxnorm_(SEXP meanSEXP, SEXP sdSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxt__(SEXP dfSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxunif_(SEXP lowSEXP, SEXP hiSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxweibull_(SEXP shapeSEXP, SEXP scaleSEXP, SEXP nSEXP, SEXP ncoresSEXP);
SEXP _rxode2random_rxRmvn0(SEXP A_SEXP, SEXP muSEXP, SEXP sigmaSEXP, SEXP lowerSEXP, SEXP upperSEXP, SEXP ncoresSEXP, SEXP isCholSEXP, SEXP aSEXP, SEXP tolSEXP, SEXP nlTolSEXP, SEXP nlMaxiterSEXP);
SEXP _rxode2random_rxRmvnSEXP(SEXP nSSEXP, SEXP muSSEXP, SEXP sigmaSSEXP, SEXP lowerSSEXP, SEXP upperSSEXP, SEXP ncoresSSEXP, SEXP isCholSSEXP, SEXP keepNamesSSEXP, SEXP aSSEXP, SEXP tolSSEXP, SEXP nlTolSSEXP, SEXP nlMaxiterSSEXP);
SEXP _rxode2random_rpp_(SEXP nSSEXP, SEXP lambdaSSEXP, SEXP gammaSSEXP, SEXP probSSEXP, SEXP t0SSEXP, SEXP tmaxSSEXP, SEXP randomOrderSSEXP);
SEXP _rxode2random_rxordSelect(SEXP uSEXP, SEXP csSEXP);
SEXP _rxode2random_rxGetSeed(void);
SEXP _rxode2random_phi(SEXP q);

SEXP _rxode2random_rxSetSeed(SEXP);
SEXP _rxode2random_cbindOme(SEXP, SEXP, SEXP);
SEXP _rxode2random_vecDF(SEXP, SEXP);

bool _rxode2random_qtest(SEXP in, const char *test);
SEXP _rxode2random_qstrictS(SEXP nn, const char *what);
SEXP _rxode2random_qstrictSn(SEXP x_, const char *what);
SEXP _rxode2random_qstrictSdn(SEXP x_, const char *what);

SEXP _rxode2random_qassertS(SEXP in, const char *test, const char *what);
SEXP _rxode2random_qtest_sexp(SEXP in, SEXP test);
SEXP _rxode2random_qstrictS_sexp(SEXP, SEXP);
SEXP _rxode2random_qassertS_sexp(SEXP, SEXP, SEXP);
SEXP _rxode2random_qstrictSn_sexp(SEXP in, SEXP test);
SEXP _rxode2random_qstrictSdn_sexp(SEXP in, SEXP test);

typedef SEXP (*lotriMat_type) (SEXP, SEXP, SEXP);
typedef SEXP (*asLotriMat_type) (SEXP, SEXP, SEXP);
typedef SEXP (*lotriSep_type) (SEXP, SEXP, SEXP, SEXP, SEXP);
typedef SEXP (*lotriAllNames_type) (SEXP);
typedef SEXP (*lotriGetBounds_type) (SEXP, SEXP, SEXP);
typedef SEXP (*isLotri_type) (SEXP);
typedef SEXP (*lotriMaxNu_type) (SEXP);
typedef SEXP (*rxSolveFreeSexp_t)(void);
typedef void (*setZeroMatrix_t)(int which);
typedef SEXP (*etTrans_t)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
typedef void (*rxModelsAssignC_t)(const char* str, SEXP assign);
typedef SEXP (*rxModelVars_SEXP_t)(SEXP);
typedef SEXP (*rxExpandNestingSexp_t)(SEXP, SEXP, SEXP);
typedef SEXP (*chin_t)(SEXP x, SEXP table);
typedef SEXP (*getLowerVec_t)(int type, rx_solve* rx);
typedef SEXP (*getUpperVec_t)(int type, rx_solve* rx);
typedef SEXP (*getArmaMat_t)(int type, int csim, rx_solve* rx);

SEXP _rxode2random_funPtrs(void) {
  int pro = 0;
  SEXP ret = PROTECT(allocVector(VECSXP, 5)); pro++;
  SET_VECTOR_ELT(ret, 0, R_MakeExternalPtrFn((DL_FUNC) &_rxode2random_getRxSeed1,
                                             Rf_install("_rxode2random_getRxSeed1"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 1, R_MakeExternalPtrFn((DL_FUNC) &_rxode2random_setSeedEng1,
                                             Rf_install("_rxode2random_setSeedEng1"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 2, R_MakeExternalPtrFn((DL_FUNC) &_rxode2random_setRxSeedFinal,
                                             Rf_install("_rxode2random_setRxSeedFinal"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 3, R_MakeExternalPtrFn((DL_FUNC) &_rxode2random_seedEng,
                                             Rf_install("_rxode2random_seedEng"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 4, R_MakeExternalPtrFn((DL_FUNC) &rxunif,
                                             Rf_install("rxunif"),
                                             R_NilValue));
  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(cls, 0, Rf_mkChar("rxode2randomFunPtrs"));
  Rf_setAttrib(ret,R_ClassSymbol, cls);
  UNPROTECT(pro);
  return(ret);
}

void _rxode2random_assignPtrsInRxode2(rx_solve rx,
                                      rx_solving_options op,
                                      rxSolveFreeSexp_t rSF,
                                      setZeroMatrix_t sZM,
                                      etTrans_t et,
                                      rxModelsAssignC_t rmac,
                                      rxModelVars_SEXP_t mv,
                                      rxExpandNestingSexp_t rens,
                                      chin_t cin,
                                      getLowerVec_t glv,
                                      getUpperVec_t guv,
                                      getArmaMat_t gams);
  void _rxode2random_assignSolveOnly(rx_solve rx,
                                     rx_solving_options op);

void R_init_rxode2random(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_rxode2random_swapMatListWithCube_", (DL_FUNC) &_rxode2random_swapMatListWithCube_, 1},
    {"_rxode2random_omegaListRse", (DL_FUNC) &_rxode2random_omegaListRse, 1},
    {"_rxode2random_funPtrs", (DL_FUNC) &_rxode2random_funPtrs, 0},
    {"_rxode2random_qassertS_sexp", (DL_FUNC) &_rxode2random_qassertS_sexp, 3},
    {"_rxode2random_vecDF", (DL_FUNC) &_rxode2random_vecDF, 2},
    {"_rxode2random_cbindOme", (DL_FUNC) &_rxode2random_cbindOme, 3},
    {"_rxode2random_rxSetSeed", (DL_FUNC) &_rxode2random_rxSetSeed, 1},
    {"_rxode2random_rinvchisq", (DL_FUNC) &_rxode2random_rinvchisq, 3},
    {"_rxode2random_rLKJ1", (DL_FUNC) &_rxode2random_rLKJ1, 3},
    {"_rxode2random_rLKJcv1", (DL_FUNC) &_rxode2random_rLKJcv1, 2},
    {"_rxode2random_rLKJcvLsd1", (DL_FUNC) &_rxode2random_rLKJcvLsd1, 3},
    {"_rxode2random_invWR1d", (DL_FUNC) &_rxode2random_invWR1d, 3},
    {"_rxode2random_rcvC1", (DL_FUNC) &_rxode2random_rcvC1, 5},
    {"_rxode2random_cvPost_", (DL_FUNC) &_rxode2random_cvPost_, 7},
    {"_rxode2random_expandTheta_", (DL_FUNC) &_rxode2random_expandTheta_, 6},
    {"_rxode2random_expandPars_", (DL_FUNC) &_rxode2random_expandPars_, 4},
    {"_rxode2random_nestingInfo_", (DL_FUNC) &_rxode2random_nestingInfo_, 2},
    {"_rxode2random_rxRmvn_", (DL_FUNC) &_rxode2random_rxRmvn_, 5},
    {"_rxode2random_rxMvnrnd", (DL_FUNC) &_rxode2random_rxMvnrnd, 7},
    {"_rxode2random_rxCholperm", (DL_FUNC) &_rxode2random_rxCholperm, 4},
    {"_rxode2random_rxGradpsi", (DL_FUNC) &_rxode2random_rxGradpsi, 4},
    {"_rxode2random_rxNleq", (DL_FUNC) &_rxode2random_rxNleq, 3},
    {"_rxode2random_rxMvrandn_", (DL_FUNC) &_rxode2random_rxMvrandn_, 10},
    {"_rxode2random_rxSeedEng", (DL_FUNC) &_rxode2random_rxSeedEng, 1},
    {"_rxode2random_rxnbinomMu_", (DL_FUNC) &_rxode2random_rxnbinomMu_, 4},
    {"_rxode2random_rxnbinom_", (DL_FUNC) &_rxode2random_rxnbinom_, 4},
    {"_rxode2random_rxbinom_", (DL_FUNC) &_rxode2random_rxbinom_, 4},
    {"_rxode2random_rxcauchy_", (DL_FUNC) &_rxode2random_rxcauchy_, 4},
    {"_rxode2random_rxchisq_", (DL_FUNC) &_rxode2random_rxchisq_, 3},
    {"_rxode2random_rxexp_", (DL_FUNC) &_rxode2random_rxexp_, 3},
    {"_rxode2random_rxf_", (DL_FUNC) &_rxode2random_rxf_, 4},
    {"_rxode2random_rxgamma_", (DL_FUNC) &_rxode2random_rxgamma_, 4},
    {"_rxode2random_rxbeta_", (DL_FUNC) &_rxode2random_rxbeta_, 4},
    {"_rxode2random_rxgeom_", (DL_FUNC) &_rxode2random_rxgeom_, 3},
    {"_rxode2random_rxnorm_", (DL_FUNC) &_rxode2random_rxnorm_, 4},
    {"_rxode2random_rxpois_", (DL_FUNC) &_rxode2random_rxpois_, 3},
    {"_rxode2random_rxt__", (DL_FUNC) &_rxode2random_rxt__, 3},
    {"_rxode2random_rxunif_", (DL_FUNC) &_rxode2random_rxunif_, 4},
    {"_rxode2random_rxweibull_", (DL_FUNC) &_rxode2random_rxweibull_, 4},
    {"_rxode2random_rxRmvn0", (DL_FUNC) &_rxode2random_rxRmvn0, 11},
    {"_rxode2random_rxRmvnSEXP", (DL_FUNC) &_rxode2random_rxRmvnSEXP, 12},
    {"_rxode2random_rpp_", (DL_FUNC) &_rxode2random_rpp_, 7},
    {"_rxode2random_rxordSelect", (DL_FUNC) &_rxode2random_rxordSelect, 2},
    {"_rxode2random_rxGetSeed", (DL_FUNC) &_rxode2random_rxGetSeed, 0},
    {"_rxode2random_phi", (DL_FUNC) &_rxode2random_phi, 1},
    {"_rxode2random_qtest_sexp", (DL_FUNC) &_rxode2random_qtest_sexp, 2},
    {"_rxode2random_qstrictS_sexp", (DL_FUNC) &_rxode2random_qstrictS_sexp, 2},
    {"_rxode2random_qstrictSn_sexp", (DL_FUNC) &_rxode2random_qstrictSn_sexp, 2},
    {"_rxode2random_qstrictSdn_sexp", (DL_FUNC) &_rxode2random_qstrictSdn_sexp, 2},
    {NULL, NULL, 0} 
  };
  // C callable to assign environments.
  R_RegisterCCallable("rxode2random", "_rxode2random_assignPtrsInRxode2", (DL_FUNC) &_rxode2random_assignPtrsInRxode2);
  R_RegisterCCallable("rxode2random", "_rxode2random_cbindOme", (DL_FUNC) &_rxode2random_cbindOme);
  R_RegisterCCallable("rxode2random", "_rxode2random_cvPost_", (DL_FUNC) &_rxode2random_cvPost_);
  R_RegisterCCallable("rxode2random", "_rxode2random_expandPars_", (DL_FUNC) &_rxode2random_expandPars_);
  R_RegisterCCallable("rxode2random", "_rxode2random_expandTheta_", (DL_FUNC) &_rxode2random_expandTheta_);
  R_RegisterCCallable("rxode2random", "_rxode2random_getRxSeed1", (DL_FUNC) &_rxode2random_getRxSeed1);
  R_RegisterCCallable("rxode2random", "_rxode2random_invWR1d", (DL_FUNC) &_rxode2random_invWR1d);
  R_RegisterCCallable("rxode2random", "_rxode2random_nestingInfo_", (DL_FUNC) &_rxode2random_nestingInfo_);
  R_RegisterCCallable("rxode2random", "_rxode2random_phi", (DL_FUNC) &_rxode2random_phi);
  R_RegisterCCallable("rxode2random", "_rxode2random_qassertS", (DL_FUNC) &_rxode2random_qassertS);
  R_RegisterCCallable("rxode2random", "_rxode2random_qstrictS", (DL_FUNC) &_rxode2random_qstrictS);
  R_RegisterCCallable("rxode2random", "_rxode2random_qstrictSdn", (DL_FUNC) &_rxode2random_qstrictSdn);
  R_RegisterCCallable("rxode2random", "_rxode2random_qstrictSn", (DL_FUNC) &_rxode2random_qstrictSn);
  R_RegisterCCallable("rxode2random", "_rxode2random_qtest", (DL_FUNC) &_rxode2random_qtest);
  R_RegisterCCallable("rxode2random", "_rxode2random_rLKJ1", (DL_FUNC) &_rxode2random_rLKJ1);
  R_RegisterCCallable("rxode2random", "_rxode2random_rLKJcv1", (DL_FUNC) &_rxode2random_rLKJcv1);
  R_RegisterCCallable("rxode2random", "_rxode2random_rLKJcvLsd1", (DL_FUNC) &_rxode2random_rLKJcvLsd1);
  R_RegisterCCallable("rxode2random", "_rxode2random_rcvC1", (DL_FUNC) &_rxode2random_rcvC1);
  R_RegisterCCallable("rxode2random", "_rxode2random_rinvchisq", (DL_FUNC) &_rxode2random_rinvchisq);
  R_RegisterCCallable("rxode2random", "_rxode2random_rpp_", (DL_FUNC) &_rxode2random_rpp_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxCholperm", (DL_FUNC) &_rxode2random_rxCholperm);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxGetSeed", (DL_FUNC) &_rxode2random_rxGetSeed);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxGradpsi", (DL_FUNC) &_rxode2random_rxGradpsi);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxMvnrnd", (DL_FUNC) &_rxode2random_rxMvnrnd);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxMvrandn_p", (DL_FUNC) &_rxode2random_rxMvrandn_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxNleq", (DL_FUNC) &_rxode2random_rxNleq);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxRmvn0", (DL_FUNC) &_rxode2random_rxRmvn0);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxRmvnSEXP", (DL_FUNC) &_rxode2random_rxRmvnSEXP);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxRmvn_", (DL_FUNC) &_rxode2random_rxRmvn_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxSeedEng",(DL_FUNC) &_rxode2random_rxSeedEng);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxbeta_", (DL_FUNC) &_rxode2random_rxbeta_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxbinom_",(DL_FUNC) &_rxode2random_rxbinom_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxcauchy_",(DL_FUNC) &_rxode2random_rxcauchy_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxchisq_",(DL_FUNC) &_rxode2random_rxchisq_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxexp_",(DL_FUNC) &_rxode2random_rxexp_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxf_",(DL_FUNC) &_rxode2random_rxf_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxgamma_",(DL_FUNC) &_rxode2random_rxgamma_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxgeom_", (DL_FUNC) &_rxode2random_rxgeom_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxnbinomMu_",(DL_FUNC) &_rxode2random_rxnbinomMu_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxnbinom_",(DL_FUNC) &_rxode2random_rxnbinom_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxnorm_", (DL_FUNC) &_rxode2random_rxnorm_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxordSelect", (DL_FUNC) &_rxode2random_rxordSelect);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxpois_",(DL_FUNC) &_rxode2random_rxpois_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxt__", (DL_FUNC) &_rxode2random_rxt__);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxunif_", (DL_FUNC) &_rxode2random_rxunif_);
  R_RegisterCCallable("rxode2random", "_rxode2random_rxweibull_", (DL_FUNC) &_rxode2random_rxweibull_);
  R_RegisterCCallable("rxode2random", "_rxode2random_seedEng", (DL_FUNC) &_rxode2random_seedEng);
  R_RegisterCCallable("rxode2random", "_rxode2random_setRxSeedFinal", (DL_FUNC) &_rxode2random_setRxSeedFinal);
  R_RegisterCCallable("rxode2random", "_rxode2random_setSeedEng1", (DL_FUNC) &_rxode2random_setSeedEng1);
  R_RegisterCCallable("rxode2random", "_rxode2random_vecDF", (DL_FUNC) &_rxode2random_vecDF);
  R_RegisterCCallable("rxode2random", "phi", (DL_FUNC) &phi);
  R_RegisterCCallable("rxode2random", "ribeta", (DL_FUNC) &ribeta);
  R_RegisterCCallable("rxode2random", "ribinom", (DL_FUNC) &ribinom);
  R_RegisterCCallable("rxode2random", "ricauchy", (DL_FUNC) &ricauchy);
  R_RegisterCCallable("rxode2random", "richisq", (DL_FUNC) &richisq);
  R_RegisterCCallable("rxode2random", "riexp", (DL_FUNC) &riexp);
  R_RegisterCCallable("rxode2random", "rif", (DL_FUNC) &rif);
  R_RegisterCCallable("rxode2random", "rigamma", (DL_FUNC) &rigamma);
  R_RegisterCCallable("rxode2random", "rigeom", (DL_FUNC) &rigeom);
  R_RegisterCCallable("rxode2random", "rinbinom", (DL_FUNC) &rinbinom);
  R_RegisterCCallable("rxode2random", "rinbinomMu", (DL_FUNC) &rinbinomMu);
  R_RegisterCCallable("rxode2random", "rinorm", (DL_FUNC) &rinorm);
  R_RegisterCCallable("rxode2random", "ripois", (DL_FUNC) &ripois);
  R_RegisterCCallable("rxode2random", "rit_", (DL_FUNC) &rit_);
  R_RegisterCCallable("rxode2random", "riunif", (DL_FUNC) &riunif);
  R_RegisterCCallable("rxode2random", "riweibull", (DL_FUNC) &riweibull);
  R_RegisterCCallable("rxode2random", "rxbeta", (DL_FUNC) &rxbeta);
  R_RegisterCCallable("rxode2random", "rxbinom", (DL_FUNC) &rxbinom);
  R_RegisterCCallable("rxode2random", "rxcauchy", (DL_FUNC) &rxcauchy);
  R_RegisterCCallable("rxode2random", "rxchisq", (DL_FUNC) &rxchisq);
  R_RegisterCCallable("rxode2random", "rxexp", (DL_FUNC) &rxexp);
  R_RegisterCCallable("rxode2random", "rxf", (DL_FUNC) &rxf);
  R_RegisterCCallable("rxode2random", "rxgamma", (DL_FUNC) &rxgamma);
  R_RegisterCCallable("rxode2random", "rxgeom", (DL_FUNC) &rxgeom);
  R_RegisterCCallable("rxode2random", "rxnbinom", (DL_FUNC) &rxnbinom);
  R_RegisterCCallable("rxode2random", "rxnbinomMu", (DL_FUNC) &rxnbinomMu);
  R_RegisterCCallable("rxode2random", "rxnorm", (DL_FUNC) &rxnorm);
  R_RegisterCCallable("rxode2random", "rxpois", (DL_FUNC) &rxpois);
  R_RegisterCCallable("rxode2random", "rxt_", (DL_FUNC) &rxt_);
  R_RegisterCCallable("rxode2random", "rxunif", (DL_FUNC) &rxunif);
  R_RegisterCCallable("rxode2random", "rxweibull", (DL_FUNC) &rxweibull);
  R_RegisterCCallable("rxode2random", "simeps", (DL_FUNC) &simeps);
  R_RegisterCCallable("rxode2random", "simeta", (DL_FUNC) &simeta);
  R_RegisterCCallable("rxode2random", "_rxode2random_assignSolveOnly", (DL_FUNC) &_rxode2random_assignSolveOnly);


  // log likelihoods used in calculations
  static const R_CMethodDef cMethods[] = {
    {NULL, NULL, 0, NULL}
  };

  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void R_unload_rxode2random(DllInfo *info){
}
 
