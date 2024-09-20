#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

SEXP _rxode2et_et_(SEXP inputSEXP, SEXP et__SEXP);
SEXP _rxode2et_etUpdate(SEXP objSEXP, SEXP argSEXP, SEXP valueSEXP, SEXP exactSEXP);
SEXP _rxode2et_etDollarNames(SEXP objSEXP);
SEXP _rxode2et_etSeq_(SEXP, SEXP, SEXP, SEXP, SEXP,
                      SEXP, SEXP, SEXP, SEXP, SEXP,
                      SEXP);
SEXP _rxode2et_etRep_(SEXP, SEXP, SEXP, SEXP, SEXP,
                      SEXP, SEXP);

SEXP _rxode2et_forderForceBase(SEXP forceBaseSEXP);
SEXP _rxode2et_rxSetIni0(SEXP ini0SEXP);


SEXP _rxode2et_rxIsEt2(SEXP objSexp);

SEXP _rxode2et_expandPars_(SEXP, SEXP, SEXP, SEXP);

SEXP _rxode2et_chin(SEXP x, SEXP table);

SEXP _rxode2et_setEvCur(SEXP cur);
SEXP _rxode2et_useForder(void);

SEXP _rxode2et_getForder(void);

SEXP _rxode2et_rxStack_(SEXP Data, SEXP vars);
SEXP _rxode2et_rxCbindStudyIndividual(SEXP inputParameters, SEXP individualParameters);
SEXP _rxode2et_cbindThetaOmega(SEXP inputParameters, SEXP individualParameters);

SEXP _rxode2et_rxModelVarsStack(SEXP);

void R_init_rxode2et(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_rxode2et_rxModelVarsStack", (DL_FUNC) &_rxode2et_rxModelVarsStack, 1},
    {"_rxode2et_cbindThetaOmega", (DL_FUNC) &_rxode2et_cbindThetaOmega, 2},
    {"_rxode2et_rxCbindStudyIndividual", (DL_FUNC) &_rxode2et_rxCbindStudyIndividual, 2},
    {"_rxode2et_rxStack_", (DL_FUNC) &_rxode2et_rxStack_, 2},
    {"_rxode2et_useForder", (DL_FUNC) &_rxode2et_useForder, 0},
    {"_rxode2et_getForder", (DL_FUNC) &_rxode2et_getForder, 0},
    {"_rxode2et_chin", (DL_FUNC) &_rxode2et_chin, 2},
    {"_rxode2et_setEvCur", (DL_FUNC) &_rxode2et_setEvCur, 1},
    {"_rxode2et_expandPars_", (DL_FUNC) &_rxode2et_expandPars_, 4},
    {"_rxode2et_rxIsEt2", (DL_FUNC) &_rxode2et_rxIsEt2, 1},
    {"_rxode2et_forderForceBase", (DL_FUNC) &_rxode2et_forderForceBase, 1},
    {"_rxode2et_etRep_", (DL_FUNC) _rxode2et_etRep_, 7},
    {"_rxode2et_etSeq_", (DL_FUNC) _rxode2et_etSeq_, 11},
    {"_rxode2et_etDollarNames", (DL_FUNC) &_rxode2et_etDollarNames, 1},
    {"_rxode2et_et_", (DL_FUNC) &_rxode2et_et_, 2},
    {"_rxode2et_etUpdate", (DL_FUNC) &_rxode2et_etUpdate, 4},
    {NULL, NULL, 0}
  };
  // C callable to assign environments.
  // log likelihoods used in calculations
  static const R_CMethodDef cMethods[] = {
    {NULL, NULL, 0, NULL}
  };

  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

void R_unload_rxode2et(DllInfo *info){
}
