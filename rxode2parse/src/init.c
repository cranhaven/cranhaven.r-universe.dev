#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "../inst/include/rxode2parse.h"
#define __DOINIT__
#include "tran.h"
#include "../inst/include/rxode2parseSbuf.h"

SEXP _rxode2parse_codeLoaded(void);
SEXP _rxode2parse_codegen(SEXP c_file, SEXP prefix, SEXP libname, SEXP pMd5, SEXP timeId, SEXP lastMv, SEXP goodFuns);
SEXP _rxode2parse_parseModel(SEXP type);
SEXP _rxode2parse_isLinCmt(void);
SEXP _rxode2parse_linCmtB(void);

void transIniNull(void);

SEXP _rxode2parse_rxQs(SEXP xSEXP);
SEXP _rxode2parse_rxQr(SEXP encoded_stringSEXP);

SEXP _rxode2parse_linCmtParse(SEXP vars, SEXP inStr, SEXP verboseSXP);

SEXP _rxode2parse_linCmtGen(SEXP linCmt, SEXP vars, SEXP linCmtSens, SEXP verbose);

SEXP _rxode2parse_rxParseSetSilentErr(SEXP silentSEXP);
SEXP _rxode2parse_rxode2parseSetRstudio(SEXP);
SEXP _rxode2parse_calcDerived(SEXP ncmtSXP, SEXP transSXP, SEXP inp, SEXP sigdigSXP);

SEXP _rxode2parse_parseFreeSexp(SEXP);

SEXP _rxode2parse_rxUpdateTrans_(SEXP, SEXP, SEXP);

double linCmtA(rx_solve *rx, unsigned int id, double t, int linCmt,
               int ncmt, int trans, double d_ka,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_tlag2, double d_F, double d_F2,
               double d_rate, double d_dur, double d_rate2, double d_dur2);

double linCmtC(rx_solve *rx, unsigned int id, double t, int linCmt,
               int ncmt, int trans, double d_ka,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_tlag2, double d_F, double d_F2,
               double d_rate, double d_dur, double d_rate2, double d_dur2);

double linCmtB(rx_solve *rx, unsigned int id, double t, int linCmt,
               int i_cmt, int trans, int val,
               double dd_p1, double dd_v1,
               double dd_p2, double dd_p3,
               double dd_p4, double dd_p5,
               double dd_ka,
               double dd_tlag, double dd_tlag2,
               double dd_F, double dd_F2,
               double dd_rate, double dd_dur,
               double dd_rate2, double dd_dur2);

void _rxode2parse_assignFuns2(rx_solve rx,
                            rx_solving_options op,
                            t_F f,
                            t_LAG lag,
                            t_RATE rate,
                            t_DUR dur,
                            t_calc_mtime mtime,
                            t_ME me,
                            t_IndF indf,
                            t_getTime gettime,
                            t_locateTimeIndex timeindex,
                            t_handle_evidL handleEvid,
                            t_getDur getdur);

SEXP _rxode2_parse_strncmpci(void);
SEXP _rxode2parse_getWh(SEXP in);
SEXP _rxode2parse_getClassicEvid(SEXP, SEXP, SEXP, SEXP, SEXP,
                                 SEXP, SEXP);
SEXP _rxode2parse_linCmtA(SEXP linDat, SEXP linPar);

SEXP _rxode2parse_etTransEvidIsObs(SEXP);
SEXP _rxode2parse_forderForceBase(SEXP);
SEXP _rxode2parse_rxSetIni0(SEXP);
SEXP _rxode2parse_etTransParse(SEXP, SEXP, SEXP, SEXP, SEXP,
                               SEXP, SEXP, SEXP, SEXP, SEXP,
                               SEXP);

SEXP _rxode2parse_rxEtTransAsDataFrame_(SEXP);

SEXP _rxode2parse_convertId_(SEXP x);
SEXP _rxode2parse_get_sexp_unique( SEXP s );


SEXP _rxode2parse_chin(SEXP x, SEXP table);
SEXP _rxode2parse_getForder(void);
int _rxode2parse_useForder(void);

int get_sexp_uniqueL( SEXP s );

 SEXP _rxode2parse_funPtrs(void) {
  int pro = 0;
  SEXP ret = PROTECT(allocVector(VECSXP, 7)); pro++;
  SET_VECTOR_ELT(ret, 0, R_MakeExternalPtrFn((DL_FUNC) &_rxode2parse_convertId_,
                                             Rf_install("_rxode2parse_convertId_"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 1, R_MakeExternalPtrFn((DL_FUNC) &_rxode2parse_get_sexp_unique,
                                             Rf_install("_rxode2parse_get_sexp_unique"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 2, R_MakeExternalPtrFn((DL_FUNC) &_rxode2parse_etTransParse,
                                             Rf_install("_rxode2parse_etTransParse"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 3, R_MakeExternalPtrFn((DL_FUNC) &_rxode2parse_chin,
                                             Rf_install("_rxode2parse_chin"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 4, R_MakeExternalPtrFn((DL_FUNC) &_rxode2parse_getForder,
                                             Rf_install("_rxode2parse_getForder"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 5, R_MakeExternalPtrFn((DL_FUNC) &_rxode2parse_useForder,
                                             Rf_install("_rxode2parse_useForder"),
                                             R_NilValue));
  SET_VECTOR_ELT(ret, 6, R_MakeExternalPtrFn((DL_FUNC) &get_sexp_uniqueL,
                                             Rf_install("get_sexp_uniqueL"),
                                             R_NilValue));

  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(cls, 0, Rf_mkChar("rxode2parseFunPtrs"));
  Rf_setAttrib(ret,R_ClassSymbol, cls);
  UNPROTECT(pro);
  return(ret);
}

double _rxode2parse_evalUdf(const char *fun, int n, const double *args);

void R_init_rxode2parse(DllInfo *info){
  R_CallMethodDef callMethods[]  = {
    {"_rxode2parse_linCmtB", (DL_FUNC) &_rxode2parse_linCmtB},
    {"_rxode2parse_convertId_", (DL_FUNC) &_rxode2parse_convertId_, 1},
    {"_rxode2parse_funPtrs", (DL_FUNC) &_rxode2parse_funPtrs, 0},
    {"_rxode2parse_rxEtTransAsDataFrame_", (DL_FUNC) &_rxode2parse_rxEtTransAsDataFrame_, 1},
    {"_rxode2parse_etTransParse", (DL_FUNC) &_rxode2parse_etTransParse, 11},
    {"_rxode2parse_rxSetIni0", (DL_FUNC) &_rxode2parse_rxSetIni0, 1},
    {"_rxode2parse_forderForceBase", (DL_FUNC) &_rxode2parse_forderForceBase, 1},
    {"_rxode2parse_etTransEvidIsObs", (DL_FUNC) &_rxode2parse_etTransEvidIsObs, 1},
    {"_rxode2parse_getClassicEvid", (DL_FUNC) &_rxode2parse_getClassicEvid, 7},
    {"_rxode2parse_getWh", (DL_FUNC) &_rxode2parse_getWh, 1},
    {"_rxode2_parse_strncmpci", (DL_FUNC) &_rxode2_parse_strncmpci, 0},
    {"_rxode2parse_codeLoaded", (DL_FUNC) &_rxode2parse_codeLoaded, 0},
    {"_rxode2parse_codegen", (DL_FUNC) &_rxode2parse_codegen, 7},
    {"_rxode2parse_parseModel", (DL_FUNC) &_rxode2parse_parseModel, 1},
    {"_rxode2parse_isLinCmt", (DL_FUNC) &_rxode2parse_isLinCmt, 0},
    {"_rxode2parse_trans", (DL_FUNC) &_rxode2parse_trans, 8},
    {"_rxode2parse_linCmtParse", (DL_FUNC) _rxode2parse_linCmtParse, 3},
    {"_rxode2parse_linCmtGen", (DL_FUNC) _rxode2parse_linCmtGen, 4},
    {"_rxode2parse_parseFreeSexp", (DL_FUNC) &_rxode2parse_parseFreeSexp, 1},
    {"_rxode2parse_calcDerived", (DL_FUNC) &_rxode2parse_calcDerived, 4},
    {"_rxode2parse_rxQs", (DL_FUNC) &_rxode2parse_rxQs, 1},
    {"_rxode2parse_rxQr", (DL_FUNC) &_rxode2parse_rxQr, 1},
    {"_rxode2parse_rxParseSetSilentErr", (DL_FUNC) _rxode2parse_rxParseSetSilentErr, 1},
    {"_rxode2parse_rxode2parseSetRstudio", (DL_FUNC) _rxode2parse_rxode2parseSetRstudio, 1},
    {"_rxode2parse_rxUpdateTrans_", (DL_FUNC) _rxode2parse_rxUpdateTrans_, 3},
    {NULL, NULL, 0}
  };
  // C callable to assign environments.
  R_RegisterCCallable("rxode2parse", "_rxode2parse_evalUdf", (DL_FUNC) &_rxode2parse_evalUdf);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_calcDerived", (DL_FUNC) &_rxode2parse_calcDerived);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_parseFree", (DL_FUNC) &_rxode2parse_parseFree);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_trans", (DL_FUNC) &_rxode2parse_trans);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_codegen", (DL_FUNC) &_rxode2parse_codegen);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_codeLoaded", (DL_FUNC) &_rxode2parse_codeLoaded);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_parseModel", (DL_FUNC) &_rxode2parse_parseModel);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_isLinCmt", (DL_FUNC) &_rxode2parse_isLinCmt);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_rxQr", (DL_FUNC) &_rxode2parse_rxQr);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_linCmtParse", (DL_FUNC) &_rxode2parse_linCmtParse);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_linCmtGen", (DL_FUNC) &_rxode2parse_linCmtGen);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_rc_buf_read", (DL_FUNC) &_rxode2parse_rc_buf_read);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_sIniTo", (DL_FUNC) &_rxode2parse_sIniTo);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_sFree", (DL_FUNC) &_rxode2parse_sFree);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_sFreeIni", (DL_FUNC) &_rxode2parse_sFreeIni);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_sAppendN", (DL_FUNC) &_rxode2parse_sAppendN);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_sAppend", (DL_FUNC) &_rxode2parse_sAppend);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_sPrint", (DL_FUNC) &_rxode2parse_sPrint);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_lineIni", (DL_FUNC) &_rxode2parse_lineIni);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_lineFree", (DL_FUNC) &_rxode2parse_lineFree);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_addLine", (DL_FUNC) &_rxode2parse_addLine);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_curLineProp", (DL_FUNC) &_rxode2parse_curLineProp);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_curLineType", (DL_FUNC) &_rxode2parse_curLineType);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_doDot", (DL_FUNC) &_rxode2parse_doDot);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_doDot2", (DL_FUNC) &_rxode2parse_doDot2);
  R_RegisterCCallable("rxode2parse", "linCmtA", (DL_FUNC) &linCmtA);
  R_RegisterCCallable("rxode2parse", "linCmtB", (DL_FUNC) &linCmtB);
  R_RegisterCCallable("rxode2parse", "linCmtC", (DL_FUNC) &linCmtC);
  R_RegisterCCallable("rxode2parse", "_rxode2parse_assignFuns2",
                      (DL_FUNC) &_rxode2parse_assignFuns2);
  // Backward compatible registration
  R_RegisterCCallable("rxode2parse", "_rxode2parseAssignPtrs",
                      (DL_FUNC) &_rxode2parse_assignFuns2);

  // log likelihoods used in calculations
  static const R_CMethodDef cMethods[] = {
    {NULL, NULL, 0, NULL}
  };

  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  transIniNull();
}

void R_unload_rxode2parse(DllInfo *info){
  parseFree(1);
}
