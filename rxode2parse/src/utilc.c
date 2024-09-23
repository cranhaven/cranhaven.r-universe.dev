#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <sys/stat.h> 
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
#include <errno.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2parse", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

#define op_global _rxode2parse_op_global
#define rx_global _rxode2parse_rx_global
#define AMT _rxode2parse_AMT
#define LAG _rxode2parse_LAG
#define RATE _rxode2parse_RATE
#define DUR _rxode2parse_DUR
#define calc_mtime _rxode2parse_calc_mtime
#define getTime_ _rxode2parse_getTime_
#define getTime _rxode2parse_getTime
#define _locateTimeIndex _rxode2parse_locateTimeIndex
#define handle_evidL _rxode2parse_handle_evidL

#include "../inst/include/rxode2parse.h"
extern rx_solve _rxode2parse_rx_global;
extern t_handle_evidL _rxode2parse_handle_evidL;
extern t_getDur _rxode2parse_getDur;

#include "../inst/include/rxode2parseHandleEvid.h"

int _setSilentErr=0, _isRstudio2=0;
extern void setSilentErr(int silent){
  _setSilentErr = silent;
}

extern void setRstudioPrint(int rstudio){
  _isRstudio2=rstudio;
}


extern int getSilentErr(void){return _setSilentErr;}

extern int getRstudioPrint(void){return _isRstudio2;}

extern void RSprintf(const char *format, ...) {
  if (_setSilentErr == 0) {
    if(_isRstudio2){
      va_list args;
      va_start(args, format);
      REvprintf(format, args);
      va_end(args);
    } else{
      va_list args;
      va_start(args, format);
      Rvprintf(format, args);
      va_end(args);
    } 
  }
}


SEXP _rxode2parse_getWh(SEXP in) {
  int wh, cmt, wh100, whI, wh0;
  getWh(INTEGER(in)[0], &wh, &cmt, &wh100, &whI, &wh0);
  SEXP ret = PROTECT(Rf_allocVector(INTSXP, 5));
  int *retI = INTEGER(ret);
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 5));
  retI[0] = wh;
  SET_STRING_ELT(retN, 0,Rf_mkChar("wh"));
  retI[1] = cmt;
  SET_STRING_ELT(retN, 1,Rf_mkChar("cmt"));
  retI[2] = wh100;
  SET_STRING_ELT(retN, 2,Rf_mkChar("wh100"));
  retI[3] = whI;
  SET_STRING_ELT(retN, 3,Rf_mkChar("whI"));
  retI[4] = wh0;
  SET_STRING_ELT(retN, 4,Rf_mkChar("wh0"));
  Rf_setAttrib(ret, R_NamesSymbol, retN);
  UNPROTECT(2);
  return ret;
}

SEXP _rxode2parse_getClassicEvid(SEXP cmtS, SEXP amtS, SEXP rateS,
                                 SEXP durS, SEXP iiS, SEXP evidS, SEXP ssS) {
  int *cmt= INTEGER(cmtS);
  double *amt = REAL(amtS);
  double *dur = REAL(durS);
  double *rate = REAL(rateS);
  double *ii = REAL(iiS);
  int *evid = INTEGER(evidS);
  double *ss = REAL(ssS);
  SEXP retS = PROTECT(Rf_allocVector(INTSXP, Rf_length(cmtS)));
  int *ret = INTEGER(retS);
  for (int i = Rf_length(cmtS); i--;) {
    ret[i] = getEvidClassic(cmt[i], amt[i], rate[i], dur[i], ii[i], evid[i], ss[i]);
  }
  UNPROTECT(1);
  return retS;
}
