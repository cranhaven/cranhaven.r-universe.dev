#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include "defines.h"
#include <rxode2parse.h>
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2et", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

using namespace Rcpp;

#define rxode2random_loaded rxode2et_rxode2random_loaded
#define rxode2random rxode2et_rxode2random
#define loadNamespace rxode2et_loadNamespace

Function loadNamespace("loadNamespace", R_BaseNamespace);
bool rxode2random_loaded = false;
Environment rxode2random;


extern "C" bool _rxode2et_qtest(SEXP in, const char *test) {
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qtest"]);
  CharacterVector c(1);
  c[0]= test;
  return as<bool>(fun(in, c));
}

extern "C" SEXP _rxode2et_qstrictS(SEXP nn, const char *what) {
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qstrictS"]);
  CharacterVector c(1);
  c[0]= what;
  return (fun(nn, c));
}

extern "C" SEXP _rxode2et_qstrictSn(SEXP x_, const char *what) {
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qstrictSn"]);
  CharacterVector c(1);
  c[0]= what;
  return (fun(x_, c));
}

extern "C" SEXP _rxode2et_qstrictSdn(SEXP x_, const char *what) {
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qstrictSdn"]);
  CharacterVector c(1);
  c[0]= what;
  return (fun(x_, c));
}

extern "C" SEXP _rxode2et_qassertS(SEXP in, const char *test, const char *what) {
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".qassertS"]);
  CharacterVector c(1);
  c[0]= test;
  CharacterVector c2(1);
  c2[0]= what;
  return (fun(in, c, c2));
}

extern "C" SEXP _rxode2et_expandPars_(SEXP objectSSEXP, SEXP paramsSSEXP, SEXP eventsSSEXP, SEXP controlSSEXP) {
  if (!rxode2random_loaded) {
    rxode2random_loaded = true;
    rxode2random = loadNamespace("rxode2random");
  }
  Function fun = as<Function>(rxode2random[".expandPars"]);
  return fun(objectSSEXP, paramsSSEXP, eventsSSEXP, controlSSEXP);
}


typedef SEXP (*_rxode2et_convertId_t_)(SEXP id);
_rxode2et_convertId_t_  _rxode2et__convertId_;
bool rxode2parse_loaded = false;
Environment rxode2parse;
extern "C" SEXP assignRxode2ParsePtrs__(void) {
  BEGIN_RCPP
    if (!rxode2parse_loaded) {
      rxode2parse_loaded = true;
      rxode2parse = loadNamespace("rxode2parse");
      Function funPtrs = rxode2parse[".rxode2parseFunPtrs"];
      List ptr = as<List>(funPtrs());
      _rxode2et__convertId_ = (_rxode2et_convertId_t_)(R_ExternalPtrAddr(ptr[0]));
      // _rxode2_etTransParseP=(_rxode2_etTransParse_type) (R_ExternalPtrAddr(ptr[2]));
      // _rxode2_chin=(_rxode2_chin_type) (R_ExternalPtrAddr(ptr[3]));
      // getForder=(_rxode2parse_getForder_type) (R_ExternalPtrAddr(ptr[4]));
      // useForder=(_rxode2parse_useForder_type) (R_ExternalPtrAddr(ptr[5]));
    }
  END_RCPP
    }

extern "C" SEXP _rxode2et_convertId_(SEXP id) {
  BEGIN_RCPP
    assignRxode2ParsePtrs__();
  return _rxode2et__convertId_(id);
  END_RCPP
    }
