//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "../inst/include/rxode2random.h"
#include <Rcpp.h>
#include "checkmate.h"

using namespace Rcpp;

Rcpp::Function loadNamespaceCheckmate("loadNamespace", R_BaseNamespace);
Rcpp::Environment checkmateNs;
bool loadCheckmateNs = false;

static void loadCheckmate() {
  if (!loadCheckmateNs) {
    checkmateNs = loadNamespaceCheckmate("checkmate");
    loadCheckmateNs = true;
  }
}

extern "C" bool qtest(SEXP in, const char *test) {
  loadCheckmate();
  Rcpp::Function _qtest = as<Function>(checkmateNs["qtest"]);
  SEXP testSXP = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(testSXP, 0, Rf_mkChar(test));
  bool ret = as<bool>(_qtest(in, testSXP));
  UNPROTECT(1);
  return ret;
}

extern "C" SEXP _rxode2random_qtest_sexp(SEXP in, SEXP test) {
  LogicalVector ret(1);
  ret[0] = qtest(in, CHAR(STRING_ELT(test, 0)));
  return wrap(ret);
}

// Modified by Matt
extern "C" SEXP qstrictS(SEXP nn, const char *what) {
  BEGIN_RCPP
  R_xlen_t pos = find_missing_string(nn);
  if (pos > 0) {
    stop("Must have %s, but is NA at position %i", what, pos);
  }
  if (Rf_isNull(nn)) {
    stop("Must have %s", what);
  }
  pos = Rf_any_duplicated(nn, FALSE);
  if (pos > 0){
    stop("Must have unique %s, but element %i is duplicated", what, pos);
  }
  pos = check_strict_names(nn);
  if (pos > 0){
    stop("Must have %s according to R's variable naming conventions, but element %i does not comply", what, pos);
  }
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP _rxode2random_qstrictS_sexp(SEXP in, SEXP test) {
  BEGIN_RCPP
  return qstrictS(in, CHAR(STRING_ELT(test, 0)));
  END_RCPP
}


extern "C" SEXP qstrictSn(SEXP x_, const char *what) {
  BEGIN_RCPP
  RObject x  = as<RObject>(x_);
  return qstrictS(as<SEXP>(x.attr("names")), what);
  END_RCPP
}

extern "C" SEXP _rxode2random_qstrictSn_sexp(SEXP in, SEXP test) {
  return qstrictSn(in, CHAR(STRING_ELT(test, 0)));
}


extern "C" SEXP qstrictSdn(SEXP x_, const char *what) {
  BEGIN_RCPP
  RObject x  = as<RObject>(x_);
  List dimnames = x.attr("dimnames");
  if (Rf_isNull(dimnames)) {
    stop("'%s' matrix must be named", what);
  }
  SEXP names;
  if (Rf_isNull(as<SEXP>(dimnames[1]))) {
    names = dimnames[0];
  } else {
    names = dimnames[1];
  }
  return qstrictS(as<SEXP>(names), what);
  END_RCPP
}

extern "C" SEXP _rxode2random_qstrictSdn_sexp(SEXP in, SEXP test) {
  return qstrictSdn(in, CHAR(STRING_ELT(test, 0)));
}

extern "C" SEXP qassertS(SEXP in, const char *test, const char *what) {
  BEGIN_RCPP
  if (!qtest(as<SEXP>(in), test)) {
    char ch1 = tolower(test[0]);
    std::string what0 = std::string(what);
    std::string err = "'" + what0 + "' must be class '";
    if (ch1 == 'r') {
      err += "double";
    } else if (ch1 == 'b') {
      err += "boolean"; 
    } else if (ch1 == 'x') {
      err += "integer";
    } else if (ch1 == 'm') {
      err += "matrix";
    } else if (ch1 == 'n') {
      err += "numeric";
    }
    err += "' ";
    int i = 1;
    int including = 0;
    int startedLen = 0;
    while (test[i] != 0) {
      if (test[i] >= '1' && test[i] <= '9') {
	if (startedLen){
	} else {
	  err.append("of length ");
	  startedLen=1;
	}
	err.append(1, test[i]);
      } else {
	if (test[i] == '[' || test[i] == '(') {
	  including = 1;
	  err.append(" with range ");
	  err.append(1,test[i]);
	} else if (including) {
	  err.append(1, test[i]);
	  if (test[i] == ',' && test[i+1] == ')') {
	    err += "Inf";
	  }
	}
      }
      i++;
    }
    // err.append("test: '");
    // err.append(test);
    // err.append("'");
    Rcpp::stop(err);
  }
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP _rxode2random_qassertS_sexp(SEXP in, SEXP test, SEXP w) {
  return qassertS(in, CHAR(STRING_ELT(test, 0)), CHAR(STRING_ELT(w, 0)));
}

