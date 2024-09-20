// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "defines.h"
#include <algorithm>
#include <rxode2parse.h>
#include <rxode2parse_control.h>
//#include "../inst/include/rxode2.h"
#include "timsort.h"
#include "needSortDefines.h"
#define SORT gfx::timsort
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )
#define isSameTime(xout, xp) ((xout)-(xp) <= DBL_EPSILON*max2(fabs(xout),fabs(xp)))

extern "C" bool _rxode2et_qtest(SEXP in, const char *test);
#include "rxode2et_as.h"

#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2et", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

#define rxModelVars(a) rxModelVars_(a)
using namespace Rcpp;
#include "checkmate.h"

void RSprintf(const char *format, ...);

Environment rxode2etenv();

Function getRxFn(std::string name, const char* err);

List _rxode2et_rxModelVars_(SEXP obj) {
	if (Rf_inherits(obj, "rxModelVars")) {
		return as<List>(obj);
	}
	Function fn = getRxFn("rxModelVars_", "need 'rxode2' for more complete model variable calculation");
	return as<List>(fn(obj));
}

Environment dataTable;
bool getForder_b=false;
Function getRxEtFn(std::string name);
bool dtForder = false;
bool forderForceBase_ = false;

//' Force using base order for rxode2 radix sorting
//'
//' @param forceBase boolean indicating if rxode2 should use R's
//'   [order()] for radix sorting instead of
//'   `data.table`'s parallel radix sorting.
//'
//' @return NILL; called for side effects
//'
//' @examples
//' \donttest{
//' forderForceBase(TRUE) # Use base `order` for rxode2 sorts
//' forderForceBase(FALSE) # Use `data.table` for rxode2 sorts
//' }
//' @export
//' @keywords internal
//[[Rcpp::export]]
RObject forderForceBase(bool forceBase = false){
  forderForceBase_=forceBase;
  return R_NilValue;
}

IntegerVector convertDvid_(SEXP inCmt, int maxDvid=0){
  IntegerVector id = asIv(inCmt, "inCmt");
  IntegerVector udvid = sort_unique(id);
  int mDvid = udvid[udvid.size()-1];
  if (mDvid > maxDvid) {
    return Rcpp::match(id, udvid);
  }
  return id;
}


Function getForder(){
  if (!getForder_b){
    Function fn = getRxEtFn(".getDTEnv");
    dataTable = fn();
    getForder_b=true;
  }
  if (!forderForceBase_ && dataTable.exists("forder")){
    dtForder=true;
    return dataTable["forder"];
  }
  Environment b=Rcpp::Environment::base_namespace();
  dtForder=false;
  return b["order"];
}

extern "C" SEXP _rxode2et_getForder() {
	return wrap(getForder());
}


Function getChin() {
  if (!getForder_b){
    Function fn = getRxEtFn(".getDTEnv");
    dataTable = fn();
    getForder_b=true;
  }
  if (!forderForceBase_ && dataTable.exists("%chin%")){
    return dataTable["%chin%"];
  }
  Environment b=Rcpp::Environment::base_namespace();
  return b["%in%"];
}

extern "C" SEXP _rxode2et_chin(SEXP x, SEXP table) {
  Function chin_ = getChin();
  return chin_(x, table);
}

extern bool useForder(){
  return getForder_b;
}

extern "C" SEXP _rxode2et_useForder(void) {
	return wrap(getForder_b);
}
