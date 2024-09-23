#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include "../inst/include/rxode2parse.h"

Rcpp::Function loadNamespaceQs("loadNamespace", R_BaseNamespace);
Rcpp::Environment qsNs;
Rcpp::Environment rxode2parseNs;
bool loadQsC = false;
bool loadRxode2parse = false;


static void loadQs() {
  if (!loadQsC) {
    qsNs = loadNamespaceQs("qs");
    loadQsC = true;
  }
  if (!loadRxode2parse) {
    rxode2parseNs = loadNamespaceQs("rxode2parse");
    loadRxode2parse=true;
  }
}

extern "C" SEXP getRxode2ParseDf(void) {
  loadQs();
  Rcpp::Function getTran = Rcpp::as<Rcpp::Function>(rxode2parseNs["rxode2parseGetTranslation"]);
  return getTran();
}

extern "C" SEXP getRxode2ParseDfBuiltin(void) {
  loadQs();
  Rcpp::Function getTran = Rcpp::as<Rcpp::Function>(rxode2parseNs["rxode2parseGetTranslationBuiltin"]);
  return getTran();
}

extern "C" SEXP getRxode2ParseGetPointerAssignment(void) {
  loadQs();
  Rcpp::Function getPtr = Rcpp::as<Rcpp::Function>(rxode2parseNs["rxode2parseGetPointerAssignment"]);
  return getPtr();
}

//[[Rcpp::export]]
Rcpp::CharacterVector rxQs(SEXP const x) {
  loadQs();
  Rcpp::Function base91_encode = Rcpp::as<Rcpp::Function>(qsNs["base91_encode"]);
  Rcpp::Function qserialize = Rcpp::as<Rcpp::Function>(qsNs["qserialize"]);
  return base91_encode(qserialize(x, Rcpp::CharacterVector::create("high"), Rcpp::CharacterVector::create("zstd"),
				    Rcpp::IntegerVector::create(22),
				    Rcpp::IntegerVector::create(15), Rcpp::LogicalVector::create(true)));
}

//[[Rcpp::export]]
SEXP rxQr(const std::string& encoded_string) {
  loadQs();
  Rcpp::Function base91_decode = Rcpp::as<Rcpp::Function>(qsNs["base91_decode"]);
  Rcpp::Function qdeserialize = Rcpp::as<Rcpp::Function>(qsNs["qdeserialize"]);
  return qdeserialize(base91_decode(Rcpp::wrap(encoded_string)), false, false);
}



int rxode2parseIsRstudioI = 0;

//[[Rcpp::export]]
SEXP rxode2parseSetRstudio(bool isRstudio=false){
  if (isRstudio) rxode2parseIsRstudioI=1;
  else rxode2parseIsRstudioI=0;
  return Rcpp::wrap(rxode2parseIsRstudioI);
}

extern "C" void setSilentErr(int silent);

//' Silence some of rxode2's C/C++ messages
//'
//' @param silent can be 0L "noisy"  or 1L "silent"
//'
//' @keywords internal
//' @return TRUE; called for side effects
//' @export
//[[Rcpp::export]]
bool rxParseSetSilentErr(int silent){
  setSilentErr(silent);
  return true;
}

//[[Rcpp::export]]
Rcpp::List rxUpdateTrans_(Rcpp::List ret, std::string prefix, std::string libName){
  Rcpp::CharacterVector oldTrans = Rcpp::as<Rcpp::CharacterVector>(ret["trans"]);
  Rcpp::CharacterVector newLst(22);
  Rcpp::CharacterVector newNme(22);
  newNme[0] ="lib.name";
  newLst[0] = libName;

  newNme[1] = "jac";
  newLst[1] = Rcpp::as<std::string>(oldTrans[1]);

  newNme[2] = "prefix";
  newLst[2] = prefix;

  newNme[3] = "dydt";
  newLst[3] = prefix + "dydt";

  newNme[4] = "calc_jac";
  newLst[4] = prefix + "calc_jac";

  newNme[5] = "calc_lhs";
  newLst[5] = prefix + "calc_lhs";

  newNme[6] = "model_vars";
  newLst[6] = prefix + "model_vars";

  newNme[7] = "theta";
  newLst[7] = prefix + "theta";

  newNme[8]="inis";
  newLst[8]= prefix + "inis";

  newNme[9]="dydt_lsoda";
  newLst[9]= prefix + "dydt_lsoda";

  newNme[10]="calc_jac_lsoda";
  newLst[10]= prefix + "calc_jac_lsoda";

  newNme[11]= "ode_solver_solvedata";
  newLst[11]= prefix + "ode_solver_solvedata";

  newNme[12] = "ode_solver_get_solvedata";
  newLst[12] = prefix+"ode_solver_get_solvedata";

  newNme[13]= "dydt_liblsoda";
  newLst[13]= prefix + "dydt_liblsoda";

  newNme[14]="F";
  newLst[14]= prefix + "F";

  newNme[15] ="Lag";
  newLst[15] = prefix + "Lag";

  newNme[16]= "Rate";
  newLst[16] = prefix + "Rate";

  newNme[17] ="Dur";
  newLst[17] = prefix + "Dur";

  newNme[18] = "mtime";
  newLst[18] = prefix + "mtime";

  newNme[19] = "assignFuns";
  newLst[19] = prefix + "assignFuns";

  newNme[20] = "ME";
  newLst[20] = prefix + "ME";

  newNme[21] = "IndF";
  newLst[21]= prefix + "IndF";

  newLst.attr("names") = newNme;

  ret[3] = newLst;

  return(ret);
}
