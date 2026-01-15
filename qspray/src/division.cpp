#include "qspray.h"

using namespace QSPRAY;

// [[Rcpp::export]]
Rcpp::List qsprayDivisionRcpp(
  Rcpp::List Powers1, Rcpp::StringVector coeffs1,
  Rcpp::List Powers2, Rcpp::StringVector coeffs2
) {
  Qspray<gmpq> p = makeQspray(Powers1, coeffs1);
  Qspray<gmpq> g = makeQspray(Powers2, coeffs2);
  std::pair<Qspray<gmpq>,Qspray<gmpq>> QR = qsprayDivision(p, g);
  return Rcpp::List::create(
    Rcpp::Named("Q") = returnQspray(QR.first),
    Rcpp::Named("R") = returnQspray(QR.second)
  );
}

// [[Rcpp::export]]
Rcpp::List BBdivisionRcpp(
  Rcpp::List Powers, Rcpp::StringVector coeffs,
  Rcpp::List gs, Rcpp::List LTgs, int d
) {
  int ngs = gs.size();
  Qspray<gmpq> p(makeQspray(Powers, coeffs));
  Qspray<gmpq> r;
  int i;
  bool divoccured;
  while(!p.empty()) {
    i = 0;
    divoccured = false;
    Rcpp::List LTp = QSPRAY::internal::leadingTerm(p, d);
    while(i < ngs && !divoccured) {
      Rcpp::List LTg = LTgs(i);
      if(QSPRAY::internal::divides(LTg, LTp)) {
        Rcpp::List g = gs(i);
        Qspray<gmpq> gspray(makeQspray(g["powers"], g["coeffs"]));
        Qspray<gmpq> qtnt = QSPRAY::internal::quotient(LTp, LTg);
        p -= qtnt * gspray;
        divoccured = true;
      } else {
        i++;
      }
    }
    if(!divoccured) {
      Rcpp::IntegerVector powsRcpp = LTp["powers"];
      std::string coeff = LTp["coeff"];
      gmpq coef(coeff);
      powers pows(powsRcpp.begin(), powsRcpp.end());
      QSPRAY::utils::simplifyPowers(pows);
      Polynomial<gmpq> LTpspray;
      LTpspray[pows] = coef;
      Qspray<gmpq> ltp(LTpspray);
      r += ltp;
      p -= ltp;
    }
  }
  return returnQspray(r);
}

