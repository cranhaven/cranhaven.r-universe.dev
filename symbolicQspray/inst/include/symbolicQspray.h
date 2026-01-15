#ifndef ___SYMBOLICQSPRAY___
#define ___SYMBOLICQSPRAY___

#include "ratioOfQsprays.h"
using namespace RATIOOFQSPRAYS;

typedef Qspray<RatioOfQsprays<gmpq>>     SymbolicQspray;
typedef Polynomial<RatioOfQsprays<gmpq>> symbolicPolynomial;

// -------------------------------------------------------------------------- //
namespace SYMBOLICQSPRAY {

  // ------------------------------------------------------------------------ //
  static inline Rcpp::List returnSymbolicQspray(SymbolicQspray SQ) { // to return a list to R
    symbolicPolynomial S = SQ.get();
    if(S.size() == 0) {
      return Rcpp::List::create(Rcpp::Named("powers") = R_NilValue,
                                Rcpp::Named("coeffs") = R_NilValue);
    } else {
      Rcpp::List Powers(S.size());
      powers pows;
      unsigned int row = 0, col = 0;
      Rcpp::List Coeffs(S.size());
      unsigned int i = 0;
      for(auto it = S.begin(); it != S.end(); ++it) {
        pows = it->first;
        Rcpp::IntegerVector Exponents(pows.size());
        col = 0;
        for(auto ci = pows.begin(); ci != pows.end(); ++ci) {
          Exponents(col++) = *ci;
        }
        Powers(row++) = Exponents;
        Coeffs(i++) = returnRatioOfQsprays(it->second);
      }
      return Rcpp::List::create(Rcpp::Named("powers") = Powers,
                                Rcpp::Named("coeffs") = Coeffs);
    }
  }

  // ------------------------------------------------------------------------ //
  static inline SymbolicQspray makeSymbolicQspray(
      const Rcpp::List& Powers, const Rcpp::List& Coeffs
  ) {
    symbolicPolynomial S;
    int n = Powers.size();
    for(int i = 0; i < n; i++) {
      Rcpp::IntegerVector Exponents = Powers(i);
      powers pows(Exponents.begin(), Exponents.end());
      Rcpp::List coeff = Coeffs(i);
      Rcpp::List numerator   = coeff["numerator"];
      Rcpp::List denominator = coeff["denominator"];
      S[pows] = makeRatioOfQsprays(numerator, denominator);
    }
    return SymbolicQspray(S);
  }

}


#endif
