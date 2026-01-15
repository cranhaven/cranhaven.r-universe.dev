#include "symbolicQspray.h"

using namespace SYMBOLICQSPRAY;

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_add(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return returnSymbolicQspray(sqspray1 + sqspray2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_subtract(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return returnSymbolicQspray(sqspray1 - sqspray2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_multiply(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return returnSymbolicQspray(sqspray1 * sqspray2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_power(
    const Rcpp::List& Powers, const Rcpp::List& Coeffs, unsigned int n
) {
  SymbolicQspray sqspray = makeSymbolicQspray(Powers, Coeffs);
  return returnSymbolicQspray(sqspray.power(n));
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
bool SymbolicQspray_equality(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return sqspray1 == sqspray2;
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_deriv(
    const Rcpp::List& Powers, const Rcpp::List& Coeffs, 
    const Rcpp::IntegerVector& n
) {
  SymbolicQspray sqspray = makeSymbolicQspray(Powers, Coeffs);
  std::vector<unsigned int> orders(n.begin(), n.end());
  SymbolicQspray sqsprayPrime = sqspray.deriv(orders);
  return returnSymbolicQspray(sqsprayPrime);
}
