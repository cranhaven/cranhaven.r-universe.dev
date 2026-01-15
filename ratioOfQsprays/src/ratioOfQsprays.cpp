#include "ratioOfQsprays.h"

using namespace RATIOOFQSPRAYS;

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List ROQaddition(
  const Rcpp::List& Numerator1, const Rcpp::List& Denominator1,
  const Rcpp::List& Numerator2, const Rcpp::List& Denominator2
) {
  RatioOfQsprays<gmpq> ROQ1 = makeRatioOfQsprays(Numerator1, Denominator1);
  RatioOfQsprays<gmpq> ROQ2 = makeRatioOfQsprays(Numerator2, Denominator2);
  return returnRatioOfQsprays(ROQ1 + ROQ2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List ROQsubtraction(
  const Rcpp::List& Numerator1, const Rcpp::List& Denominator1,
  const Rcpp::List& Numerator2, const Rcpp::List& Denominator2
) {
  RatioOfQsprays<gmpq> ROQ1 = makeRatioOfQsprays(Numerator1, Denominator1);
  RatioOfQsprays<gmpq> ROQ2 = makeRatioOfQsprays(Numerator2, Denominator2);
  return returnRatioOfQsprays(ROQ1 - ROQ2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List ROQmultiplication(
  const Rcpp::List& Numerator1, const Rcpp::List& Denominator1,
  const Rcpp::List& Numerator2, const Rcpp::List& Denominator2
) {
  RatioOfQsprays<gmpq> ROQ1 = makeRatioOfQsprays(Numerator1, Denominator1);
  RatioOfQsprays<gmpq> ROQ2 = makeRatioOfQsprays(Numerator2, Denominator2);
  return returnRatioOfQsprays(ROQ1 * ROQ2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List ROQdivision(
  const Rcpp::List& Numerator1, const Rcpp::List& Denominator1,
  const Rcpp::List& Numerator2, const Rcpp::List& Denominator2
) {
  RatioOfQsprays<gmpq> ROQ1 = makeRatioOfQsprays(Numerator1, Denominator1);
  RatioOfQsprays<gmpq> ROQ2 = makeRatioOfQsprays(Numerator2, Denominator2);
  return returnRatioOfQsprays(ROQ1 / ROQ2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List ROQpower(
  const Rcpp::List& Numerator, const Rcpp::List& Denominator, int n
) {
  RatioOfQsprays<gmpq> ROQ = makeRatioOfQsprays(Numerator, Denominator);
  return returnRatioOfQsprays(ROQ.power(n));
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
bool ROQequality(
  const Rcpp::List& Numerator1, const Rcpp::List& Denominator1,
  const Rcpp::List& Numerator2, const Rcpp::List& Denominator2
) {
  RatioOfQsprays<gmpq> ROQ1 = makeRatioOfQsprays(Numerator1, Denominator1);
  RatioOfQsprays<gmpq> ROQ2 = makeRatioOfQsprays(Numerator2, Denominator2);
  return ROQ1 == ROQ2;
}
