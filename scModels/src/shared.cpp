#include <Rcpp.h>
#include "shared.h"

bool validKummerParameters(double a, double b, bool warn) {
  if(a < 0) {
    if(warn) {
      Rcpp::warning("Parameter a is less than zero: %f\n", a);
    }
    return false;
  }
  if(b < a) {
    if(warn) {
      Rcpp::warning("Wrong parameters: b cannot be less than a: %f < %f\n", b, a);
    }
    return false;
  }
  return true;
}

bool isInteger(double x, bool warn) {
  if (ISNAN(x))
    return false;
  if (((x < 0.0) ? std::ceil(x) : std::floor(x)) != x) {
    if (warn) {
      Rcpp::warning("Non-integer: %f\n", x);
    }
    return false;
  }
  return true;
}

bool validProbability(double p, bool warn) {
  if (p >= 0.0 && p <= 1.0) {
    return true;
  } else {
    if(warn) {
      Rcpp::warning("Invalid probability: %f\n", p);
    }
    return false;
  }
}

bool isInadmissible(double x, bool warn) {
  if(Rcpp::NumericVector::is_na(x) || Rcpp::traits::is_nan<REALSXP>(x)) {
    if(warn)
      Rcpp::warning("NA/NaNs given in input\n");
    return true;
  } else {
    return false;
  }
}

bool validPbParameters(double alpha, double beta, double c, bool warn) {
  if(alpha > 0 && beta > 0 && c > 0) {
    if(warn) {
      Rcpp::warning("Negative parameters for mpb\n");
    }
    return true;
  } else {
    return false;
  }
}

void reportGslError(int status) {
  Rcpp::warning("GSL Error #%d occured\n", status);
}
