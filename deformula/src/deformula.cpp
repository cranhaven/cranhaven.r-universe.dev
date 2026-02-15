#include "deformula.h"

using namespace Rcpp;

// [[Rcpp::export]]
List integrate_zero_to_inf(Function f, double zero, double reltol,
                           int startd, int maxiter) {
  deformula::DeformulaZeroToInf de;
  de.getWeight(f, zero, reltol, startd, maxiter);

  int n = de.getSize();
  NumericVector t(n);
  NumericVector x(n);
  NumericVector w(n);
  de.getTValue(t.begin(), t.end());
  de.getXValue(x.begin(), x.end());
  de.getWValue(w.begin(), w.end());
  double s = de.getSum();
  double h = de.getH();
  int info = de.getInfo();

  return List::create(
    Named("value") = s,
    Named("x") = x,
    Named("w") = w,
    Named("t") = t,
    Named("h") = h,
    Named("message") = info
  );
}

// [[Rcpp::export]]
List integrate_mone_to_one(Function f, double zero, double reltol,
                           int startd, int maxiter) {
  deformula::DeformulaMinusOneToOne de;
  de.getWeight(f, zero, reltol, startd, maxiter);

  int n = de.getSize();
  NumericVector t(n);
  NumericVector x(n);
  NumericVector w(n);
  de.getTValue(t.begin(), t.end());
  de.getXValue(x.begin(), x.end());
  de.getWValue(w.begin(), w.end());
  double s = de.getSum();
  double h = de.getH();
  int info = de.getInfo();

  return List::create(
    Named("value") = s,
    Named("x") = x,
    Named("w") = w,
    Named("t") = t,
    Named("h") = h,
    Named("message") = info
  );
}
