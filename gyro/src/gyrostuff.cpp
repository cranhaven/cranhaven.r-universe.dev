#ifndef _GYROHEADER_
#include "gyro.h"
#endif

double sqnorm(const DVector v) {
  double x = std::inner_product(v.begin(), v.end(), v.begin(), 0.0);
  return x;
}

double dotprod(const DVector u, const DVector v) {
  double x = std::inner_product(u.begin(), u.end(), v.begin(), 0.0);
  return x;
}

// [[Rcpp::export]]
DVector Mgyroadd_cpp(const DVector X, const DVector Y, const double s) {
  double s2 = s * s;
  double x = sqnorm(X) / s2;
  double y = sqnorm(Y) / s2;
  double xy = 2.0 * dotprod(X, Y) / s2;
  return ((1.0 + xy + y) * X + (1.0 - x) * Y) / (1.0 + xy + x * y);
}

// [[Rcpp::export]]
DVector Mgyroscalar_cpp(const double r, const DVector X, const double s) {
  const double Xnorm = sqrt(sqnorm(X));
  return s / Xnorm * tanh(r * atanh(Xnorm / s)) * X;
}

// [[Rcpp::export]]
DVector MgyroABt_cpp(const DVector A,
                     const DVector B,
                     const double t,
                     const double s) {
  const DVector AB = Mgyroadd_cpp(-A, B, s);
  const DVector tAB = Mgyroscalar_cpp(t, AB, s);
  return Mgyroadd_cpp(A, tAB, s);
}

// [[Rcpp::export]]
DMatrix Mgyrosegment_cpp(const DVector A,
                         const DVector B,
                         const double s,
                         const size_t n) {
  const size_t d = A.size();
  DMatrix Segment(d, n);
  const double step = 1.0 / (n - 1);
  double t = 0.0;
  for(size_t i = 0; i < n; i++) {
    const DVector pt = MgyroABt_cpp(A, B, t, s);
    Segment(Rcpp::_, i) = pt;
    t += step;
  }
  return Segment;
}
