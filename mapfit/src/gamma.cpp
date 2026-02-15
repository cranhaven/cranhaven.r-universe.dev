
#include "gamma.h"
#include <cmath>

namespace gam {

double lgamma(double x) {
  double v, w;
  v = 1;
  while (x<N) { v *=x; x++; }
  w = 1 / (x * x);
  return ((((((((B16 / (16 * 15)) * w + (B14 / (14 * 13))) * w
                 + (B12 / (12 * 11))) * w + (B10 / (10 * 9))) * w
               + (B8 / (8 * 7))) * w + (B6 / (6 * 5))) * w
               + (B4 / (4 * 3))) * w + (B2 / (2 * 1))) / x
               + 0.5 * LOG_2PI - log(v) - x + (x - 0.5) * log(x);
}

double tgamma(double x) {
  if (x < 0) {
    return PI / (sin(PI * x) * exp(lgamma(1-x)));
  }
  return exp(lgamma(x));
}


double psi(double x) {
  double v, w;
  v = 0;
  while (x < N) { v += 1 / x; x++; }
  w = 1 / (x * x);
  v += ((((((((B16 / 16) * w + (B14 /14)) * w
               + (B12 / 12)) * w + (B10 / 10)) * w
             + (B8 / 8)) * w + (B6 / 6)) * w
             + (B4 / 4)) * w + (B2 / 2)) * w + 0.5 / x;
             return log(x) - v;
}

double polygamma(int n, double x) {
  int k;
  double t, u, v, w;
  u = 1;
  for(k=1-n; k<0; k++) u *= k;
  v = 0;
  while (x<N) { v +=1 / pow(x, n+1); x++; }
  w = x * x;
  t = (((((((B16
               * (n + 15.0) * (n + 14) / (16 * 15 * w) + B14)
              * (n + 13.0) * (n + 12) / (14 * 13 * w) + B12)
              * (n + 11.0) * (n + 10) / (12 * 11 * w) + B10)
              * (n + 9.0) * (n + 8) / (10 * 9 * w) + B8)
              * (n + 7.0) * (n + 6) / (8 * 7 * w) + B6)
              * (n + 5.0) * (n + 4) / (6 * 5 * w) + B4)
              * (n + 3.0) * (n + 2) / (4 * 3 * w) + B2)
              * (n + 1.0) * n / (2 * 1 * w)
              + 0.5 * n / x + 1;
              return u * (t / pow(x, n) + n * v);
}

double p_gamma(double a, double x, double loggamma_a) {
  int k;
  double result, term, previous;
  if (x >= 1+a) return 1 - q_gamma(a, x, loggamma_a);
  if (x == 0)   return 0;
  result = term = exp(a * log(x) - x - loggamma_a) / a;
  for (k=1; k<1000; k++) {
    term *= x / (a+k);
    previous = result;
    result += term;
    if (result == previous) return result;
  }
  return result;
}

double q_gamma(double a, double x, double loggamma_a) {
  int k;
  double result, w, temp, previous;
  double la, lb;
  la = 1; lb = 1 + x - a;
  if (x < 1+a) return 1 - p_gamma(a, x, loggamma_a);
  w = exp(a * log(x) - x - loggamma_a);
  result = w/lb;
  for (k=2; k<1000; k++) {
    temp = ((k-1-a)*(lb-la)+(k+x)*lb)/k;
    la = lb;
    lb = temp;
    w *= (k-1-a)/k;
    temp = w/(la*lb);
    previous = result;
    result += temp;
    if (result == previous) return result;
  }
  return result;
}

}
