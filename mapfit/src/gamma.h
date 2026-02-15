#ifndef MAPFIT_GAMMA_H
#define MAPFIT_GAMMA_H

#include <cmath>

namespace gam {

static int FACTMAX = 20;
static double PI = 3.14159265358979324; // pi
static double LOG_2PI = 1.83787706640934548;
static double LOG_PI = 1.14472988584940017; // log(pi)
static int ERL_MAX = 10;

static int N = 8;
//static double B0 = 1;
//static double B1 = (-1.0 / 2.0);
static double B2 = ( 1.0 / 6.0);
static double B4 = (-1.0 / 30.0);
static double B6 = ( 1.0 / 42.0);
static double B8 = (-1.0 / 30.0);
static double B10 = ( 5.0 / 66.0);
static double B12 = (-691.0 / 2730.0);
static double B14 = ( 7.0 / 6.0);
static double B16 = (-3617.0 / 510.0);

static double nfact[] = {
  1.0,                        // 0
  1.0,                        // 1
  2.0,                        // 2
  6.0,                        // 3
  24.0,                       // 4
  120.0,                      // 5
  720.0,                      // 6
  5040.0,                     // 7
  40320.0,                    // 8
  362880.0,                   // 9
  3628800.0,                  // 10
  39916800.0,                 // 11
  479001600.0,                // 12
  6227020800.0,               // 13
  87178291200.0,              // 14
  1307674368000.0,            // 15
  20922789888000.0,           // 16
  355687428096000.0,          // 17
  6402373705728000.0,         // 18
  121645100408832000.0,       // 19
  2432902008176640000.0       // 20
};

static double lognfact[] = {
  0.0,
  0.0,
  0.6931471805599453,
  1.791759469228055,
  3.1780538303479458,
  4.787491742782046,
  6.579251212010101,
  8.525161361065415,
  10.60460290274525,
  12.801827480081469,
  15.104412573075516,
  17.502307845873887,
  19.987214495661885,
  22.552163853123425,
  25.19122118273868,
  27.89927138384089,
  30.671860106080672,
  33.50507345013689,
  36.39544520803305,
  39.339884187199495,
  42.335616460753485
};

double lgamma(double x);
double tgamma(double x);
double psi(double x);
double polygamma(int n, double x);

inline
double tfact(int s) {
  if (s <= FACTMAX) {
    return nfact[s];
  } else {
    return exp(lgamma(1.0 + s));
  }
}

inline
double lfact(int s) {
  if (s <= FACTMAX) {
    return lognfact[s];
  } else {
    return lgamma(1.0 + s);
  }
}

double p_gamma(double a, double x, double loggamma_a);
double q_gamma(double a, double x, double loggamma_a);

inline
double erlang_pdf(int a, double b, double t) {
  return b * pow(b*t, a-1) * exp(-b * t) / tfact(a-1);
}

inline
double erlang_lpdf(int a, double b, double t) {
  return a * log(b) + (a-1) * log(t) - b * t - lfact(a-1);
}

inline
double erlang_cdf(int a, double b, double t) {
  if (a <= ERL_MAX) {
    double w = 1.0;
    for (int i=1; i<=a-1; i++) {
      w += pow(b*t, i) / tfact(i);
    }
    return 1.0 - exp(-b*t) * w;
  } else {
    return p_gamma(a+1.0, b*t, lgamma(a+1.0));
  }
}

inline
double erlang_ccdf(int a, double b, double t) {
  if (a <= ERL_MAX) {
    double w = 1.0;
    for (int i=1; i<=a-1; i++) {
      w += pow(b*t, i) / tfact(i);
    }
    return exp(-b*t) * w;
  } else {
    return q_gamma(a+1.0, b*t, lgamma(a+1.0));
  }
}

}

#endif
