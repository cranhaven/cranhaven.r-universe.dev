#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "precompiled.h"
#include "math.h"

double Rf_dnbinom(double x, double size, double prob, int give_log);
double Rf_dbinom(double x, double n, double p, int give_log);
double Rf_dpois(double x, double lambda, int give_log);

long double negbin_marginal(long k, double *Theta)
{
  double x = Theta[3], s = Theta[1];
  return (k < x ? -INFINITY :
            Rf_dnbinom(k, s, s / (s + Theta[0]), 1) +
              Rf_dbinom(x, k, Theta[2], 1));
}

long double noObs(long k, double *Theta)
{return k * log1pl(-Theta[0]);}

long double COMP(long k, double *Theta)
{return k * logl(Theta[0]) - Theta[1] * lgammal(k + 1);}

long double dR0(long k, double *Theta)
{
  double x = Theta[2];
  if (k == 0 || k < x) {return -INFINITY;}
  else
  {
    long double wy = Theta[1] * k, wyym1 = wy + k - 1;
    return lgammal(wyym1) - (lgammal(wy) + lgammal(k + 1)) +
      ((k-1) * (logl(Theta[0]) - logl(Theta[1])) - wyym1 *
      log1pl(Theta[0] / Theta[1])) +
                  Rf_dbinom(x, k, Theta[3], 1);
  }
}

long double powerLawDiff(long k, double *Theta)
{return (k < Theta[1] ? -INFINITY :
           -Theta[0] * logl(k) + Rf_logspace_sub(0,
                           -Theta[2] - Theta[3] * (k - Theta[1])));}

long double negbin_sentinel(long k, double *Theta)
{
 return Rf_dnbinom(k, Theta[1], Theta[1]/ (Theta[1] + Theta[0]), 1) +
   k * log1pl(-Theta[2]);
}

long double poisson_sentinel(long k, double *Theta)
{
  return Rf_dpois(k, Theta[0], 1) +
    k * log1pl(-Theta[1]);
}

long double weird_series_constL(long k, double *Theta)
{return (k == 0 ? -INFINITY : -(2 * logl(k) + k * logl(Theta[0])));}

long double weird_series(long k, double *Theta)
{return (k == 0 ? -INFINITY : lgammal(k + 1) - k * logl(k));}

long double dbl_poisson(long k, double *Theta)
{
  return k == 0 ? 0.5 * logl(Theta[1]) - Theta[0] * Theta[1]:
  0.5 * logl(Theta[1]) - (Theta[0] - k) * Theta[1] - k +
    k * (1 - Theta[1]) * logl(k) - lgammal(k + 1) + Theta[1] * k * logl(Theta[0]);
}

long double bessel_I(long k, double *Theta)
{
  return (2 * k + Theta[1]) * (logl(Theta[0]) - LOG_2) -
    lgammal(k + 1) - lgammal(Theta[1] + k + 1);
}

long double poisson_fact_moment(long k, double *Theta) {
  if (k < Theta[1]) return -INFINITY;
  return Rf_dpois(k, Theta[0], 1) +
    lgammal(k + 1) - lgammal(k - Theta[1] + 1);
}

long double bessel_I_logX(long k, double *Theta)
{
  return (2 * k + Theta[1]) * (Theta[0] - LOG_2) -
    lgammal(k + 1) - lgammal(Theta[1] + k + 1);
}
