#ifndef __SUMR_PRECOMPILED_H__
#define __SUMR_PRECOMPILED_H__
#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

long double negbin_marginal(long k, double *Theta);

long double noObs(long k, double *Theta);

long double COMP(long k, double *Theta);

long double dR0(long k, double *Theta);

long double powerLawDiff(long k, double *Theta);

long double negbin_sentinel(long k, double *Theta);

long double poisson_sentinel(long k, double *Theta);

long double weird_series_constL(long k, double *Theta);

long double weird_series(long k, double *Theta);

long double dbl_poisson(long k, double *Theta);

long double bessel_I(long k, double *Theta);

long double poisson_fact_moment(long k, double *Theta);

long double bessel_I_logX(long k, double *Theta);

#ifdef __cplusplus
}
#endif

#endif
