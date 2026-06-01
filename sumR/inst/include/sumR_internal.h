#ifndef __SUMR_H__
#define __SUMR_H__

#include <R.h>
#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

// Approximates infinite sum with an adaptive truncation
long double infiniteErrorBoundingPairs_(long double (long k, double *Theta), double*,
                                        double, double, long, long, long*);

// Approximates infinite sum by summing until added value is smaller than threshold
long double infiniteSumToThreshold_(long double logFun(long k, double *Theta),
                        double*, int, double, long, long, long*);

// Approximates infinite sum by summing in batches until the batch addes up to
// less than the desired margin
long double infiniteBatches_(long double logFun(long k, double *Theta),
                              double*, long, double, long, long, long*);

// Dispatches infiniteAdaptive or inviniteSumToThreshold based on logL value
long double infiniteSum_(
    long double logFun(long k, double *Theta),
    double *params, double logL, int alternating, double eps,
    long maxIter, long n0, long* n);

// Sum N times with no convergence checking
long double sumNTimes_(
    long double logFun(long k, double *Theta),
    double *params, long N, long n0);

#ifdef __cplusplus
}
#endif

#endif
