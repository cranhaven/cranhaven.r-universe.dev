#ifndef __SUMR_API_H__
#define __SUMR_API_H__

#include <sumR_internal.h>		// also includes R.h and Rinternals.h

#include <Rconfig.h>
#include <R_ext/Rdynload.h>

#ifdef __cplusplus
extern "C" {
#endif
  
  // infiniteSum auto select
  long double infiniteSum(
      long double logFun(long k, double *Theta),
      double *params, double logL, int alternating, double eps,
      long maxIter, long n0, long* n) {
    static long double (*fun)(long double (long k, double *Theta),
                        double*, double, int, double, long, long,
                        long*) = NULL;
    fun = (long double(*)(long double (long k, double *Theta),
                       double*, double, int, double, long, long,
                       long*))
      R_GetCCallable("sumR", "infiniteSum_");
    return fun(logFun, params, logL, alternating, eps, maxIter, n0, n);
  }
  
  // Sum-To-Threshold algorithm
  long double infiniteSumToThreshold(
      long double logFun(long k, double *Theta),
      double *params, int alternating, double eps,
      long maxIter, long n0, long* n) {
    static long double (*fun)(long double (long k, double *Theta),
                        double*, int, double, long, long,
                        long*) = NULL;
    fun = (long double(*)(long double (long k, double *Theta),
                       double*, int, double, long, long,
                       long*))
      R_GetCCallable("sumR", "infiniteSumToThreshold_");
    return fun(logFun, params, alternating, eps, maxIter, n0, n);
  }
  
  // Adaptive Truncation algorithm
  long double infiniteErrorBoundingPairs(
      long double logFun(long k, double *Theta),
      double *params, double logL, double eps,
      long maxIter, long n0, long* n) {
    static long double (*fun)(long double (long k, double *Theta),
                        double*, double, double, long, long,
                        long*) = NULL;
    fun = (long double(*)(long double (long k, double *Theta),
                       double*, double, double, long, long,
                       long*))
      R_GetCCallable("sumR", "infiniteErrorBoundingPairs_");
    return fun(logFun, params, logL, eps, maxIter, n0, n);
  }
  
  // c-folding algorithm
  long double infiniteBatches(
      long double logFun(long k, double *Theta),
      double *params, long batch_size, double eps,
      long maxIter, long n0, long* n) {
    static long double (*fun)(long double (long k, double *Theta),
                        double*, long, double, long, long,
                        long*) = NULL;
    fun = (long double(*)(long double (long k, double *Theta),
                       double*, long, double, long, long,
                       long*))
      R_GetCCallable("sumR", "infiniteBatches_");
    return fun(logFun, params, batch_size, eps, maxIter, n0, n);
  }
  
  // sum N times
  long double sumNTimes(
      long double logFun(long k, double *Theta),
      double *params, long n, long n0) {
    static long double (*fun)(long double (long k, double *Theta),
                        double*, long, long) = NULL;
    fun = (long double(*)(long double (long k, double *Theta),
                       double*, long, long))
      R_GetCCallable("sumR", "sumNTimes_");
    return fun(logFun, params, n, n0);
  }
  
#ifdef __cplusplus
}
#endif

#endif
