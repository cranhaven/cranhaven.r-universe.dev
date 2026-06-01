#include "sumR_internal.h"
#include "math.h"

long double infiniteSum_(
    long double logFun(long k, double *Theta),
    double *params, double logL, int alternating, double eps,
    long maxIter, long n0, long* n){
  if (logL < - LOG_2 || alternating)
    return infiniteSumToThreshold_(logFun, params, alternating, eps, maxIter, n0, n);
  else if (logL < 0)
    return infiniteErrorBoundingPairs_(logFun, params, logL, eps, maxIter, n0, n);
  else
    return infiniteBatches_(logFun, params, 40, eps, maxIter, n0, n);
}
