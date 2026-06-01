#include "sumR_internal.h"
#include "math.h"

long double infiniteSumToThreshold_(long double logFun(long k, double *Theta),
                              double *params, int alternating, double eps,
                              long maxIter, long n0, long* n)
{
  // Declaration
  long nMax;
  int alt = alternating;
  long double maxA, lEps = logl(eps), total = 0., totalBack = 0., c = 0.,
    cb = 0., *logFunVal = R_Calloc((size_t)(maxIter + 1), long double);
  *n = 0;
  
  // Finding function max. Only check convergence after max is reached
  logFunVal[*n] = logFun(n0, params);
  while (!R_FINITE(logFunVal[*n]))
    logFunVal[++*n] = logFun(++n0, params);

  do
    logFunVal[++*n] = logFun(++n0, params);
  while (logFunVal[*n] >= logFunVal[*n - 1] && *n <= (maxIter - 1));
  
  // If too many iterations. Last iter is max.
  if (*n == maxIter)
  {
    if (alternating)
      partial_logSumExp_alternate(logFunVal, maxIter - 1, logFunVal[*n], 0,
                                  &total, &alt);
    else
      partial_logSumExp(logFunVal, maxIter - 1, logFunVal[*n], &c, 0, &total);
    long double result = logFunVal[*n] + log1pl(total);
    
    if (logFunVal != NULL) R_Free(logFunVal);
    return result;
  }
  
  // I know which is the max due to the stop criteria.
  // Assumed local max = global max.
  maxA = logFunVal[*n - 1];
  nMax = *n;
  if (*n > 1){
    if (alternating)
      partial_logSumExp_alternate(logFunVal, *n - 2, maxA, 0,
                                  &total, &alt);
    else
      partial_logSumExp(logFunVal, *n - 2, maxA, &c, 0, &total);
  }
  
  // Calculate the tail
  do
    logFunVal[++*n] = logFun(++n0, params);
  while (logFunVal[*n] >= lEps && (*n <= (maxIter - 1)));
  if (alternating)
    partial_logSumExp_alternate(&logFunVal[nMax - 1], *n - nMax + 1, maxA, 1,
                                &totalBack, &alt);
  else
    partial_logSumExp(&logFunVal[nMax], *n - nMax, maxA, &cb, 1, &totalBack);
  
  if (logFunVal != NULL) R_Free(logFunVal);
  if (alternating)
    return maxA + logl(total + totalBack);
  else
    return maxA + log1pl(total + totalBack);
}
