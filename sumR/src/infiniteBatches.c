#include "sumR_internal.h"
#include "math.h"

long double infiniteBatches_(long double logFun(long k, double *Theta),
                                 double *params, long batch_size, double eps,
                                 long maxIter, long n0, long* n)
{
  // Declaration
  long N;
  long double maxA, lEps = logl(eps),
    *logFunVal = R_Calloc((size_t)(maxIter + 1), long double),
    partial = 0., *checkStart = logFunVal,
    S = 0., lS, cc = 0., total = 0, test1, test2 = 0.;
  *n = 0;

  logFunVal[*n] = logFun(n0, params);
  while (!R_FINITE(logFunVal[*n]) && *n < (maxIter - 1))
    logFunVal[++*n] = logFun(++n0, params);

  // Find the maximum
  do
    logFunVal[++*n] = logFun(++n0, params);
  while (logFunVal[*n] >= logFunVal[*n - 1] && *n <= (maxIter - 1));

  // If too many iterations. Last iter is max.
  if (*n == maxIter)
  {
    partial_logSumExp(logFunVal, maxIter - 1, logFunVal[*n], &cc, 0, &total);
    long double result = logFunVal[*n] + log1pl(total);
    
    if (logFunVal != NULL) R_Free(logFunVal);
    return result;
  }

  // I know which is the max due to the stop criteria.
  // Assumed local max = global max.
  maxA = logFunVal[*n - 1];
  lEps -= maxA; // For the convergence checking
  while (*n % batch_size && *n < maxIter) // Complete the next batch.
    logFunVal[++*n] = logFun(++n0, params);
  N = ((*n - batch_size) / batch_size) * batch_size; // Second to last batch.
  partial_logSumExp(logFunVal, N, maxA, &cc, 0, &partial);
  checkStart += N + 1; // pointer displacement
  N = batch_size - 1; // How many iterations to the last batch.
  cc = 0.;
  partial_logSumExp(checkStart, N, maxA, &cc, 0, &S);
  test1 = logFunVal[*n] - logFunVal[*n - 1];
  lS = logl(S);

  // Calculate the tail. Only loop once.
  // test1 and test2 are required for correct convergence condition
  while ((lS > lEps || test2 - test1 > -log1pl(expl(test2 - lS))) &&
         *n < maxIter)
  {
    partial += S;
    for (N = 0, S = 0.; N < (batch_size - 2); N++)
      KahanSum(&S, exp(logFun(++n0, params) - maxA), &cc);
    test1 = logFun(++n0, params);
    KahanSum(&S, exp(test1 - maxA), &cc);
    test2 = logFun(++n0, params);
    KahanSum(&S, exp(test2 - maxA), &cc);
    *n += batch_size;
    lS = logl(S);
  }
  
  if (logFunVal != NULL) R_Free(logFunVal);
  return maxA + logl(partial + S);
}
