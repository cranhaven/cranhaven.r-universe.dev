#include <Rinternals.h>
#include "math.h"

void partial_logSumExp(long double* fun, long evals, long double maxA,
                       long double* c, int backwards, long double* res)
{
  
  /*if (backwards)
    for (R_xlen_t i = evals; i >= 0; i--)
      KahanSum(res, expl(fun[i] - maxA), c);
  else
    for (R_xlen_t i = 0; i <= evals; i++)
      KahanSum(res, expl(fun[i] - maxA), c);
   */
  
  // This algorithm fixes some floating point precision loss
  long double leaveNoOneBehind, cc = 0.;
  long i;
  
  // Go backwards if the series is decreasing and forward otherwise.
  if (backwards)
  {
    i = evals;
    while (i >= 0)
    {
      leaveNoOneBehind = expl(fun[i--] - maxA);
      while (leaveNoOneBehind < *res && i >= 0)
        KahanSum(&leaveNoOneBehind, expl(fun[i--] - maxA), &cc);
      KahanSum(res, leaveNoOneBehind, c);
    }
  }
  else
  {
    i = 0;
    while (i <= evals)
    {
      leaveNoOneBehind = expl(fun[i++] - maxA);
      while (leaveNoOneBehind < *res && i <= evals)
        KahanSum(&leaveNoOneBehind, expl(fun[i++] - maxA), &cc);
      KahanSum(res, leaveNoOneBehind, c);
    }
  }
}

void partial_logSumExp_alternate(long double* fun, long evals, long double maxA,
                       int backwards, long double* res, int* alt)
{
  // Go backwards if the series is decreasing and forward otherwise.
  if (backwards){
    *alt *= (evals % 2 ? -1 : 1);
    for (R_xlen_t i = evals; i >= 0; i--){
      *res += *alt * expl(fun[i] - maxA);
      *alt *= -1;
      }
    }
  else
    for (R_xlen_t i = 0; i <= evals; i++){
      *res += *alt * expl(fun[i] - maxA);
      *alt *= -1;
      }
}
