#ifndef __SUMR_RWRAPPERS_H__
#define __SUMR_RWRAPPERS_H__

#include <Rinternals.h>
#include "sumR_internal.h"
#include "precompiled.h"

double Rf_logspace_add(double, double);

// Quality of life macros
#define compareStr(s) (!strcmp(CHAR(STRING_PTR_RO(funS)[0]), s))
#define arraysize(a) sizeof(a) / sizeof(double)
#define checkSize(s, n) if (s != n) error("Wrong number of parameters. Expected %d, found %d.\n", n, s)

typedef long double (*lFptr)(long, double*);

// Function evaluation
static inline double feval(SEXP lF, SEXP rho)
{return REAL(eval(lF, rho))[0];}

// Function return macro
static inline SEXP retFun(double res, long mI)
{
  SEXP out = PROTECT(allocVector(VECSXP,2));
  SET_VECTOR_ELT(out, 0, Rf_ScalarReal(res));
  SET_VECTOR_ELT(out, 1, Rf_ScalarInteger(mI));

  SEXP nms = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(nms, 0, mkChar("sum"));
  SET_STRING_ELT(nms, 1, mkChar("n"));

  /* assign names to list */
  setAttrib(out, R_NamesSymbol, nms);

  UNPROTECT(2);
  return out;
}

// Needed global var
SEXP envir, lF;

// Wrapping sums for functions defined at the R level
static inline long double translator(long k, double *Theta)
{
  defineVar(install("k"), PROTECT(Rf_ScalarInteger(k)), envir);
  UNPROTECT(1);
  return (long double)feval(lF, envir);
}

// Selectors
static inline long double algorithm_selector(lFptr logF, double *params,
                                             int alternating, double eps,
                                             long mI, double lL, long n0,
                                             int selector, long *n)
{
  if (selector == 0 || alternating)
    return infiniteSum_(logF, params, lL, alternating, eps, mI, n0, n);
  else if (selector == 1)
    return infiniteSumToThreshold_(logF, params, alternating, eps, mI, n0, n);
  else if (selector == 2)
    return infiniteErrorBoundingPairs_(logF, params, lL, eps, mI, n0, n);
  else if (selector == 3)
    return infiniteBatches_(logF, params, 40, eps, mI, n0, n);
  else
    error("Invalid forceAlgorithm argument.\n");
}

#endif
