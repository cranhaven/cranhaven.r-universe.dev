#include <Rinternals.h>
#include "r_wrappers.h"

lFptr precompiled_selector(SEXP funS, double *logL,
                           double *params, int size){

  if compareStr("negbin_marginal"){
    checkSize(size, 4);
    if (params[0] <= 0)
      error("Parameter Mu must be positive.");
    if (params[1] <= 0)
      error("Parameter Phi must be positive.");
    if (params[2] <= 0 || params[2] >= 1)
      error("Parameter Eta must be in (0,1).");
    if (params[3] <= 0 || (int)params[3] != params[3])
      error("Parameter obsX must be an integer.");
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]) || !R_FINITE(params[2]) ||
        !R_FINITE(params[3])) error("Parameters must be finite.");
    *logL = log(params[0]) - Rf_logspace_add(log(params[0]), log(params[1])) +
      log1p(-params[2]);
    return negbin_marginal;
  }
  if compareStr("noObs"){
    checkSize(size, 1);
    if (!R_FINITE(params[0])) error("Parameter must be finite.");
    *logL = log1p(-params[0]);
    return noObs;
  }
  if compareStr("COMP"){
    checkSize(size, 2);
    if (params[0] <= 0 || params[1] <= 0)
      error("Parameters must be positive.");
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]))
      error("Parameters must be finite.");
    *logL = -INFINITY;
    return COMP;
  }
  if compareStr("dR0"){
    checkSize(size, 4);
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]) || !R_FINITE(params[2]) ||
        !R_FINITE(params[3])) error("Parameters must be finite.");
    *logL = log(params[0]) + log1p(-params[3]) +
      (1 + params[1]) * (log1p(params[1]) - log(params[0] + params[1]));
    return dR0;
  }
  if compareStr("powerLawDiff"){
    checkSize(size, 3);
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]) || !R_FINITE(params[2]))
      error("Parameters must be finite.");
    *logL = log(0.9999);
    return powerLawDiff;
  }
  if compareStr("negbin_sentinel"){
    checkSize(size, 3);
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]) || !R_FINITE(params[2]))
      error("Parameters must be finite.");
    *logL = log(params[0]) - Rf_logspace_add(log(params[0]), log(params[1])) +
      log1p(-params[2]);
    return negbin_sentinel;
  }
  if compareStr("poisson_sentinel"){
    checkSize(size, 2);
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]))
      error("Parameters must be finite.");
    *logL = -INFINITY;
    return poisson_sentinel;
  }
  if compareStr("weird_series_constL"){
    checkSize(size, 1);
    *logL = log(params[0]);
    return weird_series_constL;
  }
  if compareStr("weird_series"){
    checkSize(size, 1);
    if (!R_FINITE(params[0])) error("Parameter must be finite.");
    *logL = -1;
    return weird_series;
  }
  if compareStr("double_poisson"){
    checkSize(size, 2);
    if (params[0] <= 0 || params[1] <= 0)
      error("Parameters must be positive.");
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]))
      error("Parameters must be finite.");
    *logL = -INFINITY;
    return dbl_poisson;
  }
  if compareStr("bessel_I"){
    checkSize(size, 2);
    if (params[0] <= 0)
      error("Parameter x must be positive.");
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]))
      error("Parameters must be finite.");
    *logL = -INFINITY;
    return bessel_I;
  }
  if (compareStr("poisson_fact_moment")){
    checkSize(size, 2);
    if (params[0] <= 0)
      error("Parameter lambda must be positive.");
    if (params[1] <= 1)
      error("Parameter order must be larger than 1.");
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]))
      error("Parameters must be finite.");
    *logL = -INFINITY;
    return poisson_fact_moment;
  }
  if compareStr("bessel_I_logX"){
    checkSize(size, 2);
    if (!R_FINITE(params[0]) || !R_FINITE(params[1]))
      error("Parameters must be finite.");
    *logL = -INFINITY;
    return bessel_I_logX;
  }
  error("Compiled function not found.");
}

SEXP inf_sum(SEXP logFun, SEXP params, SEXP logL, SEXP alternating, SEXP eps,
             SEXP maxIter, SEXP n0, SEXP rho, SEXP forceAlgo)
{
  defineVar(install("Theta"), params, rho);
  long double out;
  long n;

  // Global variables declared in r_wrappers.h. They are used in translator
  envir = rho;
  lF = logFun;

  out = algorithm_selector(translator, REAL(params), INTEGER(alternating)[0],
                           REAL(eps)[0], INTEGER(maxIter)[0],
                           Rf_isNull(logL) ? 1. : REAL(logL)[0], INTEGER(n0)[0],
                           INTEGER(forceAlgo)[0], &n);

  return retFun((double)out, n);
}

SEXP infinite_sum_callPrecomp(SEXP lF, SEXP params, SEXP alternating, SEXP eps,
                              SEXP maxIter, SEXP n0, SEXP forceAlgo)
{
  long double out;
  long n;
  double lL;
  lFptr logFunction = precompiled_selector(lF, &lL, REAL(params),
                                           Rf_xlength(params));

  out = algorithm_selector(logFunction, REAL(params), INTEGER(alternating)[0],
                           REAL(eps)[0],INTEGER(maxIter)[0], lL,
                           INTEGER(n0)[0], INTEGER(forceAlgo)[0], &n);

  return retFun((double)out, n);
}

//////// batches wrappers

SEXP inf_batches(SEXP logFun, SEXP params, SEXP batch_size, SEXP eps,
                 SEXP maxIter, SEXP n0, SEXP rho)
{
  defineVar(install("Theta"), params, rho);
  long double out;
  long n;

  // Global variables declared in r_wrappers.h. They are used in translator
  envir = rho;
  lF = logFun;

  out = infiniteBatches_(translator, REAL(params),
                         INTEGER(batch_size)[0], REAL(eps)[0],
                         INTEGER(maxIter)[0], INTEGER(n0)[0], &n);

  return retFun((double)out, n);
}

SEXP infinite_batches_precomp(SEXP lF, SEXP params, SEXP batch_size,
                              SEXP epsilon, SEXP maxIter, SEXP n0)
{
  long double out;
  long n;
  double logL;

  out = infiniteBatches_(precompiled_selector(lF, &logL, REAL(params),
                                               Rf_xlength(params)),
                          REAL(params), INTEGER(batch_size)[0],
                          REAL(epsilon)[0], INTEGER(maxIter)[0],
                          INTEGER(n0)[0], &n);

  return retFun((double)out, n);
}

//////// fixed iterations wrappers

SEXP sum_n_times_precomp(SEXP lF, SEXP params, SEXP N, SEXP n0)
{
  long double out;
  long n = INTEGER(N)[0];
  double logL;

  out = sumNTimes_(precompiled_selector(lF, &logL, REAL(params),
                                        Rf_xlength(params)),
                   REAL(params), n, INTEGER(n0)[0]);

  return retFun((double)out, n);
}
