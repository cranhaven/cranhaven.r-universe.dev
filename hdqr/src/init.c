#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(lqr_hd)(double *alpha, double *lam2, double *hval, 
  int *nobs, int *nvars, double *x, double *y, double *tau, int *jd, int *pfncol, 
  double *pf, double *pf2, int *dfmax, int *pmax, int *nlam, double *flmin, 
  double *ulam, double *eps, int *isd, int *maxit, int *nalam, 
  double *b0, double *beta, int *ibeta, int *nbeta, double *alam, 
  int *npass, int *jerr, double *sigma, int *is_exact);


static const R_FortranMethodDef FortranEntries[] = {
    {"lqr_hd",         (DL_FUNC) &F77_SUB(lqr_hd),         30},
    {NULL, NULL, 0}
};

void R_init_hdqr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
