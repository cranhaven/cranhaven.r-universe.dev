#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Declare your C routines here */
extern void blasso(double *Sin, double *Min, double *Omin, int *pin, int *qin,
                   double *lamin, double *tol, int *maxit,
                   double *Bout, int *warm);

/* Register routines */
static const R_CMethodDef CEntries[] = {
  {"blasso", (DL_FUNC) &blasso, 10},
  {NULL, NULL, 0}
};

void R_init_SparseTSCGM(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

