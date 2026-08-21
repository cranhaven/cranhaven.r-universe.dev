#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Fortran calls */
extern void F77_NAME(mainfunction)(double *Y, double *X, int *Measure, int *MaxSize, int *MaxDepth, int *MinNodeSize, double *MinH, int *AlgoType, double *BoundH, int *LookAheadDepth, double *Prior, double *LossM, int *MY, int *NY, int *MX, int *NX, double *TAll, double *hAll, double *h, double *Tv, int *TreeTableSize, int *NTrees, int *warn, int *NMaxNodes, int *XType);

static const R_FortranMethodDef FortranEntries[] = {
  {"mainfunction", (DL_FUNC) &F77_NAME(mainfunction), 25},
  {NULL, NULL, 0}
};

void R_init_ETrees(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
