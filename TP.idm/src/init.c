#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
  Check these declarations against the C/Fortran source code.
*/
  
  /* .C calls */
  extern void WeightsKaplanMeierSort(void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"WeightsKaplanMeierSort", (DL_FUNC) &WeightsKaplanMeierSort, 5},
  {NULL, NULL, 0}
};

void R_init_TP_idm(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
