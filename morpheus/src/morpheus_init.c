#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void hungarianAlgorithm(void*, void*, void*);
extern void Moments_M2(void*, void*, void*, void*, void*);
extern void Moments_M3(void*, void*, void*, void*, void*);
extern void Compute_Omega(void*, void*, void*, void*, void*, void*, void*);

static const R_CMethodDef CEntries[] = {
  {"hungarianAlgorithm", (DL_FUNC) &hungarianAlgorithm, 3},
  {"Moments_M2",         (DL_FUNC) &Moments_M2,         5},
  {"Moments_M3",         (DL_FUNC) &Moments_M3,         5},
  {"Compute_Omega",      (DL_FUNC) &Compute_Omega,      7},
  {NULL, NULL, 0}
};

void R_init_morpheus(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

