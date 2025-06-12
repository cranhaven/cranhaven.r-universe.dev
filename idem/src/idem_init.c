#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void bsort(void *, void *, void *, void *, void *);
extern void kdpdf(void *, void *, void *, void *, void *);
extern void rankall(void *, void *, void *, void *, void *, void *, void *, void *);
extern void rankij(void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"bsort",   (DL_FUNC) &bsort,   5},
  {"kdpdf",   (DL_FUNC) &kdpdf,   5},
  {"rankall", (DL_FUNC) &rankall, 8},
  {"rankij",  (DL_FUNC) &rankij,  8},
  {NULL, NULL, 0}
};

void R_init_idem(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, TRUE);
}
