/* Reinhard Furrer,  fall 2019, based on spam's version and 'Writing R extensions'. */

#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern void pks2               ( void *, void *);


/* to get all functions:
   nm -g ./PRSim.so | grep " T "
*/



/* .Call calls */

static const R_CallMethodDef CallEntries[] = {
  {"pks2", (DL_FUNC) &pks2, 2},
  {NULL, NULL, 0}
};


void R_init_PRSim(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

