#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void survForest(void *, void *, void *, void *, void *, void *, void *, void *,
                       void *, void *, void *, void *, void *, void *, void *, void *,
                       void *, void *, void *, void *, void *, void *);
extern void survRF(void *, void *, void *, void *, void *, void *, void *, void *,
                   void *, void *, void *, void *, void *, void *, void *, void *,
                   void *, void *, void *, void *, void *, void *, void *, void *,
                   void *, void *, void *, void *, void *, void *, void *, void *,
                   void *, void *, void *, void *, void *, void *, void *, void *,
                   void *, void *, void *, void *, void *, void *, void *, void *,
                   void *, void *, void *, void *, void *, void *, void *, void *,
                   void *, void *, void *, void *);
extern void survError(void *, void *, void *, void *, void *, void *, void *, void *,
                void *, void *, void *, void *, void *);

void ksmoothProb (void *, void *, void *, void *, void *, void *, void *, void *,
                  void *, void *);

static const R_CMethodDef CEntries[] = {
  {"survForest",   (DL_FUNC) &survForest,   22},
  {"survRF",       (DL_FUNC) &survRF,       60},
  {"survError",    (DL_FUNC) &survError,    13},
  {"ksmoothProb",  (DL_FUNC) &ksmoothProb,  10},
  {NULL, NULL, 0}
};

void R_init_icrf(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
