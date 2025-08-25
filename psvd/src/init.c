#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "psvd.h"

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

const static R_CallMethodDef R_CallDef[] = {
  CALLDEF(mGS, 3),
  CALLDEF(eigenV, 6),
  {NULL, NULL, 0}
};

void R_init_psvd(DllInfo *dll)  {
     R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);   
     R_useDynamicSymbols(dll, FALSE);
     R_forceSymbols(dll, TRUE);
}
