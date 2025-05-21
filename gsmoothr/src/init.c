
#include "R_ext/Rdynload.h"
#include "tmeantest.h"

static const R_CMethodDef cMethods[]  = {
  {"tmean",(DL_FUNC)&tmean,7},
  {NULL, NULL, 0}
};

void R_init_flagme(DllInfo *info){
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
}
