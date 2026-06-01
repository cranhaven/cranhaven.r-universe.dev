#include "sumR_internal.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_sumR(DllInfo *info)
{
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);

  /* used by external packages linking to internal sumR code from C */
  R_RegisterCCallable("sumR", "infiniteSum_", (DL_FUNC) &infiniteSum_);
  R_RegisterCCallable("sumR", "infiniteSumToThreshold_",
                      (DL_FUNC) &infiniteSumToThreshold_);
  R_RegisterCCallable("sumR", "infiniteErrorBoundingPairs_",
                      (DL_FUNC) &infiniteErrorBoundingPairs_);
  R_RegisterCCallable("sumR", "infiniteBatches_",
                      (DL_FUNC) &infiniteBatches_);
  R_RegisterCCallable("sumR", "sumNTimes_",
                      (DL_FUNC) &sumNTimes_);
}
