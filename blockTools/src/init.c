#include <R.h>
#include <Rinternals.h>
#include "naive.h"
#include "optgreed.h"

// Define a struct for registering routines
static const R_CMethodDef CMethods[] = {
  {"optgreed_c", (DL_FUNC) &optgreed_c, 17},
  {"naive_c", (DL_FUNC) &naive_c, 17},  
  {NULL, NULL, 0}  // Terminating entry
};

void R_init_blockTools(DllInfo *dll) {
  // Register routines
  R_registerRoutines(dll, CMethods, NULL, NULL, NULL);
  // Use the following if you have no routines to register
  R_useDynamicSymbols(dll, FALSE);
}
