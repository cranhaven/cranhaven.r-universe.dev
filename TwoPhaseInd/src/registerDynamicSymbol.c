// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
# include <R_ext/Error.h>
#include <R_ext/Rdynload.h>

void R_init_TwoPahseInd(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
