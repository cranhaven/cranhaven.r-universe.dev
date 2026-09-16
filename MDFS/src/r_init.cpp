#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#include "r_interface.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef callMethods[]  = {
  CALLDEF(r_compute_max_ig, 13),
  CALLDEF(r_compute_max_ig_discrete, 10),
  CALLDEF(r_compute_all_matching_tuples, 15),
  CALLDEF(r_compute_all_matching_tuples_discrete, 11),
  CALLDEF(r_discretize, 6),
  CALLDEF(r_omp_set_num_threads, 1),
  {NULL, NULL, 0}
};

extern "C"
void attribute_visible R_init_MDFS(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
