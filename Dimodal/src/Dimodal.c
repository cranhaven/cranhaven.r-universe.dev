
/*****
      Dimodal.c -
      Top level interface to C implementations of feature detectors and test
      functions.  Registers functions in other source files.

      Shared library for R, not an executable.

      c 2025 Greg Kreider, Primordial Machine Vision Systems, Inc.
*****/


/**
   To Do:
   -
**/

#include <R_ext/Rdynload.h>

#include "detectors.h"
#include "sigtests.h"
#include "utility.h"


/**** R Registration ****/

static const R_CallMethodDef callMethods[] = {
	/* Functions with an R interface. */
	{"C_find_runs", (DL_FUNC) &C_find_runs, 2},
	{"C_find_peaks", (DL_FUNC) &C_find_peaks, 5},
	{"C_find_flats", (DL_FUNC) &C_find_flats, 6},
	{"C_find_level_sections", (DL_FUNC) &C_find_level_sections, 3},
	{"C_test_runlen", (DL_FUNC) &C_test_runlen, 4},
	/* Internal unctions available via .Call, without an R wrapper. */
	{"C_altperm_symbols", (DL_FUNC) &C_altperm_symbols, 1},
	{"C_excursion", (DL_FUNC) &C_excursion, 5},
	{"C_htprob", (DL_FUNC) &C_htprob, 3},
	{"C_midq", (DL_FUNC) &C_midq, 3},
	{"C_eval_midq", (DL_FUNC) &C_eval_midq, 3},
	{"bist_flats", (DL_FUNC) &bist_flats, 0},
	{NULL, NULL, 0}
};

void R_init_Dimodal(DllInfo *info) {
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
}
