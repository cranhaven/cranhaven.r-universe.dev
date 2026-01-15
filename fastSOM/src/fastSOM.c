#include <R_ext/Rdynload.h>
#include <R_ext/Applic.h>
#include <R_ext/Callbacks.h>

// Helpers.c
SEXP solve_sym(SEXP S, SEXP B, SEXP N, SEXP M);
SEXP Nminus1(SEXP N);
SEXP scaleSigma(SEXP S, SEXP D, SEXP N);
SEXP scaleA(SEXP A, SEXP D, SEXP N, SEXP H);

// SOM.c
SEXP array_stuff(SEXP B, SEXP A, SEXP S, SEXP N1, SEXP N2, SEXP H);
SEXP matrix_stuff(SEXP A, SEXP B, SEXP C, SEXP N1, SEXP N2, SEXP N3);
SEXP paste_together(SEXP Res1, SEXP Res2, SEXP N, SEXP N1, SEXP N2, SEXP Combs, SEXP Ncombs);
SEXP trALsquared_perms(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perms, SEXP Nperms, SEXP FirstPerm);
SEXP trALplusBLinv_squared_perms(SEXP S, SEXP A, SEXP B, SEXP N, SEXP H, SEXP Perms, SEXP Nperms);
SEXP trALplusBLinv_squared(SEXP S, SEXP A, SEXP B, SEXP N, SEXP H, SEXP Perm);
SEXP ALsquared_perms(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perms, SEXP Nperms);
SEXP ALsquared(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perm);
SEXP trALsquared(SEXP S, SEXP A, SEXP N, SEXP H, SEXP Perm);
SEXP SOT_avg(SEXP S, SEXP A, SEXP N, SEXP H, SEXP NcK, SEXP Cumpos, SEXP Gensets, SEXP NminusOne);
SEXP fev(SEXP S, SEXP A, SEXP N, SEXP H);

static R_CallMethodDef callMethods[] = {                                        // Prepare registering native C routines
  {"solve_sym", (DL_FUNC) &solve_sym, 4},
  {"Nminus1", (DL_FUNC) &Nminus1, 1},
  {"scaleSigma", (DL_FUNC) &scaleSigma, 3},
  {"scaleA", (DL_FUNC) &scaleA, 4},
  {"array_stuff", (DL_FUNC) &array_stuff, 6},
  {"matrix_stuff", (DL_FUNC) &matrix_stuff, 6},
  {"paste_together", (DL_FUNC) &paste_together, 7},
  {"trALsquared", (DL_FUNC) &trALsquared, 5},
  {"trALsquared_perms", (DL_FUNC) &trALsquared_perms, 7},
  {"ALsquared", (DL_FUNC) &ALsquared, 5},
  {"ALsquared_perms", (DL_FUNC) &ALsquared_perms, 6},
  {"trALplusBLinv_squared_perms", (DL_FUNC) &trALplusBLinv_squared_perms, 7},
  {"trALplusBLinv_squared", (DL_FUNC) &trALplusBLinv_squared, 6},
  {"SOT_avg", (DL_FUNC) &SOT_avg, 8},
  {"fev", (DL_FUNC) &fev, 4},
  {NULL, NULL, 0}
};

void R_init_fastSOM(DllInfo *info) {                                            // Register native C routines
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
