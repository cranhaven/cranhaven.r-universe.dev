#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern "C" {

extern void MVR_km_clustering(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void MVR_withinsumsq(void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"MVR_km_clustering", (DL_FUNC) &MVR_km_clustering, 14},
    {"MVR_withinsumsq",   (DL_FUNC) &MVR_withinsumsq,    8},
    {NULL, NULL, 0}
};

void R_init_MVR(DllInfo *dll) {
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

}
