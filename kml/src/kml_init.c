#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void affecteIndiv(void *, void *, void *, void *, void *, void *);
extern void calculMean(void *, void *, void *, void *, void *, void *);
extern void kml1(void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"affecteIndiv", (DL_FUNC) &affecteIndiv, 6},
    {"calculMean",   (DL_FUNC) &calculMean,   6},
    {"kml1",         (DL_FUNC) &kml1,         7},
    {NULL, NULL, 0}
};

void R_init_kml(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

