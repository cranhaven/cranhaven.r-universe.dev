#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void condKendallC(void *, void *, void *, void *, void *, void *, void *);
extern void pmccC(void *, void *, void *, void *);
extern void uCondKendall(void *, void *, void *, void *);
extern void wKendallC(void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"condKendallC", (DL_FUNC) &condKendallC, 7},
    {"pmccC",        (DL_FUNC) &pmccC,        4},
    {"uCondKendall", (DL_FUNC) &uCondKendall, 4},
    {"wKendallC",    (DL_FUNC) &wKendallC,    6},
    {NULL, NULL, 0}
};

void R_init_tranSurv(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

