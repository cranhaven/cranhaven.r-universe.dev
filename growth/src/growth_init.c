#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(back)(void *, void *);
extern void F77_NAME(kalman)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(plra)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(resid)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(resid2)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(roots)(void *, void *, void *);
extern void F77_NAME(ttvert)(void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"back",   (DL_FUNC) &F77_NAME(back),    2},
    {"kalman", (DL_FUNC) &F77_NAME(kalman), 25},
    {"plra",   (DL_FUNC) &F77_NAME(plra),   43},
    {"resid",  (DL_FUNC) &F77_NAME(resid),  24},
    {"resid2", (DL_FUNC) &F77_NAME(resid2), 16},
    {"roots",  (DL_FUNC) &F77_NAME(roots),   3},
    {"ttvert", (DL_FUNC) &F77_NAME(ttvert),  2},
    {NULL, NULL, 0}
};

void R_init_growth(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
