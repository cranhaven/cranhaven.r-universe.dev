#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(pic2wt)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(pmi)(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"pic2wt", (DL_FUNC) &F77_NAME(pic2wt), 9},
    {"pmi",    (DL_FUNC) &F77_NAME(pmi),    9},
    {NULL, NULL, 0}
};

void R_init_NPRED(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
