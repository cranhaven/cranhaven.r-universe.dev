#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void slim_dantzig_ladm_scr(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void slim_dantzig_ladm_scr2(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"slim_dantzig_ladm_scr",  (DL_FUNC) &slim_dantzig_ladm_scr,  21},
    {"slim_dantzig_ladm_scr2", (DL_FUNC) &slim_dantzig_ladm_scr2, 18},
    {NULL, NULL, 0}
};

void R_init_freebird(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
