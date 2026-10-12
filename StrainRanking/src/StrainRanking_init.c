#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void C_test_p_value(void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"C_test_p_value", (DL_FUNC) &C_test_p_value, 6},
    {NULL, NULL, 0}
};

void R_init_StrainRanking(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
