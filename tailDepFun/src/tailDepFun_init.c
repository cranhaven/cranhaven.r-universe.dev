#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void alphaGeneral(void *, void *, void *, void *, void *, void *, void *);
extern void ecGeneral(void *, void *, void *, void *, void *, void *);
extern void ellsmith2(void *, void *, void *);
extern void I3smith2(void *, void *, void *);
extern void stdf(void *, void *, void *, void *, void *);
extern void tildeLGeneral(void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"alphaGeneral",  (DL_FUNC) &alphaGeneral,  7},
    {"ecGeneral",     (DL_FUNC) &ecGeneral,     6},
    {"ellsmith2",     (DL_FUNC) &ellsmith2,     3},
    {"I3smith2",      (DL_FUNC) &I3smith2,      3},
    {"stdf",          (DL_FUNC) &stdf,          5},
    {"tildeLGeneral", (DL_FUNC) &tildeLGeneral, 7},
    {NULL, NULL, 0}
};

void R_init_tailDepFun(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
