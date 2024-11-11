#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void covSetUp(void *, void *, void *, void *, void *, void *, void *, void *);
extern void loglikelihood(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void mixModelSetUp(void *, void *, void *, void *, void *, void *, void *, void *);
extern void multiCovSetUp(void *, void *);
extern void multiDataSetUp(void *, void *);
extern void ngCovSetUp(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void ngDataSetUp(void *, void *, void *, void *, void *, void *, void *, void *, void *);
// extern void npsolc(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void posteriors(void *, void *, void *, void *, void *, void *, void *);
extern void setCrit(void *);

static const R_CMethodDef CEntries[] = {
    {"covSetUp",       (DL_FUNC) &covSetUp,        8},
    {"loglikelihood",  (DL_FUNC) &loglikelihood,  13},
    {"mixModelSetUp",  (DL_FUNC) &mixModelSetUp,   8},
    {"multiCovSetUp",  (DL_FUNC) &multiCovSetUp,   2},
    {"multiDataSetUp", (DL_FUNC) &multiDataSetUp,  2},
    {"ngCovSetUp",     (DL_FUNC) &ngCovSetUp,      9},
    {"ngDataSetUp",    (DL_FUNC) &ngDataSetUp,     9},
//    {"npsolc",         (DL_FUNC) &npsolc,         28},
    {"posteriors",     (DL_FUNC) &posteriors,      7},
    {"setCrit",        (DL_FUNC) &setCrit,         1},
    {NULL, NULL, 0}
};

void R_init_depmix(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
