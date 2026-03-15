#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* How was this made?   JPS 4/2018
Within the top level of the package source:
tools::package_native_routine_registration_skeleton(".")  ## or if in different directory, give package source dir path
Then copy all text results to packagename_init.c

Then add to NAMESPACE file: useDynLib(packagename, .registration=TRUE)

*/

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void checkIntMax(void *);
extern void groupsum(void *, void *, void *, void *, void *);
extern void haplo_em_pin(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void haplo_em_ret_info(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void haplo_free_memory(void);
extern void louis_info(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void seqhapC(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"checkIntMax",       (DL_FUNC) &checkIntMax,        1},
    {"groupsum",          (DL_FUNC) &groupsum,           5},
    {"haplo_em_pin",      (DL_FUNC) &haplo_em_pin,      21},
    {"haplo_em_ret_info", (DL_FUNC) &haplo_em_ret_info, 10},
    {"haplo_free_memory", (DL_FUNC) &haplo_free_memory,  0},
    {"louis_info",        (DL_FUNC) &louis_info,        15},
    {"seqhapC",           (DL_FUNC) &seqhapC,           31},
    {NULL, NULL, 0}
};

void R_init_haplo_stats(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
