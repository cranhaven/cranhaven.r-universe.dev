#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*

NOTE (2017-08-10, NJM)

This init.c file was built by package_native_routine_registration_skeleton().

It was a change requested by Kurt in rexpokit updates:

Registering the C++ and FORTRAN calls:
https://stat.ethz.ch/pipermail/r-devel/2017-February/073755.html

library(tools)
package_native_routine_registration_skeleton(dir="/GitHub/rexpokit")

*/


/*
  The following symbols/expressions for .NAME have been omitted

    R_dgpadm
    R_dgexpv
    R_dmexpv
    R_mydmexpv
    R_mydgexpv
    R_rexpokit_as_coo

  Most likely possible values need to be added below.
*/

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void wrapalldgexpv_(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

/* .Fortran calls */
extern void F77_NAME(itscale5)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"wrapalldgexpv_", (DL_FUNC) &wrapalldgexpv_, 18},
    {NULL, NULL, 0}
};

static const R_FortranMethodDef FortranEntries[] = {
    {"itscale5", (DL_FUNC) &F77_NAME(itscale5), 10},
    {NULL, NULL, 0}
};

void R_init_rexpokit(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


