//
//  EntropyMCMC_init.c
//  
//
//  Created by Didier Chauveau on 31/01/2019.
//  output of package_native_routine_registration_skeleton(".")
//  from tools package

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .C calls */
extern void entropyNNC(void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"entropyNNC", (DL_FUNC) &entropyNNC, 5},
    {NULL, NULL, 0}
};

void R_init_EntropyMCMC(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
