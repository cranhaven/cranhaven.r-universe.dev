#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Complex.h>

extern "C" {
    void F77_NAME(classocd_warm)(Rcomplex* x, Rcomplex* y, int* n, int* p, double* lambda, Rcomplex* b0, Rcomplex* b);
    void F77_NAME(classocd_warm_screen)(Rcomplex* x, Rcomplex* y, int* n, int* p, double* lambda, double* lambda0, Rcomplex* b0, Rcomplex* b);
    void F77_NAME(cglassocd_noscale)(Rcomplex* s, int* p, double* lambda, Rcomplex* theta, Rcomplex* w, Rcomplex* w0, int* w_init, int* maxiter, double* tol, int* h, int* final_cycle);
    void F77_NAME(cglassocd_scaled)(Rcomplex* s, int* p, double* lambda, Rcomplex* theta, Rcomplex* w, Rcomplex* w0, int* w_init, int* maxiter, double* tol, int* h, int* final_cycle);
}

static const R_FortranMethodDef FortranEntries[] = {
    // name      pointer      Num args
    {"classocd_warm", (DL_FUNC)&F77_NAME(classocd_warm),        7},
    {"classocd_warm_screen", (DL_FUNC)&F77_NAME(classocd_warm_screen),        8},
    {"cglassocd_noscale", (DL_FUNC)&F77_NAME(cglassocd_noscale),        11},
    {"cglassocd_scaled", (DL_FUNC)&F77_NAME(cglassocd_scaled),        11},
    {NULL   ,             NULL,        0}   // Placeholder(?) to indicate last one.
};

void R_init_cxreg(DllInfo* info) {
    R_registerRoutines(
        info,       // DllInfo
        NULL,      // .C
        NULL,      // .Call
        FortranEntries,      // Fortran
        NULL       // External
    );
    R_useDynamicSymbols(info, TRUE);
}
