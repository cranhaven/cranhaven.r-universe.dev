#include <R_ext/Rdynload.h>
#include "simest.h"

static R_NativePrimitiveArgType derivcvxpec_t[] = {
    INTSXP, REALSXP, REALSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType cpen_t[] = {
    INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    INTSXP, REALSXP, REALSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType predcvxpen_t[] = {
    INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType spen_egcv_t[] = {
    INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, REALSXP
};

static R_NativePrimitiveArgType penta_t[] = {
    INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, REALSXP
};

static const R_CMethodDef cMethods[] = {
   {"derivcvxpec", (DL_FUNC) &derivcvxpec, 5, derivcvxpec_t},
   {"cpen", (DL_FUNC) &cpen, 16, cpen_t},
   {"predcvxpen", (DL_FUNC) &predcvxpen, 11, predcvxpen_t},
   {"spen_egcv", (DL_FUNC) &spen_egcv, 11, spen_egcv_t},
   {"penta", (DL_FUNC) &penta, 8, penta_t},
   {NULL, NULL, 0}
};

void R_init_simest(DllInfo *dll)
{
	R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
}
