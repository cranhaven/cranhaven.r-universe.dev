
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "fuzzyranktests.h"

static R_CMethodDef cMethods[] = {
    {NULL, NULL, 0, NULL}
};

static R_CallMethodDef callMethods[]  = {
    {"fpvranksum", (DL_FUNC) &fpvranksum, 5},
    {"fpvsign", (DL_FUNC) &fpvsign, 4},
    {"fpvsignrank", (DL_FUNC) &fpvsignrank, 4},
    {NULL, NULL, 0}
};

void attribute_visible R_init_fuzzyRankTests(DllInfo *info)
{
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}

