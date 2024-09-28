#include <ming.h>

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* This function will be called when the package is loaded,
   hence called only once. */
SEXP swfInit(void)
{
    /* Some global initializations */
    Ming_init();
    /* Setting small value leads to more accurate curves */
    Ming_setCubicThreshold(1);
    /* Setting the SWF version */
    Ming_useSWFVersion(8);
    /* Setting the compression level, 1 low ==> 9 high */
    Ming_setSWFCompression(9);
    /* Setting the warning and error handler. The followings will
       use R to handle warnings and errors. */
    Ming_setWarnFunction(Rf_warning);
    Ming_setErrorFunction(Rf_error);

    return R_NilValue;
}
