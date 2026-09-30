
#include <R.h>
#include <Rinternals.h>
#include "myutil.h"

double getScalarReal(SEXP foo)
{
    if (! isNumeric(foo))
        error("argument must be numeric");
    if (LENGTH(foo) != 1)
        error("argument must be scalar");
    if (isReal(foo)) {
        return REAL(foo)[0];
    } else {
        SEXP bar = coerceVector(foo, REALSXP);
        return REAL(bar)[0];
    }
}

