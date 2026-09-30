
#include <R.h>
#include <Rinternals.h>
#include "myutil.h"

int getScalarInteger(SEXP foo)
{
    if (! isNumeric(foo))
        error("argument must be numeric");
    if (LENGTH(foo) != 1)
        error("argument must be scalar");
    if (isInteger(foo)) {
        return INTEGER(foo)[0];
    } else {
        SEXP bar = coerceVector(foo, INTSXP);
        return INTEGER(bar)[0];
    }
}

