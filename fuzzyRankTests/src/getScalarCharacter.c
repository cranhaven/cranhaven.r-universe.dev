
#include <R.h>
#include <Rinternals.h>
#include "myutil.h"

const char *getScalarCharacter(SEXP foo)
{
    if (! isString(foo))
        error("argument must be character");
    if (LENGTH(foo) != 1)
        error("argument must be scalar");
    return CHAR(STRING_ELT(foo, 0));
}

