
#ifndef FUZZYRANKTESTS_MYUTIL_H
#define FUZZYRANKTESTS_MYUTIL_H

#include <R.h>
#include <Rinternals.h>

int getScalarInteger(SEXP foo);

double getScalarReal(SEXP foo);

int isAllFinite(SEXP foo);

const char *getScalarCharacter(SEXP foo);

#endif /* FUZZYRANKTESTS_MYUTIL_H */

