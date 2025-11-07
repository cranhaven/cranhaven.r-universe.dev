#include <R_ext/RS.h> // F77_SUB
#include "pnorm.h"

double F77_SUB(mvphi)(double *x){ return pnorm_std(*x, 1, 0); }
