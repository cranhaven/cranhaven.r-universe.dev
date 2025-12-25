#include <R_ext/Applic.h>
#include "optim.h"

void optim(
    int n, double *Bvec, double *X, double *Fmin, optfunc fn,
    int *fail, double abstol, double intol, void *ex,
    double alpha, double bet, double gamm, int trace,
    int *fncount, int maxit){
  return nmmin(n, Bvec, X, Fmin, fn, fail, abstol, intol, ex, alpha, bet,
               gamm, trace, fncount, maxit);
}
