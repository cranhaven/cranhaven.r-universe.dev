/* Here to avoid `warning: conflicts with previous declaration` with BLAS
 * functions defined differently in Armadillo and R_ext/BLAS.h
 */

typedef double optfunc(int, double *, void *);

void optim(
    int, double*, double*, double*, optfunc,
    int*, double, double, void*,
    double, double, double, int, int*, int);
