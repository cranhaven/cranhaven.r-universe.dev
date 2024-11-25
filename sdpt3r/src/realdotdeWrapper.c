#include "mexMatvec.h"
void realdotdeWrapper(double *x, int *idx, double *y, int *n, double* dummy) {
  dummy[0] = 5;
  dummy[0] = realdotde(x, idx[0], y, n[0]);
}
