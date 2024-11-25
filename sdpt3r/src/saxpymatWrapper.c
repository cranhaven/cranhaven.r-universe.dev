#include "mexMatvec.h"
void saxpymatWrapper(double *y, int *idx1, int *istart, int *iend, double *alp, double *z, int *idx2) {
  saxpymat(y, idx1[0], istart[0], iend[0], alp[0], z, idx2[0]);
}
