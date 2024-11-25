#include "schurmat.h"
void setvecWrapper(int *n, double *x, double *alpha){
  setvec(n[0], x, alpha[0]);
}
