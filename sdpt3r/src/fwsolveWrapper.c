#include "mextriang.h"
void fwsolveWrapper(double *y,double *u,double *x,int *n) {
  fwsolve(y,u,x,n[0]);
}
