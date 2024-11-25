#include "mextriang.h"
void bwsolveWrapper(double *x,double *u,int *n) {
  bwsolve(x,u,n[0]);
}
