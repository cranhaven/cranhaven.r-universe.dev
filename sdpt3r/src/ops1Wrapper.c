#include "opsHeader.h"
void ops1Wrapper(double *x, double *y, double *z, int *numblk, int *cumblk, int *options){
  ops1(x,y,z,numblk[0],cumblk,options[0]); 
}
