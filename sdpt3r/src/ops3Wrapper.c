#include "opsHeader.h"
void ops3Wrapper(double *x, double *y, double *z, int *numblk, int *cumblk, int *options){
  ops3(x,y,z,numblk[0],cumblk,options[0]); 
}
