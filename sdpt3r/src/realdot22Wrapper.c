#include "mexinprod.h"
void realdot22Wrapper(const double *x, int *irx, int *jcx, 
                 const int *col, const double *y, double *dummy){
  
  dummy[0] = 5;
  dummy[5] = realdot22(x, irx, jcx, col[0], y);
  
}
