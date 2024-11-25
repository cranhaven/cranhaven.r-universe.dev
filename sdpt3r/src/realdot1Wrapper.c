#include "mexinprod.h"
void realdot1Wrapper(const double *x, const int *col, 
                const double *y, const int *n, double *dummy){
  dummy[0] = 5;
  dummy[0] = realdot1(x, col[0], y, n[0]);
}
