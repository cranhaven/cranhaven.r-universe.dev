#include "skron.h"
void skronWrapper(int *n, int *maxblksize, double *P, double *Q,
                   double *x1, double *y1, double *x2, double *y2,
                   int *r, int *c, double *vvtmp){
  skron(n[0], maxblksize[0], P, Q, x1, y1, x2, y2,
         r[0], c[0], vvtmp); 
}
