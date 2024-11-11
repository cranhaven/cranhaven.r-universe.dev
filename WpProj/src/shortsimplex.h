#ifndef SHORTSIMPLEX_H
#define SHORTSIMPLEX_H
#include <math.h>
#include <R.h>
#include <R_ext/Utils.h>
#include <stdlib.h>


void shortsimplex(int *ss, int *kk, double *pp, 
                  int *mm, int *nn, int *a, int *b, 
                  double *costm, 
                  int *assignment, int *basis);
  
#endif //SHORTSIMPLEX_H
