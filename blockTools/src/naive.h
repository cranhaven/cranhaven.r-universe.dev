#ifndef NAIVE_H
#define NAIVE_H

void naive_c(double *data, double *vec, int *nrow, int *ncol, 
              double *vcovi, unsigned int *ntr, int *l2, 
              int *l1names, int *valid, 
              double *validvar, double *validlb, 
              double *validub, int *verbose, 
              double *pairdist, int *ismahal, 
              int *result, int *p);

#endif 
