#ifndef OPTGREED_H
#define OPTGREED_H

void optgreed_c(double *data, double *vec, int *nrow, int *ncol, 
              double *vcovi, int *ntr, int *l2, 
              int *l1names, int *valid, 
              double *validvar, double *validlb, 
              double *validub, int *verbose, 
              double *pairdist, int *ismahal, 
              int *result, int *p);

#endif // OPTGREED_H
