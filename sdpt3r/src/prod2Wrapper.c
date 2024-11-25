#include "mexprod.h"
void  prod2Wrapper(int *m, int *n, int *p,  
            double *A, int *irA, int *jcA, int *isspA,
            double *B, int *irB, int *jcB, int *isspB,
            double *P, int *irP, int *jcP, double *Btmp, 
            int *list1, int *list2, int *len){
  prod2(m[0], n[0], p[0], A, irA, jcA, isspA[0], B, irB, jcB, isspB[0],
        P, irP, jcP, Btmp, list1, list2, len[0]);
}
