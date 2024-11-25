#include "mexprod.h"
void  prod1Wrapper(int *m, int *n, int *p, 
            double *A,  int *irA, int *jcA, int *isspA, 
            double *B,  double *P,  int *irP, int *jcP, 
            int *list1, int *list2, int *len){
  prod1(m[0], n[0], p[0], A,  irA, jcA, isspA[0], B,  P,  irP, jcP, list1, list2, len[0]);
}
