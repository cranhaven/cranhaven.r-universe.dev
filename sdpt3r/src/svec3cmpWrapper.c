#include "svec.h"
void svec3cmpWrapper(int *n, double *r2, 
              double *A, int *irA, int *jcA, int *isspA, 
              double *B, int *irB, int *jcB, int *isspB,
              double *AI, double *BI){
  svec3cmp(n[0], r2[0], A, irA, jcA, isspA[0], 
           B, irB, jcB, isspB[0], AI, BI);
}
