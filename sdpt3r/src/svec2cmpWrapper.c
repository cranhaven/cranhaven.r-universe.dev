#include "svec.h"
void svec2cmpWrapper(int *n, int *numblk, int *cumblksize, int *blknnz, 
              double *r2, 
              double *A, int *irA, int *jcA, int *isspA, 
              double *B, int *irB, int *jcB, int *isspB,
              double *AI, double *BI){
  svec2cmp(n[0], numblk[0], cumblksize, blknnz, r2[0], 
           A, irA, jcA, isspA[0], B, irB, jcB, isspB[0],
           AI, BI);
}
