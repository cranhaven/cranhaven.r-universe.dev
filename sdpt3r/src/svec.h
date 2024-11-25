void svec4cmp(int n, int numblk, int *cumblksize, int *blknnz, 
              double r2, 
              double *A, int *irA, int *jcA, int isspA, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI);

void svec3cmp(int n, double r2, 
              double *A, int *irA, int *jcA, int isspA, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI);

void svec2cmp(int n, int numblk, int *cumblksize, int *blknnz, 
              double r2, 
              double *A, int *irA, int *jcA, int isspA, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI);

void svec1cmp(int n, double r2, 
              double *A, int *irA, int *jcA, int isspA, 
              double *B, int *irB, int *jcB, int isspB,
              double *AI, double *BI);

void svec4(int n, int numblk, int *cumblksize, int *blknnz, 
           double r2, 
           double *A, int *irA, int *jcA, int isspA, 
           double *B, int *irB, int *jcB, int isspB);

void svec3(int n, double r2, 
           double *A, int *irA, int *jcA, int isspA, 
           double *B, int *irB, int *jcB, int isspB);

void svec2(int n, int numblk, int *cumblksize, int *blknnz, 
           double r2, 
           double *A, int *irA, int *jcA, int isspA, 
           double *B, int *irB, int *jcB, int isspB);

void svec1(int n, double r2, 
           double *A, int *irA, int *jcA, int isspA, 
           double *B, int *irB, int *jcB, int isspB);
