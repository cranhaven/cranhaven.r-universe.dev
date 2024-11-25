void sym
  (
    double *Q,
    int n
  );

void symcmp
  (
      double *Q,
      double *QI,
      int n
  );

void smat2cmp
  (
    int n,
    const int numblk,
    const int *cumblksize,
    const int *blknnz, 
    const double ir2, 
    const double *A,
    const int *irA,
    const int *jcA,
    int isspA, 
    int mA,
    int colidx, 
    double *B,
    int *irB,
    int *jcB,
    int isspB,
    double *AI,
    double *BI
  );

void smat1cmp
  (
      int n, 
      const double ir2, 
      const double *A,
      const int *irA,
      const int *jcA,
      int isspA, 
      int mA, 
      int colidx, 
      double *B, 
      int *irB, 
      int *jcB, 
      int isspB,
      double *AI, 
      double *BI
  );

void smat2
  (
      int n,
      const int numblk,
      const int *cumblksize,
      const int *blknnz, 
      const double ir2, 
      const double *A,
      const int *irA,
      const int *jcA,
      int isspA, 
      int mA,
      int colidx, 
      double *B, 
      int *irB, 
      int *jcB, 
      int isspB
  );

void smat1
  (
      int n,
      const double ir2, 
      const double *A,
      const int *irA,
      const int *jcA,
      int isspA, 
      int mA,
      int colidx, 
      double *B,
      int *irB,
      int *jcB,
      int isspB
  );
