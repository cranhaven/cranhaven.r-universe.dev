#define  r2   1.41421356237309504880      /* sqrt(2) */
#define  ir2  0.70710678118654752440      /* 1/sqrt(2) */


void setvec(int n, double *x, double alpha);

void schurij1(int n, double *Avec, 
              int *idxstart, int *nzlistAi, int *nzlistAj,
              double *U, int col, double *schurcol);

void schurij3(int n,  double *Avec, 
              int *idxstart, int *nzlistAi, int *nzlistAj,
              double *U, double *V, int col, double *schurcol);

void vec(int numblk, int *cumblksize, int *blknnz, 
         double *A, int *irA, int *jcA, double *B);

void schurij2(double *Avec, 
              int *idxstart, int *nzlistAi, int *nzlistAj, 
              double *Utmp,
              int *nzlistAr, int *nzlistAc, int *cumblksize, 
              int *blkidx, int col, double *schurcol);

void schurij4( double *Avec, 
               int *idxstart, int *nzlistAi, int *nzlistAj,
               double *Utmp, double *Vtmp, 
               int *nzlistAr, int *nzlistAc, int *cumblksize, 
               int *blkidx, int col, double *schurcol);
  
  


