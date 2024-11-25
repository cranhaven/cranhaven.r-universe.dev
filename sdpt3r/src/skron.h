#define  r2   1.41421356237309504880      /* sqrt(2) */
#define  ir2  0.70710678118654752440      /* 1/sqrt(2) */

void skronWrapper(int *n, int *maxblksize, double *P, double *Q,
                  double *x1, double *y1, double *x2, double *y2,
                  int *r, int *c, double *vvtmp);

void skron2Wrapper(int *n, int *maxblksize, double *P, double *Q,
                   double *x1, double *y1, double *x2, double *y2,
                   int *r, int *c, double *vvtmp);

void skron2(int n, int maxblksize, double *P, double *Q,
            double *x1, double *y1, double *x2, double *y2,
            int r, int c, double *vvtmp);

void skron(int n, int maxblksize, double *P, double *Q,
           double *x1, double *y1, double *x2, double *y2,
           int r, int c, double *vvtmp);
