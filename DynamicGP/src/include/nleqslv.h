#ifndef NLEQSLV_H
#define NLEQSLV_H
#include<R.h>
typedef double (*targfun) (double, void*);
typedef double (*targderv) (double, void*);
typedef void (*targfunderv) (double, void*, double*, double*);

enum Stat
{
  success,
  failure,
  exceed
};
void F77_NAME(liqsiz)(int *n, int *wrksiz);
void F77_NAME(nwnleq)(double *x, int *n, double *scalex, int *maxit,
		      int *jacflg, double *xtol, double *ftol, double *btol,
		      double *cndtol, int *method, int *global, int *xscalm,
		      double *stepmx, double *delta, double *sigma,
                      double *rjac, int *ldr, double *rwork, int *lrwork,
		      double *rcdwrk, int *icdwrk, double *qrwork, int *qrwsiz,
                      void (*fcnjac)(double *rjac, int *ldr, double *x, int *n),
                      void (*fcnval)(double *xc, double *fc, int *n, int *flag),
                      int *outopt, double *xp, double *fp, double *gp, int *njcnt,
		      int *nfcnt, int *iter, int *termcd);

int nleqslv(double x0, targfun fun, targderv dfun, void* param,
	    double *root, int maxiter, double xabs, double xrel);
#endif
