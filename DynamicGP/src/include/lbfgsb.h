#ifndef __LBFGSB_H__
#define __LBFGSB_H__

extern void lbfgsb3_(int *n, int *m, double *x, double *l, double *u,
		     int *nbd, double *f, double *g, double *factr,
		     double *pgtol, double *wa, int *iwa, int *itask,
		     int *iprint, int *icsave, int *lsave, int *isave,
		     double *dsave);
typedef double (* lbfgsb_fmin)(int, double*, void*);
typedef void (* lbfgsb_fgrad)(int, double*, double *, void *);

void lbfgsb_C(int n, double *x, double *l, double *u, lbfgsb_fmin fun,
	      lbfgsb_fgrad grad, int *fail, void *info, double pgtol,
	      int *counts, int maxit, char *msg, int trace);
#endif
