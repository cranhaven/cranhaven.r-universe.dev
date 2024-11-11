#include<R.h>
#include"matrix.h"
#include"nleqslv.h"

static targfun sfunc=NULL;
static targderv sdfunc=NULL;
static void* pinfo=NULL;

static void fcnval(double *x, double *fc, int *n, int *flag)
{
  *fc = sfunc(*x, pinfo);
}
static void fcnjac(double *rjac, int *ldr, double *x, int *n)
{
  *rjac = sdfunc(*x, pinfo);
}
/* warning it is not a thread safe function */
int nleqslv(double x0, targfun fun, targderv dfun, void *param,
	    double *root, int maxiter, double xtol, double ftol)
{
  int njcnt, nfcnt, iter, termcd, qrwsiz, icdwrk;
  int n = 1, ldr = 1, lrwork = 9;
  int method = 1, global = 4, xscalm = 0;
  int jacflg[4] = {0,-1,-1,0};
  int outopt[3] = {0, 0, 0};
  double xp, fp, gp, rwork[9], rjac[2], rcdwrk[3];
  double scalex = 1.0, btol = 0.001, stepmx = -1.0, delta = -2.0;
  double cndtol = 1E-12, sigma = 0.5;
  double *qrwork;
  /* setup the data */
  sfunc = fun;
  sdfunc = dfun;
  pinfo = param;
  /* allocate working memory */
  F77_CALL(liqsiz)(&n, &qrwsiz);
  qrwork = new_vector(qrwsiz);  

  /* call the fortan routine */
  F77_CALL(nwnleq)(&x0, &n, &scalex, &maxiter, jacflg, &xtol, &ftol, &btol, &cndtol,
		   &method, &global, &xscalm, &stepmx, &delta, &sigma, rjac, &ldr,
		   rwork, &lrwork, rcdwrk, &icdwrk, qrwork, &qrwsiz, &fcnjac,
		   &fcnval, outopt, &xp, &fp, &gp, &njcnt, &nfcnt, &iter, &termcd);
  *root = xp;
  /* free memory */
  free(qrwork);
  /* restore the original state */
  sfunc = NULL;
  sdfunc = NULL;
  pinfo = NULL;
  if(termcd >= 1 && termcd <= 3) return success;
  if(termcd == 4) return exceed;
  return failure;
}
