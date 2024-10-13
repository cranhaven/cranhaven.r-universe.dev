#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <math.h>

#ifndef CBLAS
#  include <R_ext/BLAS.h>
#  define daxpy F77_CALL(daxpy)
#else
#  include <cblas.h>
#endif

#include "ccd.h"

inline double softthresh(double x,double t) {
  double v = fabs(x) - t;
  double ret = 0.;
  if (v  > 0.) {
    if (x >= 0.) ret = v;
    else ret = -v;
  } 
//  printf("softthresh: %f\n", ret);
	return ret;
}
int ccd_common(param_t* params) {
  int its = 0;
  double delta = 0.0;
  double deltabeta = 0.0;
  double betajstar = 0.0;
  double betajstar_old = 0.0;
  //bool penalizethis = true;
  //int state = 1;
  int p = params->p;
  double factor2 = params->factor2;

  // inf-norm of X'y
  double infnorm = 0.;
  for (int i=0; i < p; ++i) {
    double this = fabs(params->Xty[i] / params->factor2);
    if (this > infnorm)
      infnorm = this;
  }
  if (params->trace > 0) myprintf("lambda: %f\n",params->lambda);
  if (params->trace > 0) myprintf("infnorm: %f\n",infnorm);
  params->infnorm = infnorm;
  if (params->lambda > infnorm && !params->nopenalize) {
    if (params->trace > 0) myprintf("returning because lambda > infnorm and nopenalize is not set\n");
    return 1; // XXX: quit if lambda > ||X'y||_inf and nopenalize is not set
  }
  double *s;
  if (params->s)
   s = params->s;
  else {
   s = params->Xty;
   for(int i=0; i < p; ++i) { // compensate s for given beta
     const double factor = -params->beta[i]*factor2;
     if (factor != 0) {
       int one = 1; // memory layout
       daxpy(&p, &factor, &params->XtX[i], &p, s, &one);
     }
   }
  }
  if (!params->w) {
    params->w = (double*)calloc(p,sizeof(double));
    for(int i=0; i < p; ++i) params->w[i] = 1.0f;
  }
  for(int i=0; params->nopenalize && params->nopenalize[i] >= 0; ++i) {
    params->w[params->nopenalize[i]] = 0.0f;
  }

  if (params->trace >= 3)
    for (int i=0; i < p; ++i) {
      myprintf("penalize beta_%d with %.2f\n",i,params->w[i]);
    }
  //

  do {
    delta = 0.0;
    for(int j=0;j < p; ++j) {
      //gsl_vector_view XtXj = gsl_matrix_column(XtX_, (size_t)j);
      //double XtXjj = gsl_matrix_get(XtX_,j,j);
      //double XtXjj = gsl_vector_get(&XtXj.vector, j);
      double XtXjj = params->XtX[j+j*p];
//			printf("XtXjj = %f  \n",XtXjj);

      if (XtXjj == 0. || params->forcezero == j+1) continue;
      //if (active && its % 10 != 0 && beta_[j] == 0.)  continue;
 
      betajstar_old = betajstar;
      betajstar = s[j] + (factor2*XtXjj) * params->beta[j];
#ifdef DEBUG
      if (params->trace >= 2) {
	FILE*D = fopen("ccd.debug","a");
	fprintf(D,"forcezero=%d, its=%d, betajstar-pre: %f\n", params->forcezero,its,betajstar);
	fclose(D);
      }
#endif

      if (isinf(betajstar) || isnan(betajstar)) {
	myprintferr("******************************************\n"
	     __FILE__ ": BUG OR PATHOLOGICAL DATA\n\n");
	myprintferr( "Please mail me the data that can reproduce this error <Tobias.Abenius@Chalmers.SE>\n");
	myprintferr("betajstar prev = %f  \n",betajstar_old);
	myprintferr("deltabeta prev = %f  \n",deltabeta);
	myprintferr("s_%d = %f  \n",j,s[j]);
	myprintferr("betajstar_%d = %f  \n",j,betajstar);
	myprintferr("beta_%d = %f  \n",j,params->beta[j]);
	myprintferr("XtXjj = %f  \n",XtXjj);
	myprintferr("\nGiving up...\n");
	myprintferr("******************************************\n");
#       ifdef win32
#         ifdef _THIS_IS_AN_R_PACKAGE
            R_FlushConsole();
#         endif 
#       endif 
	if(params->w) free(params->w);
	return 0;
      }
//      if (fabs(params->w[j]*params->lambda) < 1e-40) {
//        betajstar /= factor2*XtXjj;
//      } else {
        betajstar = softthresh(betajstar, params->w[j]*params->lambda) / (factor2*XtXjj);
#ifdef DEBUG
      if (params->trace >= 2) {
	FILE*D = fopen("ccd.debug","a");
	fprintf(D,"forcezero=%d, its=%d, betajstar-post: %f, lam: %f, div: %f\n", params->forcezero,its,betajstar,params->w[j]*params->lambda,factor2*XtXjj);
	fclose(D);
      }
#endif

//      }
			//printf("beta_%d = %.4f\n",j,betajstar);
      deltabeta = betajstar - params->beta[j];
#ifdef DEBUG
      if (params->trace >= 2) {
	FILE*D = fopen("ccd.debug","a");
	fprintf(D,"forcezero=%d, its=%d, deltabeta: %f\n", params->forcezero,its,deltabeta);
	fclose(D);
      }
#endif

      params->beta[j] = betajstar;
      delta = max(delta, fabs(deltabeta));
      /* s <- s - 2*deltabeta XtX(:,j);
         axpy :: y <- ax + y*/
#ifndef CBLAS
#if 0
#warning Using R fortran BLAS calls
#endif
      const double factor = -deltabeta*factor2;
      int one = 1; // memory layout
      daxpy(&p, &factor, &params->XtX[j], &p, s, &one);
#else
#if 0
#warning Using cblas BLAS calls
#endif
      cblas_daxpy(p, -deltabeta*factor2,&params->XtX[j],p, s,1);
#endif
#ifdef DEBUG
      if (params->trace >= 2) 
	for (int di=0; di < p; ++di)  {
	  FILE*D = fopen("ccd.debug","a");
	  fprintf(D,"forcezero=%d, its=%d, s%d: %f\n", params->forcezero,its,di,s[di]);
	  fclose(D);
	}
#endif

      //gsl_blas_daxpy(-deltabeta*factor2,&XtXj.vector, &s.vector);
    }
#ifdef DEBUG
    if (params->trace >= 2) {
      FILE*D = fopen("ccd.debug","a");
      fprintf(D,"its = %d \tdelta %f  \n",its,delta);
      fclose(D);
    }
#endif
  } while (++its < params->maxits && delta > params->tol);
  if (params->trace) myprintf("ccd ran for %d iterations, delta: %g\n",its,delta);
  params->its = its;
  params->delta = delta;
  for(int i=0; i < params->p; ++i) {
    params->beta[i] *= factor2;
  }
  params->s = s;
  if(params->w) free(params->w);
  return 1;
}

