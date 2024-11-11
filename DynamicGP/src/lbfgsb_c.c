#include<stdlib.h>
#include<string.h>
#include "lbfgsb.h"
#include "matrix.h"

static int* new_fill_ivec(int n, int fill)
{
  int i;
  int *ivec = new_ivector(n);
  for(i=0; i<n; ++i) ivec[i] = fill;
  return ivec;
}
static void writemsg(int itask, char *msg)
{
  switch(itask){
  case 1: strcpy(msg,"NEW_X"); break;
  case 2: strcpy(msg,"START"); break;
  case 3: strcpy(msg,"STOP"); break;
  case 4: strcpy(msg,"FG"); break;
  case 5: strcpy(msg,"ABNORMAL_TERMINATION_IN_LNSRCH"); break;
  case 6: strcpy(msg,"CONVERGENCE"); break;
  case 7: strcpy(msg,"CONVERGENCE: NORM_OF_PROJECTED_GRADIENT_<=_PGTOL"); break;
  case 8: strcpy(msg,"CONVERGENCE: REL_REDUCTION_OF_F_<=_FACTR*EPSMCH"); break;
  case 9: strcpy(msg,"ERROR: FTOL .LT. ZERO"); break;
  case 10: strcpy(msg,"ERROR: GTOL .LT. ZERO"); break;
  case 11: strcpy(msg,"ERROR: INITIAL G .GE. ZERO"); break;
  case 12: strcpy(msg,"ERROR: INVALID NBD"); break;
  case 13: strcpy(msg,"ERROR: N .LE. 0"); break;
  case 14: strcpy(msg,"ERROR: NO FEASIBLE SOLUTION"); break;
  case 15: strcpy(msg,"ERROR: STP .GT. STPMAX"); break;
  case 16: strcpy(msg,"ERROR: STP .LT. STPMIN"); break;
  case 17: strcpy(msg,"ERROR: STPMAX .LT. STPMIN"); break;
  case 18: strcpy(msg,"ERROR: STPMIN .LT. ZERO"); break;
  case 19: strcpy(msg,"ERROR: XTOL .LT. ZERO"); break;
  case 20: strcpy(msg,"FG_LNSRCH"); break;
  case 21: strcpy(msg,"FG_START"); break;
  case 22: strcpy(msg,"RESTART_FROM_LNSRCH"); break;
  case 23: strcpy(msg,"WARNING: ROUNDING ERRORS PREVENT PROGRESS"); break;
  case 24: strcpy(msg,"WARNING: STP .eq. STPMAX"); break;
  case 25: strcpy(msg,"WARNING: STP .eq. STPMIN"); break;
  case 26: strcpy(msg,"WARNING: XTOL TEST SATISFIED"); break;
  }
}

void lbfgsb_C(int n, double *x, double *l, double *u, lbfgsb_fmin fun,
	      lbfgsb_fgrad grad, int *fail, void *info, double pgtol,
	      int *counts, int maxit, char *msg, int trace)
{
  int m, iprint, itask, icsave, iter;
  int *nbd, *iwa, isave[44], lsave[4];
  double factr, f, *wa, *g, dsave[29];

  m = 5;
  itask = 2;
  factr = 1.0e7;
  nbd = new_fill_ivec(n, 2);
  wa = new_zero_vector(2*m*n + 5*n + 11*m*m + 8*m);
  iwa = new_ivector(3 * n);
  g = new_vector(n);

  *fail = 0;
  switch(trace) {
  case 2: iprint = 0; break;
  case 3: iprint = 10; break;
  case 4: iprint = 99; break;
  case 5: iprint = 100; break;
  case 6: iprint = 101; break;
  default: iprint = -1; break;
  }
  iter = 0;
  while(1)
  {
    lbfgsb3_(&n, &m, x, l, u, nbd, &f, g, &factr, &pgtol, wa,
	     iwa, &itask, &iprint, &icsave, lsave, isave, dsave);
    if(itask == 4 || itask == 20 || itask == 21) { /* fg  */
      f = fun(n, x, info);
      grad(n, x, g, info);
    }
    else if(itask == 1){	/* newx */
      iter++;
      /* if(trace == 1 && (iter % 10 == 0)){ */
      /* 	MYprintf(MYstdout,"iter %4d value %f\n", iter, f); */
      /* } */
      if(iter > maxit) {
	*fail = 1;
	break;
      }
    }
    else if(itask >= 23 && itask <= 26){ /* warning */
      *fail = 51;
      break;
    }
    else if(itask >= 6 && itask <= 8) { /* convergence */
      break;
    }
    else if(itask >= 9 && itask <= 19){ /* error */
      *fail = 52;
      break;
    }
    else {			/* otherwise */
      *fail = 52;
      break;
    }
  }
  counts[0] = counts[1] = isave[33]; /* total numer of function evals */
  writemsg(itask,msg);
  f = fun(n, x, info);		/* set the optimal value */
  /* if(trace){ */
  /*   MYprintf(MYstdout,"final value %f \n", f); */
  /*   if(iter < maxit && *fail == 0) MYprintf(MYstdout,"converged\n"); */
  /*   else MYprintf(MYstdout,"stopped after %i iterations\n", iter); */
  /* } */
  free(wa);
  free(iwa);
  free(nbd);
  free(g);
}
