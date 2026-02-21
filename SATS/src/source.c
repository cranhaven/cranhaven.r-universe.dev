
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <R.h>
#include <R_ext/Memory.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


#define binary char
#define NMF_STOPCONV 40
#define NMF_CHECKINTERVAL 10
#define NMF_MAXITER 2000
#define CHECK_MEM(obj) if (obj == NULL) {Rprintf("ERROR: allocating memory \n"); error("1");}
#define NUMERICZERO 1e-200
#define LARGEDOUBLE 1.0e200
#define MINUSINFINITY -1.0e200
#define DOUBLE_MISS -9999.0e200
#define DOUBLE_MISS_TEST -9999.0e150
#define MAX( a, b ) ( ((a) > (b)) ? (a) : (b) )
#define MIN( a, b ) ( ((a) < (b)) ? (a) : (b) )


#define IARG_N_SAMP 0
#define IARG_N_SIG 1
#define IARG_N_MUT 2
#define IARG_N_START 3
#define IARG_EM_MAXITER 4
#define IARG_PRINT 5
#define IARG_DEBUG 6

#define DARG_EM_EPS 0
#define DARG_RUNIF_LOWER 1
#define DARG_RUNIF_UPPER 2

struct sal_struct {
  
  double **V;         /* NMUT X NSAMP */
  double **L;         /* NMUT X NSAMP */
  double **W;         /* NMUT X NSIG */
  double **tW;        /* transose(W) */
  double **tWL;       /* transpose(W) %*% L  NSIG X NSAMP */
  double **H;         /* NSIG X NSAMP */
  double *ret_H;
  double **WH;        /* W %*% H */
  double eps;
  double runif_lower;
  double runif_upper;
  double loglike;
  double **tmat_p_n;   /* NMUT X NSAMP */
  double **tmat_k_n;   /* NSIG X NSAMP */

  int n;            /* N_SAMP */
  int p;            /* N_MUT */
  int k;            /* N_SIG */
  int itermax;      
  int nstart;
  int print;
  int DEBUG;

};
typedef struct sal_struct SALSTR;
 
/*
static void print_dVec(vec, n, name, by)
double *vec;
int n, by;
char name[10];
{
  int i, j=0;
  Rprintf("%s \n", name);
  for (i=0; i<n; i++) {
    Rprintf(" %g ", vec[i]);
    j++;
    if (by && (j >= by)) {
      Rprintf("\n");
      j = 0;
    }
  }
  Rprintf("\n");
}

static void print_dMat_vec(vec, nr, nc, name)
double *vec;
int nr, nc;
char name[10];
{
  int i, j;
  Rprintf("%s \n", name);
  for (i=0; i<nr; i++) {
    for (j=0; j<nc; j++) {
      Rprintf(" %g ", vec[i + j*nr]);
    }
    Rprintf("\n");
  }
  
}

static void print_dMat(mat, nr, nc, name)
double **mat;
int nr, nc;
char name[10];
{
  int i, j;
  Rprintf("%s \n", name);
  for (i=0; i<nr; i++) {
    for (j=0; j<nc; j++) {
      Rprintf(" %g ", mat[i][j]);
    }
    Rprintf("\n");
  }
  
}


static double * dVec_alloc(n, initFlag, initVal)
int n, initFlag;
double initVal;
{
  int i;
  double *ret, *p;

  if (n < 1) error("n < 1 in dVec_alloc");
  ret = (double *) R_Calloc(n, double);
  CHECK_MEM(ret);
  if (initFlag) {
    for (i=0, p=ret; i<n; i++, p++) *p = initVal;
  }

  return(ret);

} 
*/

static double ** dMat_alloc(int nr, int nc, int initFlag, double initVal)
{
  int i, j;
  double **ret, *p;

  ret = (double **) R_Calloc(nr, double *);
  CHECK_MEM(ret);
  for (i=0; i<nr; i++) {
    ret[i] = (double *) R_Calloc(nc, double);
    CHECK_MEM(ret[i]);
    if (initFlag) {
      p = ret[i];
      for (j=0; j<nc; j++) p[j] = initVal; 
    }
  }
  return(ret);

} 


static double ** dMat_alloc_fromVec(int nr, int nc, double *vec)
{
  int i;
  double **ret;

  if (nr < 1) error("nr < 1 in dVec_alloc_fromVec");
  if (nc < 1) error("nc < 1 in dVec_alloc_fromVec");
  if (!vec) error("vec = NULL in dVec_alloc_fromVec");

  ret = (double **) R_Calloc(nr, double *);
  CHECK_MEM(ret);
  for (i=0; i<nr; i++) ret[i] = &vec[nc*i];
 
  return(ret);

} 

static void dMat_free(double **x, int nr)
{
  int i;
  
  for (i=0; i<nr; i++) R_Free(x[i]);
  R_Free(x);

}

/*
static int * iVec_alloc(n, initFlag, initVal)
int n, initFlag, initVal;
{
  int i, *ret, *p;

  if (n < 1) error("n < 1 in iVec_alloc");
  ret = (int *) R_Calloc(n, int);
  CHECK_MEM(ret);
  if (initFlag) {
    for (i=0, p=ret; i<n; i++, p++) *p = initVal;
  }

  return(ret);

} 

static char * cVec_alloc(n, initFlag, initVal)
int n, initFlag;
char initVal;
{
  char *ret, *pret;
  int i;

  if (n < 1) error("n < 1 in cVec_alloc");
  ret = (char *) R_Calloc(n, char);
  CHECK_MEM(ret);
  if (initFlag) {
    for (i=0, pret=ret; i<n; i++, pret++) *pret = initVal;
  }
  
  return(ret);

} 

static void copy_dVec(v, n, ret)
double *v, *ret;
int n;
{
  int i;
  double *p1, *p2;

  for (i=0, p1=v, p2=ret; i<n; i++, p1++, p2++) *p2 = *p1;

}
*/

static void matrixMult(double **m1, int m1_nr, int m1_nc, double **m2, int m2_nc, double **ret)
{
  int i, j, k;
  double sum;

  for (i=0; i<m1_nr; i++) {
    for (j=0; j<m2_nc; j++) {
      sum = 0.0;
      for (k=0; k<m1_nc; k++) {
        sum += m1[i][k]*m2[k][j];
      }
      ret[i][j] = sum;
    }
  }

}

static void putMatIntoVecbyRow(double **mat, int nr, int nc, double *ret)
{
  int i, j, k;

  k = 0;
  for (i=0; i<nr; i++) {
    for (j=0; j<nc; j++) ret[k++] = mat[i][j]; 
  }

}

static void init_H0(int nr, int nc, double lower, double upper, double **ret)
{
  int i, j;

  for (i=0; i<nr; i++) {
    for (j=0; j<nc; j++) ret[i][j] = runif(lower, upper);
  }

}

static void update_H(double **H, double **tmat, double **tWL, int H_nr, int H_nc)
{
  /* matrix H gets updated 
     H1 <- H0*( trans.W0 %*% (V/(WH.hat)))/(trans.W0 %*% L)
     tmat = trans.W0 %*% (V/(WH.hat))
     tWL  = trans.W0 %*% L
  */
  int i, j;

  for (i=0; i<H_nr; i++) {
    for (j=0; j<H_nc; j++) {
      H[i][j] = H[i][j]*(tmat[i][j]/tWL[i][j]);
    }
  }
  
}

static void transposeMat(double **mat, int mat_nr, int mat_nc, double **ret)
{
  int i, j;

  for (i=0; i<mat_nr; i++) {
    for (j=0; j<mat_nc; j++) {
      ret[j][i] = mat[i][j]; 
    }
  }

}

static void divideMats(double **mat1, double **mat2, int nr, int nc, double **ret)
{
  int i, j;

  for (i=0; i<nr; i++) {
    for (j=0; j<nc; j++) {
      ret[i][j] = mat1[i][j]/mat2[i][j]; 
    }
  }

}

static double calc_loglike(double **V, double **WH, double **L, int nr, int nc)
/* V, WH, and L all have dim nr by nc (p X n) */
{
  /* V.hat   <- WH.hat * L
     Lik_mat <- V*log(V.hat)-V.hat 
     lik0    <- sum(Lik_mat)/np
  */

  int i, j;
  double sum=0.0, vhat;

  for (i=0; i<nr; i++) {
    for (j=0; j<nc; j++) {
      vhat = WH[i][j]*L[i][j];
      if (vhat < NUMERICZERO) return(DOUBLE_MISS); 
      sum += V[i][j]*log(vhat) - vhat; 
    }
  }  
  sum = sum/(nr*nc);
  return(sum);
}

static int salmon_h_1(SALSTR *str)
{
  int n, k, p, iter, conv, prt, DEBUG=str->DEBUG;
  double ll0, ll1, **W, **H, **WH, **V, **L, **tW, **tWL, **tmat_p_n, **tmat_k_n, eps;

  if (DEBUG) Rprintf("Begin salmon_h_1\n");

  str->loglike = DOUBLE_MISS;
  n            = str->n;
  k            = str->k;
  p            = str->p;
  W            = str->W;
  H            = str->H;
  WH           = str->WH;
  V            = str->V;
  L            = str->L;
  tW           = str->tW;
  tWL          = str->tWL;
  tmat_p_n     = str->tmat_p_n;
  tmat_k_n     = str->tmat_k_n;
  conv         = 0;
  prt          = str->print;
  eps          = str->eps;
  ll1          = 0.0;

  if (DEBUG) Rprintf(" n=%d, k=%d, p=%d\n", n, k, p);
  if (DEBUG) Rprintf(" Initialize H\n");
  init_H0(k, n, str->runif_lower, str->runif_upper, H);

  if (DEBUG) Rprintf(" Multiply W and H\n");
  matrixMult(W, p, k, H, n, WH);
  if (DEBUG) Rprintf(" Compute initial loglike\n");
  ll0 = calc_loglike(V, WH, L, p, n);
  if (ll0 < DOUBLE_MISS_TEST) return(conv);
  if (DEBUG) Rprintf(" Initial loglike=%g\n", ll0);

  for (iter=1; iter<=str->itermax; iter++) {
    /* M step: Update H */
    divideMats(V, WH, p, n, tmat_p_n);
    matrixMult(tW, k, p, tmat_p_n, n, tmat_k_n);
    update_H(H, tmat_k_n, tWL, k, n);

    /* E step: update the expectation */
    matrixMult(W, p, k, H, n, WH);
    
    ll1 = calc_loglike(V, WH, L, p, n);
    if (DEBUG) Rprintf("loglike=%g\n", ll1);
    if ( (ll1 < DOUBLE_MISS_TEST) || (!R_FINITE(ll1)) ) {
      if (prt) Rprintf("Non-finite likelihood at iteration %d\n", iter);
      return(conv);
    }
    if (fabs((ll1 - ll0)/ll0) < eps) {
      conv = 1;
      break;
    }
    ll0 = ll1;
  }
  str->loglike = ll1;
  if (prt) {
    if (conv) {
      Rprintf("EM algorithm converged in %d iterations. Loglike = %g\n", iter, ll1);
    } else {
      Rprintf("EM algorithm ran out of iterations\n");
    }
  }
  if (DEBUG) Rprintf("End salmon_h_1\n");
  return(conv);
}

static int salmon_h(SALSTR *str)
{
  int i, rc, k, n, conv=0, DEBUG=str->DEBUG;
  double maxlike, ll, **H, *ret_H;

  if (DEBUG) Rprintf("Begin salmon_h\n");
  k       = str->k;
  n       = str->n;
  H       = str->H;
  ret_H   = str->ret_H;
  maxlike = DOUBLE_MISS;

  for (i=1; i<=str->nstart; i++) {
    rc = salmon_h_1(str);
    /* Check likelihood */
    ll = str->loglike;
    if ((ll > DOUBLE_MISS_TEST) && (ll > maxlike)) {
      /* save likelihood and H */
      conv    = rc;
      maxlike = ll;
      putMatIntoVecbyRow(H, k, n, ret_H);
    }
  }
  str->loglike = maxlike;
  if (DEBUG) Rprintf("End salmon_h\n");
  return(conv);
}

static void SALSTR_init(SALSTR *str, int *iargs, double *dargs, double *V, double *L, double *W, double *ret_H) 
{
  int n, p, k;

  str->DEBUG       = iargs[IARG_DEBUG];
  if (str->DEBUG) Rprintf("Begin SALSTR_init\n");
  str->n           = iargs[IARG_N_SAMP];
  str->k           = iargs[IARG_N_SIG];
  str->p           = iargs[IARG_N_MUT];
  str->itermax     = iargs[IARG_EM_MAXITER];
  str->nstart      = iargs[IARG_N_START];
  str->print       = iargs[IARG_PRINT];
  
  str->eps         = dargs[DARG_EM_EPS];
  str->runif_lower = dargs[DARG_RUNIF_LOWER];
  str->runif_upper = dargs[DARG_RUNIF_UPPER];

  n              = str->n;
  p              = str->p;
  k              = str->k;
  str->V         = dMat_alloc_fromVec(p, n, V);
  str->L         = dMat_alloc_fromVec(p, n, L);
  str->W         = dMat_alloc_fromVec(p, k, W);
  str->ret_H     = ret_H;

  str->H         = dMat_alloc(k, n, 1, 0.0);
  str->WH        = dMat_alloc(p, n, 1, 0.0);
  str->tmat_p_n  = dMat_alloc(p, n, 1, 0.0);
  str->tmat_k_n  = dMat_alloc(k, n, 1, 0.0);
  str->tWL       = dMat_alloc(k, n, 1, 0.0);
  str->tW        = dMat_alloc(k, p, 1, 0.0);
  transposeMat(str->W, p, k, str->tW);
  matrixMult(str->tW, k, p, str->L, n, str->tWL);
  
  if (str->DEBUG) Rprintf("End SALSTR_init\n");
}

static void SALSTR_free(SALSTR *str) 
{
  int p=str->p, k=str->k, DEBUG=str->DEBUG;

  if (DEBUG) Rprintf("Begin SALSTR_free\n");
  /* Vectors of pointers */
  if (DEBUG) Rprintf(" free str->V\n");
  R_Free(str->V); str->V = NULL;
  if (DEBUG) Rprintf(" free str->L\n");
  R_Free(str->L); str->L = NULL;
  if (DEBUG) Rprintf(" free str->W\n");
  R_Free(str->W); str->W = NULL;
 
  if (DEBUG) Rprintf(" free str->H\n");
  dMat_free(str->H, k);        str->H        = NULL;
  if (DEBUG) Rprintf(" free str->WH\n");
  dMat_free(str->WH, p);       str->WH       = NULL;
  if (DEBUG) Rprintf(" free str->tmat_p_n\n");
  dMat_free(str->tmat_p_n, p); str->tmat_p_n = NULL;
  if (DEBUG) Rprintf(" free str->tmat_k_n\n");
  dMat_free(str->tmat_k_n, k); str->tmat_k_n = NULL;
  if (DEBUG) Rprintf(" free str->tW\n");
  dMat_free(str->tW, k);       str->tW       = NULL;
  if (DEBUG) Rprintf(" free str->tWL\n");
  dMat_free(str->tWL, k);      str->tWL      = NULL;

  if (DEBUG) Rprintf("End SALSTR_free\n");

}

void C_call_salmon(int *iargs, double *dargs, double *V, double *L, double *W, double *ret_H, 
                   double *ret_ll, int *ret_conv)
{
  SALSTR str;

  /* For random number generation */
  GetRNGstate();

  SALSTR_init(&str, iargs, dargs, V, L, W, ret_H);
  
  *ret_conv = salmon_h(&str);
  *ret_ll   = str.loglike;

  SALSTR_free(&str);

  PutRNGstate(); 

  return;
}








