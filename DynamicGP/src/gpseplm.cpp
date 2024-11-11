#include <cmath>
#include <cassert>
#include <cstdlib>
#include <cfloat>
#include <R.h>
#include <Rmath.h>
#include "gpseplm.hpp"
#include "exceptions.hpp"

extern "C"{
#include "util.h"
#include "matrix.h"
#include "linalg.h"
#include "lbfgsb.h"
}
using std::free;

#define SDEPS sqrt(DBL_EPSILON)
  
GPsepLm* newGPsepLm(const unsigned int m, const unsigned int n, double **X,
		    double* Z, double *d, const double g, const int dK,
		    const unsigned int p, double **H)
{
  GPsepLm* gplm;
  gplm = (GPsepLm*) malloc(sizeof(GPsepLm));
  gplm->gpsep = newGPsep(m,n,X,Z,d,g,dK);
  gplm->p = p;
  gplm->H = new_dup_matrix(H,n,p);
  return buildGPsepLm(gplm);
}
void deleteGPsepLm(GPsepLm* gplm)
{
  assert(gplm);
  if(gplm->gpsep) deleteGPsep(gplm->gpsep);
  if(gplm->H) delete_matrix(gplm->H);
  if(gplm->regcoef) free(gplm->regcoef);
  if(gplm->Kires) free(gplm->Kires);
  if(gplm->KiH) delete_matrix(gplm->KiH);
  if(gplm->Kernel) delete_matrix(gplm->Kernel);
  free(gplm);
}
void calc_HtKiH_sepLm(GPsepLm* gplm)
{
  assert(gplm);
  assert(gplm->gpsep);
  unsigned int n = gplm->gpsep->n;
  unsigned int p = gplm->p;
  int info;
  double *resid;
  double **HtKiHiHtKi, **Chol;
  linalg_dsymm(CblasRight,p,n,1.0,gplm->gpsep->Ki,
	       n,gplm->H,p,0.0,gplm->KiH,p);
  linalg_dgemm(CblasNoTrans,CblasTrans,p,p,n,1.0,gplm->KiH,
	       p,gplm->H,p,0.0,gplm->HtKiH,p);
  HtKiHiHtKi = new_dup_matrix(gplm->KiH,n,p);
  Chol = new_dup_matrix(gplm->HtKiH,p,p);
  info = linalgext_dposv(p,n,Chol,HtKiHiHtKi);
  if(info){
    delete_matrix(HtKiHiHtKi);
    delete_matrix(Chol);
    throw cholException(__LINE__, __FILE__, info, gplm->gpsep->m, gplm->gpsep->g, gplm->gpsep->d);
  }
  gplm->ldetHtKiH = log_determinant_chol(Chol,p);
  linalg_dgemv(CblasNoTrans,p,n,1.0,HtKiHiHtKi,p,gplm->gpsep->Z,
	       1,0.0,gplm->regcoef,1);
  resid = new_dup_vector(gplm->gpsep->Z,n);
  linalg_dgemv(CblasTrans,p,n,-1.0,gplm->H,p,gplm->regcoef,1,1.0,resid,1);

  linalg_dsymv(n,1.0,gplm->gpsep->Ki,n,resid,1,0.0,gplm->Kires,1);

  linalg_dgemm(CblasTrans,CblasNoTrans,n,n,p,1.0,gplm->KiH,p,HtKiHiHtKi,
	       p,0.0,gplm->Kernel,n);
  gplm->psi = linalg_ddot(n,gplm->gpsep->Z,1,gplm->Kires,1);
  delete_matrix(HtKiHiHtKi);
  delete_matrix(Chol);
  free(resid);
}
GPsepLm* buildGPsepLm(GPsepLm* gplm)
{
  unsigned int n;
  unsigned int p;
  n = gplm->gpsep->n;
  p = gplm->p;
  gplm->KiH = new_matrix(n, p);
  gplm-> HtKiH = new_matrix(p,p);
  gplm->regcoef = new_vector(p);
  gplm->Kires = new_vector(n);
  gplm->Kernel = new_matrix(n,n);
  calc_HtKiH_sepLm(gplm);
  return gplm;
}


/* log likelihood of gp with constant mean */

double llikGPsepLm(GPsepLm *gplm, double *dab, double *gab)
{
  unsigned int k, n, p, m;
  double llik, dnmp, g, ldetK;
  double *d;
  m = gplm -> gpsep -> m;
  n = gplm -> gpsep -> n;
  p = gplm -> p;
  d = gplm->gpsep->d;
  g = gplm->gpsep->g;
  dnmp = (double)(n-p);
  ldetK = gplm -> gpsep -> ldetK;
  llik = 0.0 - 0.5 * dnmp * log(gplm -> psi) - 0.5 * ldetK;
  llik -= 0.5 * gplm -> ldetHtKiH;

  if(dab && dab[0] > 0 && dab[1] > 0) {
    for(k=0; k<m; k++) {
      if(d[k] > 0) llik += dgamma(d[k], dab[0], 1.0/dab[1], 1);
    }
  }

  /* if priors are being used; for nugget */
  if(g > 0 && gab && gab[0] > 0 && gab[1] > 0)
    llik += dgamma(g, gab[0], 1.0/gab[1], 1);

  return(llik);

}

void dllikGPsepLm(GPsepLm* gplm, double *ab, double *dllik)
{
  unsigned int i, j, n, m, p, k;
  double *Kires, *d, *dKKires;
  double dkkij, dnmp, qkires;
  double ***dK;
  assert(gplm->gpsep->dK);
  assert(dllik);
  dK = gplm->gpsep->dK;
  Kires = gplm->Kires;
  n = gplm->gpsep->n;
  m = gplm->gpsep->m;
  d = gplm->gpsep->d;
  p = gplm->p;
  dnmp = (double) (n-p);
  dKKires = new_vector(n);
  for(k=0; k<m; ++k)
  {
    if(ab && ab[0] > 0 && ab[1] > 0)
      dllik[k] = (ab[0] - 1.0)/d[k] - ab[1];
    else
      dllik[k] = 0.0;

    for(i=0; i<n; i++)
    {
      for(j=0; j<i; j++) /* off diagonal */
      {
	dkkij = dK[k][i][j];
        dllik[k] -= gplm->gpsep->Ki[i][j] * dkkij;
	dllik[k] += gplm->Kernel[i][j] * dkkij;
      }

      /* on-diagonal */
      dkkij = dK[k][i][i];
      dllik[k] -= 0.5 * gplm->gpsep->Ki[i][i] * dkkij;
      dllik[k] += 0.5 * gplm->Kernel[i][j] * dkkij;
    }
    linalg_dsymv(n,1.0,dK[k],n,Kires,1,0.0,dKKires,1);
    qkires = linalg_ddot(n, dKKires,1, Kires,1);
    dllik[k] += 0.5 * dnmp * qkires/gplm->psi;
  }
  free(dKKires);
}

void dllikGPsepLm_nug(GPsepLm* gplm, double *ab, double *dllik, double *d2llik)
{
  unsigned int i, j, n, p;
  double dnmp, psi, dlp, d2lp, g, dpsidg;
  double qreski, qresker;
  double **Ki, **Kernel, *Kires, *KiKires, *KerKires;
  assert(dllik);
  g = gplm->gpsep->g;

  if(ab && ab[0] > 0 && ab[1] > 0)
  {
    dlp = (ab[0] - 1.0)/g - ab[1];
    d2lp = 0.0 - (ab[0] - 1.0)/g/g;
  }
  else
    dlp = d2lp = 0.0;

  n = gplm->gpsep->n;
  p = gplm->p;
  dnmp = (double)(n-p);
  psi = gplm -> psi;
  Kires = gplm -> Kires;
  Ki = gplm -> gpsep -> Ki;
  Kernel = gplm -> Kernel;
  for(i=0; i<n; ++i)
  {
    dlp += 0.5 * Kernel[i][i];
    dlp -= 0.5 * Ki[i][i];
    if(d2llik)
    {
      for(j=0; j<i; ++j)
      {
	d2lp += Ki[i][j]*Ki[i][j];
	d2lp += Kernel[i][j] * Kernel[i][j];
	d2lp -= 2.0 * Kernel[i][j] * Ki[i][j];
      }
      d2lp += 0.5*Ki[i][i]*Ki[i][i];
      d2lp += 0.5*Kernel[i][i]*Kernel[i][i];
      d2lp -= Kernel[i][i] * Ki[i][i];
    }
  }
  dpsidg = linalg_ddot(n,Kires,1,Kires,1);
  dlp += 0.5 * dnmp * dpsidg/psi;
  *dllik = dlp;
  if(d2llik)
  {
    KiKires = new_vector(n);
    linalg_dsymv(n,1.0,Ki,n,Kires,1,0.0,KiKires,1);
    qreski = linalg_ddot(n,KiKires,1,Kires,1);
    KerKires = new_vector(n);
    linalg_dsymv(n,1.0,Kernel,n,Kires,1,0.0,KerKires,1);
    qresker = linalg_ddot(n,KerKires,1,Kires,1);
    d2lp += 0.5 * dnmp * dpsidg * dpsidg/psi/psi;
    d2lp -= dnmp*(qreski-qresker)/psi;
    *d2llik=d2lp;
    free(KiKires);
    free(KerKires);
  }
}

void newparamsGPsepLm(GPsepLm *gplm, double *d, const double g)
{
  newparamsGPsep(gplm->gpsep,d,g);
  calc_HtKiH_sepLm(gplm);
}

struct callinfo_sepLm
{
  GPsepLm* gplm;
  double* ab;
  int its;
  int verb;
};

static double fcnnllik_sepLm(int n, double *p, struct callinfo_sepLm *info)
{
  double llik;
  int dsame, k;

  GPsep* gpsep = info->gplm->gpsep;

  assert(n == gpsep->m);

  dsame = 1;

  for(k=0; k<n; k++)
    if(p[k] != gpsep->d[k])
    {
      dsame=0;
      break;
    }

  if(!dsame)
  {
    (info->its)++;
    newparamsGPsepLm(info->gplm, p, gpsep->g);
  }

  llik = llikGPsepLm(info->gplm, info->ab, NULL);

  return 0.0-llik;
}

static void fcnndllik_sepLm(int n, double *p, double *df, struct callinfo_sepLm *info)
{
  int dsame, k;
  GPsep *gpsep = info->gplm->gpsep;

  dsame = 1;
  for(k=0; k<n; ++k)
    if(p[k] != gpsep->d[k])
    {
      dsame = 0;
      break;
    }

  if(!dsame)
  {
    (info->its)++;
    newparamsGPsepLm(info->gplm, p, gpsep->g);
  }
  dllikGPsepLm(info->gplm, info->ab, df);

  for(k=0; k<n; ++k) df[k] = 0.0-df[k];

}

void mleGPsepLm(GPsepLm* gplm, double *dmin, double *dmax, double *ab,
		const unsigned int maxit, int verb, double *p, int *its,
		char *msg, unsigned int msg_size, int *conv)
{
  int lbfgs_verb;
  unsigned int k;
  double rmse;
  double *dold;
  GPsep *gpsep = gplm->gpsep;

  /* create structure for Brent_fmin */
  struct callinfo_sepLm info;
  info.gplm = gplm;
  info.ab = ab;
  info.its = 0;
  info.verb = verb;

  /* copy the starting value */
  dupv(p, gpsep->d, gpsep->m);
  dold = new_dup_vector(gpsep->d, gpsep->m);


  /* set ifail argument and verb/trace arguments */
  *conv = 0;
  if(verb <= 1) lbfgs_verb = 0;
  else lbfgs_verb = verb - 1;

  /* call the C-routine behind R's optim function with method = "L-BFGS-B" */
  lbfgsb_C(gpsep->m, p, dmin, dmax,
	   (lbfgsb_fmin) fcnnllik_sepLm,
	   (lbfgsb_fgrad) fcnndllik_sepLm,
	   conv, &info, SDEPS, its, maxit, msg, lbfgs_verb);

  /* check if parameters in p are new */
  rmse = 0.0;
  for(k=0; k<gpsep->m; k++) rmse += sq(p[k] - gpsep->d[k]);
  rmse = 0.0;
  for(k=0; k<gpsep->m; k++) rmse += sq(p[k] - dold[k]);
  if(sqrt(rmse/k) < SDEPS) {
    snprintf(msg, msg_size, "lbfgs initialized at minima");
    *conv = 0;
    its[0] = its[1] = 0;
  }
}

struct callinfo_sepLm_nug
{
  GPsepLm *gplm;
  double *ab;
  int its;
  int verb;
};

static double fcnnllik_sepLm_nug(double x, struct callinfo_sepLm_nug *info)
{
  double llik;
  (info->its)++;
  GPsep *gpsep = info->gplm->gpsep;
  newparamsGPsepLm(info->gplm, gpsep->d, x);
  llik = llikGPsepLm(info->gplm, NULL, info->ab);
  return 0.0-llik;
}

double Ropt_sepLm_nug(GPsepLm* gplm, double tmin, double tmax,
		      double *ab, const char *msg, int *its, int verb)
{
  double tnew;
  double Tol = SDEPS;
  GPsep *gpsep = gplm->gpsep;
  /* sanity check */
  assert(tmin < tmax);

  /* get parameter */

  /* create structure for Brent_fmin */
  struct callinfo_sepLm_nug info;
  info.gplm = gplm;
  info.ab = ab;
  info.its = 0;
  info.verb = verb;

  /* call the C-routine behind R's optimize function */
  while(1)
  { /* check to make sure solution is not on boundary */
    tnew = Brent_fmin(tmin, tmax, (double (*)(double, void*)) fcnnllik_sepLm_nug, &info, Tol);
    if(tnew > tmin && tnew < tmax) break;

    if(tnew == tmin)
    { /* left boundary found */
      tmin *= 2;
    }
    else
    { /* right boundary found */
      tmax /= 2.0;
    }
    /* check that boundaries still valid */
    if(tmin >= tmax) throw optException(__LINE__, __FILE__, tmin, tmax);
  }

  /* check that last value agrees with GP parameterization */
  if(gpsep->g != tnew) newparamsGPsepLm(gplm, gpsep->d, tnew);

  /* possible print message and return */

  *its += info.its;
  return(tnew);
}

double mleGPsepLm_nug(GPsepLm* gplm, double tmin, double tmax, double *ab,
		       int verb, int *its)
{
  double tnew, dllik, d2llik, llik_init, llik_new, adj, rat;
  double th;
  double *gab, *dab;
  int restoredKGP;
  GPsep *gpsep = gplm->gpsep;
  /* set priors based on Theta */
  dab = NULL;
  gab = ab;

  /* initialization */
  *its = 0;
  restoredKGP = 0;
  th = gpsep->g;

  /* check how close we are to tmin */
  if(fabs(th - tmin) < SDEPS) {
    goto alldone;
  }

  /* initial likelihood calculation */
  llik_init = llikGPsepLm(gplm, dab, gab);

  while(1) { /* checking for improved llik */
    while(1) {  /* Newton step(s) */
      llik_new = 0.0-DBL_MAX;
      while(1) {  /* Newton proposal */

        /* calculate first and second derivatives */
        dllikGPsepLm_nug(gplm, gab, &dllik, &d2llik);

        /* check for convergence by root */
        if(fabs(dllik) < SDEPS) {
          if(*its == 0) {
            goto alldone;
          } else goto newtondone;
        }

        /* Newton update */
        rat = dllik/d2llik; adj = 1.0; (*its)++;

        /* check if we're going the right way */
        if((dllik < 0 && rat < 0) || (dllik > 0 && rat > 0)) {
          if(!gpsep->dK && restoredKGP == 0) {
            deletedKGPsep(gpsep); restoredKGP = 1;
          }
          th = Ropt_sepLm_nug(gplm, tmin, tmax, ab, "[slip]", its, verb); goto mledone;
        } else tnew = th - adj*rat;  /* right way: Newton: */

        /* check that we haven't proposed a tnew out of range */
        while((tnew <= tmin || tnew >= tmax) && adj > SDEPS) {
          adj /= 2.0; tnew = th - adj*rat;
        }

        /* if still out of range, restart? */
        if(tnew <= tmin || tnew >= tmax) {
          if(!gpsep->dK && restoredKGP == 0) {
            deletedKGPsep(gpsep); restoredKGP = 1;
          }
          th = Ropt_sepLm_nug(gplm, tmin, tmax, ab, "[range]", its, verb);
          goto mledone;
        } else break;
      } /* end inner while -- Newton proposal */

      /* else, resets gpsep->g = tnew */
      if(!gpsep->dK && restoredKGP == 0) {
        deletedKGPsep(gpsep); restoredKGP = 1;
      }
      newparamsGPsepLm(gplm, gpsep->d, tnew);

      /* check for convergence, and break or update */
      if(fabs(tnew - th) < SDEPS) break;
      else th = tnew;

      /* check for max its */
      if(*its >= 100) {
       goto alldone;
      }
    } /* end middle while -- Newton step */

    /* sanity check check that we went in the right direction */
newtondone:
    llik_new = llikGPsepLm(gplm, dab, gab);
    if(llik_new < llik_init-SDEPS) {
      llik_new = 0.0-DBL_MAX;
      if(!gpsep->dK && restoredKGP == 0) {
        deletedKGPsep(gpsep); restoredKGP = 1;
      }
      th = Ropt_sepLm_nug(gplm, tmin, tmax, ab, "[dir]", its, verb);
      goto mledone;
    } else break;
  } /* outer improved llik check while(1) loop */

  /* capstone progress indicator */
mledone:
  if(!R_FINITE(llik_new)) llik_new = llikGPsepLm(gplm, dab, gab);

  /* return theta-value found */
alldone:
  if(restoredKGP) newdKGPsep(gpsep);
  return th;
}

void jmleGPsepLm(GPsepLm *gplm, int maxit, double *dmin, double *dmax,
		  double *grange, double *dab, double *gab, int verb,
		  int *dits, int *gits, int *dconv)
{
  unsigned int i;
  int dit[2], git;
  char msg[60];
  double *d;
  GPsep *gpsep = gplm->gpsep;
  /* sanity checks */
  assert(gab && dab);
  assert(dmin && dmax && grange);

  /* auxillary space for d-parameter values(s) */
  d = new_vector(gpsep->m);

  /* loop over coordinate-wise iterations */
  *dits = *gits = 0;
  for(i=0; i<100; i++) {
    mleGPsepLm(gplm, dmin, dmax, dab, maxit, verb, d, dit, msg, 60, dconv);
    if(dit[1] > dit[0]) dit[0] = dit[1];
    *dits += dit[0];
    mleGPsepLm_nug(gplm, grange[0], grange[1], gab, verb, &git);
    *gits += git;
    if((git <= 2 && (dit[0] <= (int)(gpsep->m+1) && *dconv == 0)) || *dconv > 1) break;
  }
  /* clean up */
  free(d);
}

void predGPsepLm_lite(GPsepLm* gplm, unsigned int nn, double **XX, double **HH,
		       double* mean, double *sigma2, double *df, double *llik)
{
  unsigned int i, n, p;
  int info;
  double **k, **ktKi, *ktKik;
  double **hoffset, **HtKiHi, **Chol, *HtKiHio;
  double psidf, qoffset, g;
  GPsep *gpsep;
  gpsep = gplm -> gpsep;
  n = gpsep -> n;
  p = gplm -> p;
  g = gplm -> gpsep -> g;
  assert(df);
  *df = (double) (n-p);
  new_predutilGPsep_lite(gpsep, nn, XX, &k, &ktKi, &ktKik);
  linalg_dgemv(CblasNoTrans,nn,n,1.0,k,nn,gplm->Kires,1,0.0,mean,1);
  linalg_dgemv(CblasTrans,p,nn,1.0,HH,p,gplm->regcoef,1,1.0,mean,1);
  if(sigma2)
  {
    Chol = new_dup_matrix(gplm->HtKiH,p,p);
    HtKiHi = new_id_matrix(p);
    info = linalg_dposv(p,Chol,HtKiHi); /* may need check info */
    if(info){
      free(ktKik);
      delete_matrix(k);
      delete_matrix(ktKi);
      delete_matrix(Chol);
      delete_matrix(HtKiHi);
      throw cholException(__LINE__, __FILE__, info, gplm->gpsep->m, gplm->gpsep->g, gplm->gpsep->d); 
    }
    hoffset = new_dup_matrix(HH,nn,p);
    linalg_dgemm(CblasNoTrans,CblasTrans,p,nn,n,-1.0,gplm->KiH,
		 p,k,nn,1.0,hoffset,p);
    HtKiHio = new_vector(p);
    psidf = gplm->psi/(*df);
    for(i=0; i<nn; ++i)
    {
      linalg_dsymv(p,1.0,HtKiHi,p,hoffset[i],1,0.0,HtKiHio,1);
      qoffset = linalg_ddot(p,hoffset[i],1,HtKiHio,1);
      sigma2[i] = psidf * (1.0 + g - ktKik[i] + qoffset);
    }
    delete_matrix(Chol);
    delete_matrix(HtKiHi);
    delete_matrix(hoffset);
    free(HtKiHio);
  }
  if(llik)
  {
    *llik = 0.0 - 0.5 * (*df) * log(0.5 * gplm->psi);
    *llik -= 0.5 * (gplm->gpsep->ldetK);
    *llik -= 0.5 * gplm->ldetHtKiH;
  }
  delete_matrix(k);
  delete_matrix(ktKi);
  free(ktKik);
}
