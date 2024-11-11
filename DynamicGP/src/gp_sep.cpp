/****************************************************************************
 *
 * Local Approximate Gaussian Process Regression
 * Copyright (C) 2013, The University of Chicago
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 *
 * Questions? Contact Robert B. Gramacy (rbg@vt.edu)
 *
 ****************************************************************************/

#include "gp_sep.hpp"
#include <cassert>
#include <cstdlib>
#include <cmath>
#include <cfloat>
#include <R.h>
#include <Rmath.h>
#include "exceptions.hpp"
extern "C"{
#include "util.h"
#include "matrix.h"
#include "linalg.h"
#include "covar_sep.h"
#include "lbfgsb.h"
}
using std::free;

#define SDEPS sqrt(DBL_EPSILON)

void deletedKGPsep(GPsep *gpsep)
{
  unsigned int k;
  if(gpsep->dK) {
    for(k=0; k<gpsep->m; k++) {
      assert(gpsep->dK[k]);
      delete_matrix(gpsep->dK[k]);
    }
    free(gpsep->dK);
  }
}


/*
 * deleteGPsep:
 *
 * free the memory allocated to a separable gp structure
 *
 * similar to deleteGP except loops over dK
 */

void deleteGPsep(GPsep* gpsep)
{
  assert(gpsep);
  if(gpsep->X) delete_matrix(gpsep->X);
  if(gpsep->Z) free(gpsep->Z);
  if(gpsep->K) delete_matrix(gpsep->K);
  if(gpsep->Ki) delete_matrix(gpsep->Ki);
  if(gpsep->KiZ) free(gpsep->KiZ);
  deletedKGPsep(gpsep);
  if(gpsep->d) free(gpsep->d);
  free(gpsep);
}

/*
 * calc_ZtKiZ_sep:
 *
 * re-calculates phi = ZtKiZ from Ki and Z stored in
 * the GP object; also update KiZ on which it depends
 *
 * SAME as gp.c but uses GPsep instead
 */

void calc_ZtKiZ_sep(GPsep *gpsep)
{
  assert(gpsep);
  /* phi <- t(Z) %*% Ki %*% Z */
  if(gpsep->KiZ == NULL) gpsep->KiZ = new_vector(gpsep->n);
  linalg_dsymv(gpsep->n,1.0,gpsep->Ki,gpsep->n,gpsep->Z,1,0.0,gpsep->KiZ,1);
  gpsep->phi = linalg_ddot(gpsep->n, gpsep->Z, 1, gpsep->KiZ, 1);
}

/*
 * newdKGPsep:
 *
 * allocate new space for dK and d2K calculations, and
 * calculate derivatives
 *
 * similar to newdKGP except no 2nd derivatives or fishinfo
 */

void newdKGPsep(GPsep *gpsep)
{
  unsigned int j;
  assert(gpsep->dK == NULL);
  gpsep->dK = (double ***) malloc(sizeof(double **) * gpsep->m);
  for(j=0; j<gpsep->m; j++) gpsep->dK[j] = new_matrix(gpsep->n, gpsep->n);
  diff_covar_sep_symm(gpsep->m, gpsep->X, gpsep->n, gpsep->d, gpsep->K,
		      gpsep->dK);
}

/*
 * buildGPsep:
 *
 * intended for newly created separable GPs, e.g., via newGPsep
 * does all of the correlation calculations, etc., after data and
 * parameters are defined
 *
 * similar to buildGP except calculates gradient dK
 */

GPsep* buildGPsep(GPsep *gpsep, const int dK)
{
  double **Kchol, **X;
  unsigned int n, m;
  int info;

  assert(gpsep && gpsep->K == NULL);
  n = gpsep->n;
  m = gpsep->m;
  X = gpsep->X;

  /* build covari ance matrix */
  gpsep->K = new_matrix(n, n);
  covar_sep_symm(m, X, n, gpsep->d, gpsep->g, gpsep->K);

  /* invert covariance matrix */
  gpsep->Ki = new_id_matrix(n);
  Kchol = new_dup_matrix(gpsep->K, n, n);
  info = linalg_dposv(n, Kchol, gpsep->Ki);
  if(info) {
    throw cholException(__LINE__, __FILE__, info, m, gpsep->g, gpsep->d);
  }
  gpsep->ldetK = log_determinant_chol(Kchol, n);
  delete_matrix(Kchol);

  /* phi <- t(Z) %*% Ki %*% Z */
  gpsep->KiZ = NULL;
  calc_ZtKiZ_sep(gpsep);

  /* calculate derivatives ? */
  gpsep->dK = NULL;
  if(dK) newdKGPsep(gpsep);

  /* return new structure */
  return(gpsep);
}


/*
 * newGPsep:
 *
 * allocate a new separable GP structure using the data and parameters
 * provided
 *
 * similar to  newGP except gpseps and pointer to d, and does not have dK
 * flag since gradient is always calculated
 */

GPsep* newGPsep(const unsigned int m, const unsigned int n, double **X,
	  double *Z, double *d, const double g, const int dK)
{
  GPsep* gpsep;

  /* new gp structure */
  gpsep = (GPsep*) malloc(sizeof(GPsep));
  gpsep->m = m;
  gpsep->n = n;
  gpsep->X = new_dup_matrix(X, n, m);
  gpsep->Z = new_dup_vector(Z, n);
  gpsep->d = new_dup_vector(d, m);
  gpsep->g = g;
  gpsep->K = NULL;
  gpsep->dK = NULL;
  return buildGPsep(gpsep, dK);
}

/*
 * llikGPsep:
 *
 * calculate and return the log marginal likelihood
 *
 * similar to llikGPsep except loops over separable d for
 * prior calculation
 */

double llikGPsep(GPsep *gpsep, double *dab, double *gab)
{
  unsigned int k;
  double llik;

  /* proportional to likelihood calculation */
  llik = 0.0 - 0.5*(((double) gpsep->n) * log(0.5 * gpsep->phi) + gpsep->ldetK);
  /* llik += lgamma(0.5*((double) gpsep->n)) - ((double) gpsep->n)*M_LN_SQRT_2PI; */

  /* if priors are being used; for lengthscale */
  if(dab && dab[0] > 0 && dab[1] > 0) {
    for(k=0; k<gpsep->m; k++) {
      if(gpsep->d[k] > 0) llik += dgamma(gpsep->d[k], dab[0], 1.0/dab[1], 1);
    }
  }

  /* if priors are being used; for nugget */
  if(gpsep->g > 0 && gab && gab[0] > 0 && gab[1] > 0)
    llik += dgamma(gpsep->g, gab[0], 1.0/gab[1], 1);

  return(llik);
}


/*
 * dllikGPsep:
 *
 * batch calculation of the gradient of the log likelihood
 * of a separable gp, with respect to the
 * lengthscale parameter, d; requires that derivatives
 * be pre-calculated
 *
 * substantially changed from dllikGP and removed d2llik
 */

void dllikGPsep(GPsep *gpsep, double *ab, double *dllik)
{
  double *KiZtwo;
  unsigned int i, j, n, k;
  double dn, phirat ;

  /* sanity check */
  assert(gpsep->dK);
  assert(dllik);

  /* copy dims for fast access */
  n = gpsep->n;
  dn = (double) n;

  KiZtwo = new_vector(n);
  for(k=0; k<gpsep->m; k++) {

    /* deal with possible prior */
    if(ab && ab[0] > 0 && ab[1] > 0) {
      dllik[k] = (ab[0] - 1.0)/gpsep->d[k] - ab[1];
    } else dllik[k] = 0.0;

    /* dllik = - 0.5 * tr(Ki %*% dK) */
    for(i=0; i<n; i++) {
      for(j=0; j<i; j++) /* off diagonal */
        dllik[k] -= gpsep->Ki[i][j] * gpsep->dK[k][i][j];

      /* on-diagonal */
      dllik[k] -= 0.5 * gpsep->Ki[i][i] * gpsep->dK[k][i][i];
    }

    /* now third part of the expression, re-using KiZtwo */
    /* KiZtwo = dK %*% KiZ */
    linalg_dsymv(n,1.0,gpsep->dK[k],n,gpsep->KiZ,1,0.0,KiZtwo,1);
    /* now t(KiZ) %*% dK %*% KiZ */
    phirat = linalg_ddot(n, gpsep->KiZ, 1, KiZtwo, 1) / gpsep->phi;
    dllik[k] += 0.5*dn*phirat;
  }

  /* clean up */
  free(KiZtwo);
}

/*
 * dllikGPsep_nug:
 *
 * batch calculation of the first derivative
 * of the log likelihood of a gp, with respect to the
 * NUGGET parameter, g
 *
 */

void dllikGPsep_nug(GPsep *gpsep, double *ab, double *dllik, double *d2llik)
{
  unsigned int i, j, n;
  double *KiZtwo;
  double **two, **dKKidK;
  double dn, phirat, dlp, d2lp;

  /* sanity check */
  assert(dllik);

  /* deal with possible prior */
  if(ab && ab[0] > 0 && ab[1] > 0) {
    dlp = (ab[0] - 1.0)/gpsep->g - ab[1];
    d2lp = 0.0 - (ab[0] - 1.0)/sq(gpsep->g);
  } else dlp = d2lp = 0.0;

  /* copy dims for fast access */
  n = gpsep->n;
  dn = (double) n;

  if(d2llik) {
    two = new_matrix(n, n);
    dKKidK = gpsep->Ki;
  } else two = dKKidK = NULL;

  /* d2llik = - 0.5 * tr(Ki %*% [0.0 - Ki]); the first expression */
  /* dllik = - 0.5 * tr(Ki) */
  if(d2llik) *d2llik = d2lp;
  *dllik = dlp;
  for(i=0; i<n; i++) {
    if(d2llik) {
      for(j=0; j<i; j++) { /* off diagonal */
        *d2llik += gpsep->Ki[i][j] * dKKidK[i][j];
        two[i][j] = two[j][i] = 2.0*dKKidK[i][j];
      }
    }
    /* on-diagonal */
    *dllik -= 0.5 * gpsep->Ki[i][i];
    if(d2llik) {
      *d2llik += 0.5 * gpsep->Ki[i][i] * dKKidK[i][i];
      two[i][i] = 2.0*dKKidK[i][i];
    }
  }

  /* now the second part of the expression: */
  /* d2llik -= 0.5 * KiZ %*% two %*% KiZ */
  if(d2llik) {
    KiZtwo = new_vector(n);
    linalg_dsymv(n,1.0,two,n,gpsep->KiZ,1,0.0,KiZtwo,1);
    *d2llik -= 0.5*dn*linalg_ddot(n, gpsep->KiZ, 1, KiZtwo, 1) / gpsep->phi;
    free(KiZtwo);
  }

  /* now third part of the expression, re-using KiZtwo */
  /* now t(KiZ) %*% dK %*% KiZ */
  phirat = linalg_ddot(n, gpsep->KiZ, 1, gpsep->KiZ, 1) / gpsep->phi;
  if(d2llik) *d2llik += 0.5*dn*sq(phirat);
  *dllik += 0.5*dn*phirat;

  /* clean up */
  if(two) delete_matrix(two);
}

/*
 * newparamsGPsep:
 *
 * change the lengthscale and nugget parameters to the gp
 *
 * SIMIAR to newparamsGP except vectorized d and always does
 * gradient
 */

void newparamsGPsep(GPsep* gpsep, double *d, const double g)
{
  int info, m, n;
  double **Kchol;

  /* sanity check */
  assert(g >= 0);

  /* build covariance matrix */
  m = gpsep->m; n = gpsep->n;
  dupv(gpsep->d, d, m);
  gpsep->g = g;
  covar_sep_symm(m, gpsep->X, n, gpsep->d, gpsep->g, gpsep->K);

  /* invert covariance matrix */
  id(gpsep->Ki, n);
  Kchol = new_dup_matrix(gpsep->K, n, n);
  info = linalg_dposv(n, Kchol, gpsep->Ki);
  if(info) {
    delete_matrix(Kchol);
    throw cholException(__LINE__, __FILE__, info, m, gpsep->g, gpsep->d);
  }
  gpsep->ldetK = log_determinant_chol(Kchol, n);
  delete_matrix(Kchol);

  /* phi <- t(Z) %*% Ki %*% Z */
  calc_ZtKiZ_sep(gpsep);

  /* calculate derivatives ? */
  if(gpsep->dK)
    diff_covar_sep_symm(gpsep->m, gpsep->X, gpsep->n, gpsep->d,
      gpsep->K, gpsep->dK);
}


/*
 * utility structure for fcn_nllik_sep_nug defined below
 * for use with Brent_fmin (R's optimize) or uniroot
 *
 * SIMPLIFIED compared to callinfo in gp.c because it only does the nugget
 */

struct callinfo_sep_nug {
  GPsep *gpsep;
  double *ab;
  int its;
  int verb;
};


/*
 * fcn_nllik_sep_nug:
 *
 * a utility function for Brent_fmin (R's optimize) to apply to the separable
 * GP log likelihood after changes to the nugget parameter
 *
 * SIMPLIFIED compared to fcn_nllik in gp.c since it only does the nugget
 */

static double fcn_nllik_sep_nug(double x, struct callinfo_sep_nug *info)
{
  double llik;
  (info->its)++;
  newparamsGPsep(info->gpsep, info->gpsep->d, x);
  llik = llikGPsep(info->gpsep, NULL, info->ab);
  return 0.0-llik;
}

/*
 * Ropt_sep_nug:
 *
 * use R's Brent Fmin routine (from optimize) to optimize
 *
 * SIMPLIFIED compared to Ropt in GP because it only does the nugget
 */

double Ropt_sep_nug(GPsep* gpsep, double tmin, double tmax,
		    double *ab, const char *msg, int *its, int verb)
{
  double tnew;
  double Tol = SDEPS;

  /* sanity check */
  assert(tmin < tmax);

  /* get parameter */
  /*th = gpsep->g;*/

  /* create structure for Brent_fmin */
  struct callinfo_sep_nug info;
  info.gpsep = gpsep;
  info.ab = ab;
  info.its = 0;
  info.verb = verb;

  /* call the C-routine behind R's optimize function */
  while(1) { /* check to make sure solution is not on boundary */
   tnew = Brent_fmin(tmin, tmax, (double (*)(double, void*)) fcn_nllik_sep_nug, &info, Tol);
   if(tnew > tmin && tnew < tmax) break;
   if(tnew == tmin) { /* left boundary found */
    tmin *= 2;
   } else { /* right boundary found */
    tmax /= 2.0;
  }
  /* check that boundaries still valid */
   if(tmin >= tmax) throw optException(__LINE__, __FILE__, tmin, tmax);
  }

  /* check that last value agrees with GP parameterization */
  if(gpsep->g != tnew) newparamsGPsep(gpsep, gpsep->d, tnew);

  /* possible print message and return */

  *its += info.its;
  return(tnew);
}

/*
 * mleGPsep_nug:
 *
 * calculate the MLE with respect to the lengthscale parameter;
 * derivatives for the Newton method are calculated on the fly
 */

double mleGPsep_nug(GPsep* gpsep, double tmin, double tmax, double *ab,
             int verb, int *its)
{
  double tnew, dllik, d2llik, llik_init, llik_new, adj, rat;
  double th;
  double *gab, *dab;
  int restoredKGP;

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
  llik_init = llikGPsep(gpsep, dab, gab);

  /* initial printing */

  while(1) { /* checking for improved llik */
    while(1) {  /* Newton step(s) */
      llik_new = R_NegInf;
      while(1) {  /* Newton proposal */

        /* calculate first and second derivatives */
        dllikGPsep_nug(gpsep, gab, &dllik, &d2llik);

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
          th = Ropt_sep_nug(gpsep, tmin, tmax, ab, "[slip]", its, verb); goto mledone;
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
          th = Ropt_sep_nug(gpsep, tmin, tmax, ab, "[range]", its, verb);
          goto mledone;
        } else break;
      } /* end inner while -- Newton proposal */

      /* else, resets gpsep->g = tnew */
      if(!gpsep->dK && restoredKGP == 0) {
        deletedKGPsep(gpsep); restoredKGP = 1;
      }
      newparamsGPsep(gpsep, gpsep->d, tnew);


      /* check for convergence, and break or update */
      if(fabs(tnew - th) < SDEPS) break;
      else th = tnew;

      /* check for max its */
      if(*its >= 100) {
        /* could also call Ropt here as last resort */
       goto alldone;
      }
    } /* end middle while -- Newton step */

    /* sanity check check that we went in the right direction */
newtondone:
    llik_new = llikGPsep(gpsep, dab, gab);
    if(llik_new < llik_init-SDEPS) {
      llik_new = R_NegInf;
      if(!gpsep->dK && restoredKGP == 0) {
        deletedKGPsep(gpsep); restoredKGP = 1;
      }
      th = Ropt_sep_nug(gpsep, tmin, tmax, ab, "[dir]", its, verb);
      goto mledone;
    } else break;
  } /* outer improved llik check while(1) loop */

  /* capstone progress indicator */
mledone:
  if(!R_FINITE(llik_new)) llik_new = llikGPsep(gpsep, dab, gab);

  /* return theta-value found */
alldone:
  if(restoredKGP) newdKGPsep(gpsep);
  return th;
}

struct mycallinfo_sep {
  GPsep *gpsep;
  double *dab;
  double *gab;
  int its;   /* updated but not used since lbfgsb counts fmin and gr evals */
  int verb;
};

static double nllik_sep(int n, double *p, struct mycallinfo_sep *info)
{
  double llik;
  int psame, k;

  /* sanity check */
  // m = info->gpsep->m;
  assert(n == info->gpsep->m);

  /* check if parameters in p are new */
  psame = 1;
  for(k=0; k<n; k++) {
    if(p[k] != info->gpsep->d[k]) { psame = 0; break; }
  }

  /* update GP with new parameters */
  if(!psame) {
    (info->its)++;
    newparamsGPsep(info->gpsep, p, info->gpsep->g);
  }

  /* evaluate likelihood with potentially new paramterization */
  llik = llikGPsep(info->gpsep, info->dab, info->gab);

  /* done */
  return 0.0-llik;
}

static void ndllik_sep(int n, double *p, double *df, struct mycallinfo_sep *info)
{
  int dsame, k;

  /* sanity check */
  assert(n == info->gpsep->m);

  /* check if parameters in p are new */
  dsame = 1;
  for(k=0; k<n; k++) if(p[k] != info->gpsep->d[k]) { dsame = 0; break; }

  /* update GP with new parameters */
  if(!dsame) {
    (info->its)++;
    newparamsGPsep(info->gpsep, p, info->gpsep->g);
  }

  /* evaluate likelihood with potentially new paramterization */
  dllikGPsep(info->gpsep, info->dab, df);

  /* negate values */
  for(k=0; k<n; k++) df[k] = 0.0-df[k];

}
void mymleGPsep(GPsep* gpsep, double* dmin, double *dmax, double *ab,
		const unsigned int maxit, int verb, double *p, int *its,
		char *msg, unsigned int msg_size, int *conv)
{
  int lbfgs_verb;
  unsigned int k;
  double rmse;
  double *dold;

  /* create structure for Brent_fmin */
  struct mycallinfo_sep info;
  info.gpsep = gpsep;
  info.dab = ab;
  info.gab = NULL;
  info.its = 0;
  info.verb = verb-6;

  /* copy the starting value */
  dupv(p, gpsep->d, gpsep->m);
  dold = new_dup_vector(gpsep->d, gpsep->m);

  /* set ifail argument and verb/trace arguments */
  *conv = 0;
  if(verb <= 1) lbfgs_verb = 0;
  else lbfgs_verb = verb - 1;

  /* call the C-routine behind R's optim function with method = "L-BFGS-B" */
  lbfgsb_C(gpsep->m, p, dmin, dmax, (lbfgsb_fmin) nllik_sep,
	   (lbfgsb_fgrad) ndllik_sep, conv, (void*) &info,
	   SDEPS, its, maxit, msg, lbfgs_verb);

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

  /* clean up */
  free(dold);
}

void myjmleGPsep(GPsep *gpsep, int maxit, double *dmin, double *dmax,
		 double *grange, double *dab, double *gab, int verb,
		 int *dits, int *gits, int *dconv)
{
  unsigned int i;
  int dit[2], git;
  char msg[60];
  double *d;

  /* sanity checks */
  assert(gab && dab);
  assert(dmin && dmax && grange);

  /* auxillary space for d-parameter values(s) */
  d = new_vector(gpsep->m);

  /* loop over coordinate-wise iterations */
  *dits = *gits = 0;
  for(i=0; i<100; i++) {
    mymleGPsep(gpsep, dmin, dmax, dab, maxit, verb, d, dit, msg, 60, dconv);
    if(dit[1] > dit[0]) dit[0] = dit[1];
    *dits += dit[0];
    mleGPsep_nug(gpsep, grange[0], grange[1], gab, verb, &git);
    *gits += git;
    if((git <= 2 && (dit[0] <= (int)(gpsep->m+1) && *dconv == 0)) || *dconv > 1) break;
  }
  /* clean up */
  free(d);
}

/*
 * updateGPsep:
 *
 * quickly augment (O(n^2)) a gp based on new X-Z pairs.
 * Uses the Bartlet partition inverse equations
 */

void updateGPsep(GPsep* gpsep, unsigned int nn, double **XX, double *ZZ,
                 int verb)
{
  unsigned int i, j, l, n, m;
  double *kx, *x, *gvec;
  double mui, Ztg;
  double **Gmui, **temp;

  /* allocate space */
  n = gpsep->n; m = gpsep->m;
  kx = new_vector(n);
  gvec = new_vector(n);
  Gmui = new_matrix(n, n);
  temp = new_matrix(1, 1);

  /* for each new location */
  for(j=0; j<nn; j++) {

    /* shorthand for x being updated */
    x = XX[j];

    /* calculate the Bartlet quantities */
    calc_g_mui_kxy_sep(m, x, gpsep->X, n, gpsep->Ki, NULL, 0, gpsep->d,
                       gpsep->g, gvec, &mui, kx, NULL);

    /* Gmui = g %*% t(g)/mu */
    linalg_dgemm(CblasNoTrans,CblasTrans,n,n,1,
     mui,&gvec,n,&gvec,n,0.0,Gmui,n);

    /* Ki = Ki + Gmui */
    linalg_daxpy(n*n, 1.0, *Gmui, 1, *(gpsep->Ki), 1);

    /* now augment covariance matrices */
    /* for nn > 1 might be better to make bigger matricies once
       outside the for-loop */
    gpsep->Ki = new_bigger_matrix(gpsep->Ki, n, n, n+1, n+1);
    for(i=0; i<n; i++) gpsep->Ki[n][i] = gpsep->Ki[i][n] = gvec[i];
    gpsep->Ki[n][n] = 1.0/mui;
    gpsep->K = new_bigger_matrix(gpsep->K, n, n, n+1, n+1);
    for(i=0; i<n; i++) gpsep->K[n][i] = gpsep->K[i][n] = kx[i];
    covar_sep_symm(m, &x, 1, gpsep->d, gpsep->g, temp);
    gpsep->K[n][n] = **temp;

    /* update the determinant calculation */
    gpsep->ldetK += log(**temp + mui * linalg_ddot(n, gvec, 1, kx, 1));

    /* update KiZ and phi */
    /* Ztg = t(Z) %*% gvec */
    Ztg = linalg_ddot(n, gvec, 1, gpsep->Z, 1);
    gpsep->KiZ = (double*) realloc(gpsep->KiZ, sizeof(double)*(n+1));
    /* KiZ[1:n] += (Ztg/mu + Z*g) * gvec */
    linalg_daxpy(n, Ztg*mui + ZZ[j], gvec, 1, gpsep->KiZ, 1);
    /* KiZ[n+1] = Ztg + z*mu */
    gpsep->KiZ[n] = Ztg + ZZ[j]/mui;
    /* phi += Ztg^2/mu + 2*z*Ztg + z^2*mu */
    gpsep->phi += sq(Ztg)*mui + 2.0*ZZ[j]*Ztg + sq(ZZ[j])/mui;

    /* now augment X and Z */
    gpsep->X = new_bigger_matrix(gpsep->X, n, m, n+1, m);
    dupv(gpsep->X[n], x, m);
    gpsep->Z = (double*) realloc(gpsep->Z, sizeof(double)*(n+1));
    gpsep->Z[n] = ZZ[j];
    (gpsep->n)++;

    /* augment derivative covariance matrices */
    if(gpsep->dK) {
      for(l=0; l<m; l++)
        gpsep->dK[l] = new_bigger_matrix(gpsep->dK[l], n, n, n+1, n+1);
      double ***dKn = (double***) malloc(sizeof(double **) * m);
      for(l=0; l<m; l++) dKn[l] = new_matrix(1, n);
      diff_covar_sep(m, &x, 1, gpsep->X, n, gpsep->d, &(gpsep->K[n]), dKn);
      for(l=0; l<m; l++) {
        for(i=0; i<n; i++)
          gpsep->dK[l][i][n] = gpsep->dK[l][n][i] = dKn[l][0][i];
        delete_matrix(dKn[l]);
      }
      free(dKn);
      for(l=0; l<m; l++) gpsep->dK[l][n][n] = 0.0;
    }

    /* if more then re-allocate */
    if(j < nn-1) {
      kx = (double*) realloc(kx, sizeof(double)*(n+1));
      gvec = (double*) realloc(gvec, sizeof(double)*(n+1));
      Gmui = new_bigger_matrix(Gmui, n, n, n+1, n+1);
    }

    n = gpsep->n; /* increment for next interation */
  }

  /* clean up */
  delete_matrix(Gmui);
  free(kx);
  free(gvec);
  delete_matrix(temp);
}

void pred_generic(const unsigned int n, const double phidf, double *Z,
		  double **Ki, const unsigned int nn, double **k,
		  double *mean, double **Sigma)
{
  unsigned int i, j;
  double **ktKi, **ktKik;

  /* ktKi <- t(k) %*% util$Ki */
  ktKi = new_matrix(n, nn);
  linalg_dsymm(CblasRight,nn,n,1.0,Ki,n,k,nn,0.0,ktKi,nn);
  /* ktKik <- ktKi %*% k */
  ktKik = new_matrix(nn, nn);
  linalg_dgemm(CblasNoTrans,CblasTrans,nn,nn,n,
               1.0,k,nn,ktKi,nn,0.0,ktKik,nn);

  /* mean <- ktKi %*% Z */
  linalg_dgemv(CblasNoTrans,nn,n,1.0,ktKi,nn,Z,1,0.0,mean,1);

  /* Sigma <- phi*(Sigma - ktKik)/df */
  for(i=0; i<nn; i++) {
    Sigma[i][i] = phidf * (Sigma[i][i] - ktKik[i][i]);
    for(j=0; j<i; j++)
      Sigma[j][i] = Sigma[i][j] = phidf * (Sigma[i][j] - ktKik[i][j]);
  }

  /* clean up */
  delete_matrix(ktKi);
  delete_matrix(ktKik);
}

/*
 * new_predutil_generic_lite:
 *
 * a function allocates space and calculate portions of the GP predictive
 * equations without reference struct gp objects.  Created so that code can
 * be shared between GP and GPsep objects, and beyond
 */

void new_predutil_generic_lite(const unsigned int n, double ** Ki,
  const unsigned int nn, double **k, double ***ktKi, double **ktKik)
{
  unsigned int i, j;

  /* ktKi <- t(k) %*% util$Ki */
  *ktKi = new_matrix(n, nn);
  linalg_dsymm(CblasRight,nn,n,1.0,Ki,n,k,nn,0.0,*ktKi,nn);
  /* ktKik <- diag(ktKi %*% k) */
  *ktKik = new_zero_vector(nn);
  for(i=0; i<nn; i++) for(j=0; j<n; j++) (*ktKik)[i] += (*ktKi)[j][i]*k[j][i];
}

/*
 * new_predutilGPsep_lite:
 *
 * utility function that allocates and calculate useful vectors
 * and matrices for prediction; used by predGPsep_lite and dmus2GP
 */

void new_predutilGPsep_lite(GPsep *gpsep, unsigned int nn, double **XX,
			    double ***k, double ***ktKi, double **ktKik)
{
  /* k <- covar(X1=X, X2=XX, d=Zt$d, g=0) */
  *k = new_matrix(gpsep->n, nn);
  covar_sep(gpsep->m, gpsep->X, gpsep->n, XX, nn, gpsep->d, *k);

  /* call generic function that would work for all GP covariance specs */
  new_predutil_generic_lite(gpsep->n, gpsep->Ki, nn, *k, ktKi, ktKik);
}


/*
 * predGPsep_lite:
 *
 * return the student-t predictive equations,
 * i.e., parameters to a multivatiate t-distribution
 * for XX predictive locations of dimension (n*m);
 * lite because sigma2 not Sigma is calculated
 */

void predGPsep_lite(GPsep* gpsep, unsigned int nn, double **XX, double *mean,
		    double *sigma2, double *df, double *llik)
{
  unsigned int i;
  double **k, **ktKi;
  double *ktKik;
  double phidf;

  /* sanity checks */
  assert(df);
  *df = gpsep->n;

  /* utility calculations */
  new_predutilGPsep_lite(gpsep, nn, XX, &k, &ktKi, &ktKik);

  /* mean <- ktKi %*% Z */
  if(mean) linalg_dgemv(CblasNoTrans,nn,gpsep->n,1.0,ktKi,nn,gpsep->Z,
                        1,0.0,mean,1);

  /* Sigma <- phi*(Sigma - ktKik)/df */
  /* *df = n - m - 1.0; */  /* only if estimating beta */
  if(sigma2) {
    phidf = gpsep->phi/(*df);
    for(i=0; i<nn; i++) {
      sigma2[i] = phidf * (1.0 + gpsep->g - ktKik[i]);
      if(sigma2[i] < 0.0) sigma2[i] = 0.0;
    }
  }

  /* calculate marginal likelihood (since we have the bits) */
  /* might move to updateGP if we decide to move phi to updateGP */
  if(llik) *llik = 0.0 - 0.5*(((double) gpsep->n) * log(0.5* gpsep->phi) +
    gpsep->ldetK);
  /* continuing: - ((double) n)*M_LN_SQRT_2PI;*/

  /* clean up */
  delete_matrix(k);
  delete_matrix(ktKi);
  free(ktKik);
}

/*
 * alcGPsep:
 *
 * return s2' component of the ALC calculation of the
 * expected reduction in variance calculation at locations
 * Xcand averaging over reference locations Xref:
 * ds2 = s2 - s2', where the s2s are at Xref and the
 * s2' incorporates Xcand, and everything is averaged
 * over Xref.
 */

void alcGPsep(GPsep *gpsep, unsigned int ncand, double **Xcand,
	      unsigned int nref, double **Xref,  int verb, double *alc)
{
  unsigned int m, n, i;
  double **k; //, **Gmui;
  double *kx, *kxy, *gvec, *ktKikx; //, *ktGmui;
  double mui, df;
  double s2p[2] = {0, 0};

  /* degrees of freedom */
  m = gpsep->m;
  n = gpsep->n;
  df = (double) n;

  /* allocate g, kxy, and ktKikx vectors */
  gvec = new_vector(n);
  kxy = new_vector(nref);
  kx = new_vector(n);
  ktKikx = new_vector(nref);

  /* k <- covar(X1=X, X2=Xref, d=Zt$d, g=0) */
  k = new_matrix(nref, n);
  covar_sep(m, Xref, nref, gpsep->X, n, gpsep->d, k);

  /* utility allocations */
  // Gmui = new_matrix(n, n);
  // ktGmui = new_vector(n);

  /* calculate the ALC for each candidate */
  for(i=0; i<ncand; i++) {

    /* calculate the g vector, mui, and kxy */
    calc_g_mui_kxy_sep(m, Xcand[i], gpsep->X, n, gpsep->Ki, Xref, nref,
      gpsep->d, gpsep->g, gvec, &mui, kx, kxy);

    /* skip if numerical problems */
    if(mui <= SDEPS) {
      alc[i] = R_NegInf;
      continue;
    }

    /* use g, mu, and kxy to calculate ktKik.x */
    // calc_ktKikx(NULL, nref, k, n, gvec, mui, kxy, Gmui, ktGmui, ktKikx);
    calc_ktKikx(NULL, nref, k, n, gvec, mui, kxy, NULL, NULL, ktKikx);

    /* calculate the ALC */
    alc[i] = calc_alc(nref, ktKikx, s2p, gpsep->phi, NULL, df, NULL);
  }

  /* clean up */
  // delete_matrix(Gmui);
  // free(ktGmui);
  free(ktKikx);
  free(gvec);
  free(kx);
  free(kxy);
  delete_matrix(k);
}
