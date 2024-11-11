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

#include <stdlib.h>
#include <math.h>
#include <float.h>
#include "util.h"
#include "matrix.h"
#include "linalg.h"
#include <assert.h>
#include <Rmath.h>

/*
 * log_determinant_chol:
 *
 * returns the log determinant of the n x n
 * choleski decomposition of a matrix M
 */

double log_determinant_chol(double **M, const unsigned int n)
{
  double log_det;
  unsigned int i;

  /* det = prod(diag(R)) .^ 2 */
  log_det = 0;
  for(i=0; i<n; i++) log_det += log(M[i][i]);
  log_det = 2*log_det;

  return log_det;
}


/* the C-function behind uniroot from R */
double Brent_fmin(double ax, double bx, double (*f)(double, void *),
		  void *info, double tol)
{
    /*  c is the squared inverse of the golden ratio */
    const double c = (3. - sqrt(5.)) * .5;

    /* Local variables */
    double a, b, d, e, p, q, r, u, v, w, x;
    double t2, fu, fv, fw, fx, xm, eps, tol1, tol3;

/*  eps is approximately the square root of the relative machine precision. */
    eps = DBL_EPSILON;
    tol1 = eps + 1.;/* the smallest 1.000... > 1 */
    eps = sqrt(eps);

    a = ax;
    b = bx;
    v = a + c * (b - a);
    w = v;
    x = v;

    d = 0.;/* -Wall */
    e = 0.;
    fx = (*f)(x, info);
    fv = fx;
    fw = fx;
    tol3 = tol / 3.;

/*  main loop starts here ----------------------------------- */

    for(;;) {
  xm = (a + b) * .5;
  tol1 = eps * fabs(x) + tol3;
  t2 = tol1 * 2.;

  /* check stopping criterion */

  if (fabs(x - xm) <= t2 - (b - a) * .5) break;
  p = 0.;
  q = 0.;
  r = 0.;
  if (fabs(e) > tol1) { /* fit parabola */

      r = (x - w) * (fx - fv);
      q = (x - v) * (fx - fw);
      p = (x - v) * q - (x - w) * r;
      q = (q - r) * 2.;
      if (q > 0.) p = -p; else q = -q;
      r = e;
      e = d;
  }

  if (fabs(p) >= fabs(q * .5 * r) ||
      p <= q * (a - x) || p >= q * (b - x)) { /* a golden-section step */

      if (x < xm) e = b - x; else e = a - x;
      d = c * e;
  }
  else { /* a parabolic-interpolation step */

      d = p / q;
      u = x + d;

      /* f must not be evaluated too close to ax or bx */

      if (u - a < t2 || b - u < t2) {
    d = tol1;
    if (x >= xm) d = -d;
      }
  }

  /* f must not be evaluated too close to x */

  if (fabs(d) >= tol1)
      u = x + d;
  else if (d > 0.)
      u = x + tol1;
  else
      u = x - tol1;

  fu = (*f)(u, info);

  /*  update  a, b, v, w, and x */

  if (fu <= fx) {
      if (u < x) b = x; else a = x;
      v = w;    w = x;   x = u;
      fv = fw; fw = fx; fx = fu;
  } else {
      if (u < x) a = u; else b = u;
      if (fu <= fw || w == x) {
        v = w; fv = fw;
        w = u; fw = fu;
      } else if (fu <= fv || v == x || v == w) {
        v = u; fv = fu;
      }
  }
    }
    /* end of main loop */

    return x;
}

/*
 * calc_alc:
 *
 * function that iterates over the m Xref locations, and the
 * stats calculated by previous calc_* function in order to 
 * calculate the reduction in variance
 */

double calc_alc(const int m, double *ktKik, double *s2p, const double phi, 
		double *badj, const double tdf, double *w)
{
  int i;
  double zphi, ts2, alc, dfrat;
  
  dfrat = tdf/(tdf - 2.0);
  alc = 0.0;
  for(i=0; i<m; i++) {
    zphi = (s2p[1] + phi)*ktKik[i];
    if(badj) ts2 = badj[i] * zphi / (s2p[0] + tdf);
    else ts2 = zphi / (s2p[0] + tdf);
    if(w) alc += w[i]*dfrat*ts2;
    else alc += ts2*dfrat; 
  }

  return (alc/m);
}


/*
 * calc_ktKikx:
 *
 * function for calculating the ktKikx vector used in the
 * IECI calculation -- writes over the KtKik input --
 * R interface (calc_ktKikx_R) available in plgp source tree
 */

void calc_ktKikx(double *ktKik, const int m, double **k, const int n,
		 double *g, const double mui, double *kxy, double **Gmui,
		 double *ktGmui, double *ktKikx)
{
  int i;
  // double **Gmui;
  // double *ktGmui;

  /* first calculate Gmui = g %*% t(g)/mu */
  // if(!Gmui_util) Gmui = new_matrix(n, n);
  // else Gmui = Gmui_util;
  if(Gmui) {
    linalg_dgemm(CblasNoTrans,CblasTrans,n,n,1,
               mui,&g,n,&g,n,0.0,Gmui,n);
    assert(ktGmui);
  }

  /* used in the for loop below */
  // if(!ktGmui_util) ktGmui = new_vector(n);
  // else ktGmui = ktGmui_util;
  if(ktGmui) assert(Gmui);

  /* loop over all of the m candidates */
  for(i=0; i<m; i++) {

    /* ktGmui = drop(t(k) %*% Gmui) */
    /* zerov(ktGmui, n); */
    if(Gmui) { 
      linalg_dsymv(n,1.0,Gmui,n,k[i],1,0.0,ktGmui,1);

      /* ktKik += diag(t(k) %*% (g %*% t(g) * mui) %*% k) */
      if(ktKik) ktKikx[i] = ktKik[i] + linalg_ddot(n, ktGmui, 1, k[i], 1);
      else ktKikx[i] = linalg_ddot(n, ktGmui, 1, k[i], 1);
    } else {
      if(ktKik) ktKikx[i] = ktKik[i] + sq(linalg_ddot(n, k[i], 1, g, 1))*mui;
      else ktKikx[i] = sq(linalg_ddot(n, k[i], 1, g, 1))*mui;
    }

    /* ktKik.x += + 2*diag(kxy %*% t(g) %*% k) */
    ktKikx[i] += 2.0*linalg_ddot(n, k[i], 1, g, 1)*kxy[i];

    /* ktKik.x + kxy^2/mui */
    ktKikx[i] += sq(kxy[i])/mui;
  }

  /* clean up */
  // if(!ktGmui_util) free(ktGmui);
  // if(!Gmui_util) delete_matrix(Gmui);
}

/*
 * Cgamma:
 *
 * (complete) gamma function and its logarithm (all logarithms are base 10)
 * from UCS
 */

double Cgamma(const double a, const int ulog)
{
  double r;
  if(ulog) r = lgammafn(a) / M_LN10;
  else r = gammafn(a);
  /* MYprintf(MYstdout, "Cgamma: a=%g, ulog=%d, r=%g\n", a, ulog, r); */
  assert(!isnan(r));
  return(r);
}


/*
 * Rgamma_inv:
 *
 * regularized gamma inverse function from UCS
 */

double Rgamma_inv(const double a, const double y, const int lower,
                  const int ulog)
{
  double r;
  if(ulog) r = qgamma(y*M_LN10, a, /*scale=*/ 1.0, lower, ulog);
  else r = qgamma(y, a, /*scale=*/ 1.0, lower, ulog);
  /*MYprintf(MYstdout, "Rgamma_inv: a=%g, y=%g, lower=%d, ulog=%d, r=%g\n",
    a, y, lower, ulog, r); */
  assert(!isnan(r));
  return(r);
}


/*
 * Igamma_inv:
 *
 * incomplete gamma inverse function from UCS
 */

double Igamma_inv(const double a, const double y, const int lower,
                  const int ulog)
{
  double r;
  if(ulog) r = Rgamma_inv(a, y - Cgamma(a, ulog), lower, ulog);
  else r = Rgamma_inv(a, y / Cgamma(a, ulog), lower, ulog);
  assert(!isnan(r));
  /* MYprintf(MYstdout, "Rgamma_inv: a=%g, y=%g, lower=%d, ulog=%d, r=%g\n",
     a, y, lower, ulog, r); */
  return(r);
}
