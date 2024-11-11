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


#ifndef __GP_SEP_H__
#define __GP_SEP_H__

struct GPsep{
  double **X;       /* design matrix */
  double **K;       /* covariance between design points */
  double **Ki;      /* inverse of K */
  double ***dK;     /* gradient of K */
  double ldetK;     /* log of the determinant of K */
  double *Z;        /* response vector */
  double *KiZ;      /* Ki %*% Z */
  unsigned int m;   /* number of cols in X */
  unsigned int n;   /* number of rows in X; length of Z */
  double *d;        /* separable lengthscale parameter to correlation */
  double g;         /* nugget parameter to correlation */
  double phi;       /* t(Z) %*% Ki %*% Z = t(Z) %*% KiZ, used for s2 */
};


GPsep* newGPsep(const unsigned int m, const unsigned int n, double **X,
    double *Z, double *d, const double g, const int dK);
GPsep* newGPsep_sub(const unsigned int m, const unsigned int n, int *p,
    double **X, double *Z, double *d, const double g, const int dK);
void updateGPsep(GPsep* gpsep, unsigned int nn, double **XX, double *ZZ,
    int verb);
unsigned int get_gpsep(void);
void deletedKGPsep(GPsep *gpsep);
void deleteGPsep(GPsep* gpsep);
void deleteGPsep_index(unsigned int i);
void deleteGPseps(void);
void calc_ZtKiZ_sep(GPsep *gpsep);
void newdKGPsep(GPsep *gpsep);
GPsep* buildGPsep(GPsep *gpsep, const int dK);
double llikGPsep(GPsep *gpsep, double *dab, double *gab);
void dllikGPsep(GPsep *gpsep, double *ab, double *dllik);
void dllikGPsep_nug(GPsep *gpsep, double *ab, double *dllik, double *d2llik);
void newparamsGPsep(GPsep* gpsep, double *d, const double g);
void jmleGPsep(GPsep *gpsep, int maxit, double *dmin, double *dmax,
      double *grange, double *dab, double *gab, int verb,
      int *dits, int *gits, int *dconv, int fromR);
void mleGPsep(GPsep* gpsep, double* dmin, double *dmax, double *ab,
      const unsigned int maxit, int verb, double *p, int *its,
      char *msg, int *conv, int fromR);
double mleGPsep_nug(GPsep* gpsep, double tmin, double tmax, double *ab,
      int verb, int *its);
void mleGPsep_nug_R(int *gpsepi_in, int *verb_in, double *tmin_in,
       double *tmax_in, double *ab_in, double *mle_out, int *its_out);
void mymleGPsep(GPsep* gpsep, double* dmin, double *dmax, double *ab,
		const unsigned int maxit, int verb, double *p, int *its,
		char *msg, unsigned int, int *conv);
void myjmleGPsep(GPsep *gpsep, int maxit, double *dmin, double *dmax,
		 double *grange, double *dab, double *gab, int verb,
		 int *dits, int *gits, int *dconv);
void predGPsep(GPsep* gpsep, unsigned int nn, double **XX, double *mean,
      double **Sigma, double *df, double *llik);
void new_predutilGPsep_lite(GPsep *gpsep, unsigned int nn, double **XX,
      double ***k, double ***ktKi, double **ktKik);
void predGPsep_lite(GPsep* gpsep, unsigned int nn, double **XX, double *mean,
     double *sigma2, double *df, double *llik);
void alcGPsep(GPsep *gpsep, unsigned int ncand, double **Xcand,
        unsigned int nref, double **Xref,  int verb, double *alc);
void pred_generic(const unsigned int n, const double phidf, double *Z,
		  double **Ki, const unsigned int nn, double **k, double *mean,
		  double **Sigma);
void new_predutil_generic_lite(const unsigned int n, double **Ki,
			       const unsigned int nn, double **k, double ***ktKi,
			       double **ktKik);
#endif
