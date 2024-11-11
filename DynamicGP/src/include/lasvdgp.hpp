#ifndef __LASVDGP_H__
#define __LASVDGP_H__
#include "gp_sep.hpp"

enum ERROR_TYPE{Chol = 1, SVD = 2, Opt= 3};

struct lasvdGP
{

  unsigned int nbas;		/* number of svd basis determined by frac*/
  unsigned int N;		/* const number of total design points  */
  unsigned int m;		/* const dimension of inputs */
  unsigned int tlen;		/* const: length of time series */
  unsigned int n0;		/* const: starting number of neighborhood points */
  unsigned int nn;		/* const: total number of neighborhood points */
  unsigned int nfea;		/* number of feasible points  */
  unsigned int nsvd;		/* number of points to perform svd */
  unsigned int nadd; 		/* number of added points in each iteration */
  unsigned int nappsvd;		/* number of points in svd sets with approximated coeff */
  unsigned int hasfitted; 	/* flag if mle has been performed 1, else 0 */
  double frac;			/* const: fraction for cumulative rule */
  double gstart; 		/* starting value for nugget */

  GPsep **gpseps;
  int *feaidx;			/* index of neighbor and feasible  points in design */
  int *svdidx;		        /* index of the set to perform svd in total design set*/
  int *neigsvdidx;		/* index of the neighborhood set in svd set */
  double *xpred;		/* the prediction point */
  double *basis;		/* tlen \times nbas matrix of basis cmajor */
  double *reds;			/* nbas vector of singular vector */
  double **design;		/* total design matrix, no memory copy */
  double **resp; 		/* total response matrix, no memory copy  */
  double **coeff;		/* nsv \times nbas matrix of right singular vectors  */
};
void getDs(double **X, unsigned int n, unsigned int m,
	   double *dstart, double *dmin, double *dmax, double *dab2);
lasvdGP* newlasvdGP(double* xpred, double **design, double **resp,
		    unsigned int N, unsigned int m, unsigned int tlen,
		    unsigned int nn, unsigned int n0, unsigned int nfea,
		    unsigned int nsvd, unsigned int nadd, double frac,
		    double gstart);
void deletelasvdGP(lasvdGP* lasvdgp);
void buildBasis(lasvdGP *lasvdgp);
void buildGPseps(lasvdGP *lasvdgp);
void jmlelasvdGP(lasvdGP *lasvdgp, unsigned int maxit, unsigned int verb);
void selectNewPoints(lasvdGP *lasvdgp);
void renewlasvdGP(lasvdGP* lasvdgp);
void predlasvdGP(lasvdGP* lasvdgp, double* pmean, double* ps2);
void iterlasvdGP(lasvdGP* lasvdgp, unsigned int resvdThres,
		 unsigned int every, unsigned int maxit, unsigned int verb);
#endif
