#ifndef __GPSEP_LM_H__
#define __GPSEP_LM_H__
#include "gp_sep.hpp"

struct GPsepLm
{
  GPsepLm();
  GPsep* gpsep;
  unsigned int p;		/* number of regression covariates */
  double **H;			/* n \times p regression matrix */
  double *regcoef;		/* p-vector regression coefficient */
  double *Kires;		/* n-vector K^{-1}(Z-H\regcoef) */
  double **KiH;			/* n \times p matrix K^{-1}H */
  double **HtKiH;		/* p \times p matrix H^TK^{-1}H */
  double **Kernel;		/* n \times n matrix K^{-1}H(H^TK^{-1}H)^{-1}H^TK^{-1} */
  double psi, ldetHtKiH;
};

GPsepLm* newGPsepLm(const unsigned int, const unsigned int, double**,
		    double*, double*, const double, const int,
		    const unsigned int, double**);

void deleteGPsepLm(GPsepLm*);

void calc_HtKiH_sepLm(GPsepLm*);

GPsepLm* buildGPsepLm(GPsepLm*);

double llikGPsepLm(GPsepLm*, double*, double*);

void dllikGPsepLm(GPsepLm*, double*, double*);

void dllikGPsepLm_nug(GPsepLm*, double*, double*, double*);

void newparamsGPsepLm(GPsepLm*, double*, const double);

void mleGPsepLm(GPsepLm*, double *, double *, double *,
		const unsigned int, int, double*, int *,
		char*, unsigned int, int *);

double mleGPsepLm_nug(GPsepLm*, double, double, double*,
		       int, int*);

void jmleGPsepLm(GPsepLm*, int, double*, double*,
		  double*, double*, double*, int,
		  int*, int*, int*);

void predGPsepLm_lite(GPsepLm*, unsigned int, double**, double**,
		      double*, double*, double*, double*);
#endif
