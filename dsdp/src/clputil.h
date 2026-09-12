#ifndef _CLP_UTIL_
#define _CLP_UTIL_

#include "clp.h"

double momentExpDist(const int n, const double lambda0);
double momentGaussDist(const CLP_INT n, const double mu, const double sig);
void compute_GaussDistMomentMatrix(const CLP_INT d, const double mu, 
    const double sig, double *M1);
void compute_ExpDistMomentMatrix(const CLP_INT d, const double lmd,
                                 double *M1, double *M2);
CLP_INT compute_GaussDistDataMatrix(const CLP_INT d, const  CLP_INT nlen,
    const double *xin, double *arrayX);
CLP_INT compute_ExpDistDataMatrix(const CLP_INT d, const CLP_INT nlen,
    const double *xin, double *arrayX1, double *arrayX2);
void compute_coeff1(const CLP_INT d, const double *M1, double *av);
void compute_coeff2(const CLP_INT d, const double *M1, const double *M2,
    double *av);
void eval_poly(const CLP_INT d, const CLP_INT nlen,const double *coeffv, 
    const double *xv, double *yv);
void polyaxb(const CLP_INT n, const double *av, const double c,
    const double alpha, const double beta, double *a1v);
double aic_GaussDist(const CLP_INT d, const CLP_INT n, const double *y,
    const double *c, const double *x, const double mu, const double sig);
double aic_ExpDist(const CLP_INT d, const CLP_INT n, const double *y,
    const double *c, const double *x, const double lmd);
double histmean(const CLP_INT n, const double *data, const double *freq);
double histstd(const CLP_INT n, const double mean, const double *data,
    const double *freq);
double ggamma(const double alpha, const double lmd, const double p,
    const double t);
double momentGGammaDist(const CLP_INT n, const double alpha, const double lmd,
    const double p);
void compute_GGammaDistMomentMatrix(const CLP_INT d, const double alpha,
    const double lmd, const double p, double *M1, double *M2);
CLP_INT cdf_polygauss(const CLP_INT d, const CLP_INT nlen, const double *coeffv,
    const double mu, const double sig, const double *xv, double *yv);
CLP_INT cdf_polyggamma(const CLP_INT d, const CLP_INT nlen, const double *coeffv,
    const double alpha, const double lmd, const double p,
    const double *xv, double *yv);

double igamma(const double a, const double x);
double icgamma(const double a, const double x);
double cdfmomentgauss(const CLP_INT k, const double A, const double mu,
    const double sig);
double cdfmomentggamma(const CLP_INT k, const double A, const double alpha,
    const double lmd, const double p);
#endif
