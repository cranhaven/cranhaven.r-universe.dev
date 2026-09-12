#ifndef _CLPSOL_H_
#define _CLPSOL_H_
#include "clp.h"


double iprod(const vecCLP *x, const vecCLP *s);
double extendedNormalizedDualityGap(const regionInfo *rinfo,
    const vecCLP *x, const vecCLP *s);
CLP_INT find_region(const regionInfo *rinfo, const double x);
CLP_INT compute_Aty(const ACLP *A, const double *y, vecCLP *z);
void compute_Rp(const double *b, const ACLP *A, const vecCLP *x, double *Rp);
void compute_Rd(const CLPinfo *clpinfo, const vecCLP *Asty, const vecCLP *c_s,
     const vecCLP *ss, vecCLP *Rd);
void compute_Rc(const CLPinfo *clpinfo, const coeffCLP* nu, const vecCLP *dinv,
    const vecCLP *d, const vecCLP* cor, vecCLP *z);
void compute_nucoeffCLP(const CLPinfo *clpinfo, const regionInfo *rinfo,
    const double nu, const double dg, coeffCLP *coeff);
CLP_INT compute_cor(const CLPinfo *infoclp, const vecCLP *dx, const vecCLP *ds,
    const vecCLP *dinv, vecCLP *cor);
CLP_INT scalingOpNTCLP(const vecCLP *x, const vecCLP *s, vecCLP *d, vecCLP *dinv,
    vecCLP *d05inv, vecCLP *g, vecCLP *ginv);
CLP_INT scaleACLP(const CLPinfo *infoclp, const ACLP *A, const vecCLP *ginv, ACLP* A1);
CLP_INT scaleDualvecCLP(const vecCLP *s, const vecCLP *g, vecCLP *ss);
CLP_INT scalebackDualvecCLP(const vecCLP *ss, const vecCLP *ginv, vecCLP *s);
CLP_INT scalebackPrimalvecCLP(const vecCLP *xs, const vecCLP *g, vecCLP *x);
void update_vecCLP(const double alpha, const vecCLP *dx, vecCLP *x);
void update_vecCLP2(const vecCLP *x, const double alpha, vecCLP *dx);
CLP_INT compute_stepsize(const regionInfo *rinfo, const CLP_INT rid, 
    const vecCLP *d, const vecCLP *d05inv, const vecCLP *dx_s, const vecCLP *ds_s,
    double *alpha, double *beta);
CLP_INT directinNTMHPC(const CLPinfo *clpinfo, const dataCLP *data, 
    const regionInfo *rinfo, const vecCLP *x, const vecCLP *s, const double *y,
    vecCLP *dx, vecCLP *ds, double *dy, double *alpha, double *beta);
double normCLP(const vecCLP *x);
void init_point(const dataCLP *data, const CLPinfo *clpinfo, 
    const regionInfo *rinfo, const double nu0, vecCLP *x, vecCLP *s, double *y);
CLP_INT feasibility(const dataCLP *data, const regionInfo *rinfo, 
    const vecCLP *x, const vecCLP *s, const double *y,
    double *gap, double *relgap, double *pobj, double *dobj,
    double *pinfeas, double *dinfeas);
CLP_INT solver(const dataCLP *data, const OPTIONS *options, RESULTS *results);
#endif
