#ifndef _CLPMISC_H_
#define _CLPMISC_H_
#include "clp.h"

CLP_INT compute_qr(const CLP_INT m, const CLP_INT n, double *A, double *tau);
CLP_INT compute_Qx(const char trans, const CLP_INT m, const CLP_INT n, 
    const double *A, const double *tau, double *x);
void solve_Rx(const char trans, const CLP_INT m, const CLP_INT n,
    const double *R, double *x);
CLP_INT solve_normalEquation(const CLP_INT n, const CLP_INT m, const double *At,
    const double *tau, const double *RinvtRp, const double *Rd, 
    const double *Rc, double *x, double *s, double *y);
CLP_INT compute_minEig(const CLP_INT n, double *S, double *w, CLP_INT *m);
CLP_INT compute_svd(const CLP_INT n, double *A, double *s);
void copy_mat(const char mode, const CLP_INT m, const CLP_INT n, 
    const double *x, const CLP_INT ldx, double *z, const CLP_INT ldz);
void zerofill_vec(const CLP_INT n, double *x);
void zerofill_mat(const char mode, const CLP_INT m, const CLP_INT n,
    double *x, const CLP_INT ldx);
void mul_diagMat(const char side, const CLP_INT m, const CLP_INT n, double *x,
    const CLP_INT ldx, const double *d);
CLP_INT compute_chol(const CLP_INT n, double *x, const CLP_INT ldx);
void transpose(const CLP_INT m, const CLP_INT n, const double *x, double *z);
CLP_INT compute_scalingOpNTSDP(const CLP_INT n, const CLP_INT ldx, const double *x,
    const double *s,
    double *d, double *dinv, double *d05inv, double *g, double *ginv);
void scalebackPrimalSDP(const CLP_INT n, const double *xs, const double *g,
 double *z, double *x);
 void scaleDualSDP(const CLP_INT n, const double *s, const double *g,  double *z,
    double *ss);
void scalebackDualSDP(const CLP_INT n, const double *ss, const double *ginv, 
    double *z, double *s);
void compute_quadcorSDP(const CLP_INT n, const double *dx,
    const double *ds, const double *dinv, double *TZ1, double *TZ2, double *qc);
CLP_INT det(const CLP_INT n, double *x, double *val);
CLP_INT detS(const CLP_INT n, const double *x, double *val);
void printvec(const CLP_INT n, const double *z);
void printmat(const CLP_INT m, const CLP_INT n, const double *Z);
#endif
