
#ifndef _SUMM_STATS_H
#define _SUMM_STATS_H

// USE_FC_LEN_T and FCONE needed when calling Fortran from C, see comment on top of mgcv.h in mgcv package
#ifndef  USE_FC_LEN_T
# define USE_FC_LEN_T
#endif
#include <R_ext/Linpack.h>
#include <R_ext/Lapack.h>
#ifndef FCONE
# define FCONE
#endif

#include <R.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* The data -> statistics routines */
void slacf(double *acf,double *x,int *n,int *n_reps,int *max_lag,double *NAcode,int *correlation);
void slnlar(double *beta, double *x,int *n,int *n_reps,int *n_terms,
            int *lag,int *power,double *NAcode);
void order_reg(double *beta, double *x,double *z,int *n,int *n_reps,int *np,int *diff);
void blowC(double *n,double *theta,double *e,double *e1,int *burn_in,int *n_t, int *n_reps);

/* LAPACK based matrix routines, from mgcv's mat.c */

void mgcv_qr(double *x, int *r, int *c,int *pivot,double *tau);
void mgcv_qrqy(double *b,double *a,double *tau,int *r,int *c,int *k,int *left,int *tp);
void mgcv_backsolve(double *R,int *r,int *c,double *B,double *C, int *bc);

#endif
