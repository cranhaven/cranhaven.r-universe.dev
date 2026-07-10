#ifndef MYMATH_H
#define MYMATH_H

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

double sign(double x);

double max(double x,double y);

double max_abs_vec(double * x, int n);

double max_vec(double * x, int n);

void max_selc(double *x, double vmax, double *x_s, int n, int *n_s, double z);

double min(double x,double y);

// x-y;
void vec_dif(double * x, double * y, int * xa_idx, int n);

double l1norm(double * x, int n);

// ||x||_1
double l1norm_ac(double * x, int * xa_idx, int n);

// ||x||_2^2;
double l2norm(double * x, int * xa_idx, int n);

// ||x-y||_2^2;
double dif_vec_l2norm(double * x, double * y, int n);

// ||x-y||_2^2;
double dif_vec_l2norm_as(double * x, double * y, int * xa_idx, int n);

// ||y-Ax||_2^2;
double dif_l2norm(double *r, double *y, double *A, double *x, int n, int m, int size_a, int * idx_a);

// ||y-Ax||_2^2;
double dif_l2norm_as(double *r, double *y, double *A, double *x, int *xa_idx, int n, int m);

// <x,y-z>;
double inner_prod2(double * x, double * y, double * z, int n);

// <x,y-z>;
double inner_prod2_as(double * x, double * y, double * z, int * xz_idx, int n);

void euc_proj(double * v, double z, int n);

double fun1(double lambda, double * v, double z, int n);

double mod_bisec(double * v, double z, int n);

void fabs_vc(double *v_in, double *v_out, int n);

void max_fabs_vc(double *v_in, double *v_out, double *vmax, int *n1, int n, double z);

void sort_up_bubble(double *v, int n);

void get_residual(double *r, double *y, double *A, double *x, int *xa_idx, int *nn, int *mm);

void get_dual(double *u, double *r, double *mmu, int *nn);

void get_dual1(double *u, double *r, double *mmu, int *nn);

void get_grad(double *g, double *A, double *u, int *dd, int *nn);

void get_base(double *base, double *u, double *r, double *mmu, int *nn);

void lineaization(double *XX, double *XY, double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, double T, double threshold, int size_a, int intercept, int dim);

void lineaization_lasso(double *XX, double *XY, double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, int *idx_i, int *size_a, int *idx_a1, int *idx_i1, int *size_a1, double T, double threshold, int intercept, int dim);

void lineaization_lasso_dantzig(double *XX, double *XY, double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, int *idx_scr, int *size_a, int *idx_a1, int *idx_i1, int *size_a1, double T, double threshold, int intercept, int dim);

void dantzig_ladm_scr(double *Y0, double *X0, double *Y, double *X, double *XX, int *idx_scr, int num_scr, int dim, double *alp, double *beta, double * mu, double *T, double rho, int *ite, double lambda, int max_ite, double prec, int intercept, int flag, int nlamb);

void clime_ladm_scr(double *Y0, double *X0, double *Y, double *X, double *XX, int *idx_scr, int num_scr, int dim, double *alp, double *beta, double * mu, double *T, double rho, int *ite, double lambda, int max_ite, double prec, int flag, int nlamb);

void lineaization_clime(double *beta0, double *beta1, double *beta_tild, double *g, int *idx_a, int *size_a, int *idx_a1, int *idx_i1, int *size_a1, double T, double threshold, int dim);


void dantzig_ladm_scr(double *Y0, double *X0, double *Y, double *X, double *XX, int *idx_scr, int num_scr, int dim, double *alp, double *beta, double * mu, double *T, double rho, int *ite, double lambda, int max_ite, double prec, int intercept, int flag, int nlamb);

void sqrt_ladm_scr(double *Y0, double *X0, double *X, double *XX0, double *XX, int *idx_scr, int num_scr, int ndata, int dim, double *alp, double *beta, double * mu, double *T, double rho, int *ite, double lambda, int max_ite, double prec, int intercept, int flag, int nlamb, double nrholamb);

//extern clock_t start, stop;
//extern clock_t time0;
//extern clock_t time1;
//extern clock_t time2;
//extern clock_t time3;

#endif
