#ifndef NRUTIL_H
#define NRUTIL_H

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#define NR_END 1
#define FREE_ARG char*


int nrutil_nrerror(char error_text[]);
float *nrutil_vector(long nl, long nh);
int *nrutil_ivector(long nl, long nh);
unsigned char *nrutil_cvector(long nl, long nh);
unsigned long *nrutil_lvector(long nl, long nh);
double *nrutil_dvector(long nl, long nh);
float **nrutil_matrix(long nrl, long nrh, long ncl, long nch);
double **nrutil_dmatrix(long nrl, long nrh, long ncl, long nch);
int **nrutil_imatrix(long nrl, long nrh, long ncl, long nch);
float **nrutil_submatrix(float **a, long oldrl, long oldrh, long oldcl, long oldch,
	long newrl, long newcl);
float **convert_matrix(float *a, long nrl, long nrh, long ncl, long nch);
float ***f3tensor(long nrl, long nrh, long ncl, long nch, long ndl, long ndh);
double ***d3tensor(long nrl, long nrh, long ncl, long nch, long ndl, long ndh);
void free_vector(float *v, long nl, long nh);
void free_ivector(int *v, long nl, long nh);
void free_cvector(unsigned char *v, long nl, long nh);
void free_lvector(unsigned long *v, long nl, long nh);
void free_dvector(double *v, long nl, long nh);
void free_matrix(float **m, long nrl, long nrh, long ncl, long nch);
void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch);
void free_imatrix(int **m, long nrl, long nrh, long ncl, long nch);
void free_submatrix(float **b, long nrl, long nrh, long ncl, long nch);
void free_convert_matrix(float **b, long nrl, long nrh, long ncl, long nch);
void free_f3tensor(float ***t, long nrl, long nrh, long ncl, long nch,
	long ndl, long ndh);
void free_d3tensor(double ***t, long nrl, long nrh, long ncl, long nch,
	long ndl, long ndh);


#endif // NRUTIL_H
