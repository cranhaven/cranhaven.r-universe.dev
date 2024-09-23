/*******************************************************************************

This file is a modified version of the file trenchR.h of the R package
ltsa 1.4.4 [1]

References
[1] McLeod AI, Yu H, Krougly ZL (2007) Algorithms for linear time series
    analysis: With R package. Journal of Statistical Software 23(5):1-26
*******************************************************************************/

#ifndef _TRENCH_H_
#define _TRENCH_H_

#include "nrutil.h"
#include <string.h>

#include <R.h>

int lev (double *r,int n,double *x,double *y,double *e,double EPS);
double levDet(int n,double *e);
double dot(int n,double* u,double* v);
double flipupdot(int n,double* u,double* v);
int sign(double u);
double sum(int n,double* u);
int sumint(int n,int* u);

#endif /* _TRENCH_H_ */
