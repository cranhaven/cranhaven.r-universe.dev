/*******************************************************************************
levDet

Levinson algorithm (Algorithm 4.7.2 [1]).

Determinant computation of a Toeplitz autocorrelation matrix (n+1) x (n+1)
produced by a normal stochastic process.

Takes argument e:
    e: Vector of residuals of size n x 1.

Program output:
    logDet: the logarithm of determinant.

Used in the likelihood.c, logpHx.c, ltza.c and ltzc.c functions

References
[1] Golub GH, Van Loan CF (1996) Matrix Computations, third edition.
    John Hopkins University Press, Baltimore
*******************************************************************************/

#include "trenchR.h"

double levDet(int n,double *e)
{
	int i;
	double logDet;
	logDet = 0.0;
	for (i = 0; i < n; i++)
		logDet += log(e[i]);
	return logDet;
}
