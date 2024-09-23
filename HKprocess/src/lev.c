/*******************************************************************************
lev

Levinson algorithm (Algorithm 4.7.2 [1]).

Takes arguments r,x, and EPS:
    r: The autocorrelation vector of size n x 1 for a normal stochastic process
    x: A column vector of size n x 1.
    EPS: Used to define precision in control statements.

Changes vectors y and e:
    y: The solution of the linear system R y = x of size n x 1.
    e: Vector of residuals of size (n-1) x 1.
    R: The autocorrelation matrix formed from r.

Program outputs:
Possible values of fault and corresponding fault conditions are:
    0 The program is normally performed
    1 Error ("Singular Matrix")
    2 Error ("Input r[0] is not equal to 1.")

Used in the likelihood.c, logpHx.c, ltza.c, ltzb.c, ltzc.c and ltzd.c functions
Ideas for creating this function came from C functions of the of the R package
ltsa 1.4.4 [2]


References
[1] Golub GH, Van Loan CF (1996) Matrix Computations, third edition.
    John Hopkins University Press, Baltimore
[2] McLeod AI, Yu H, Krougly ZL (2007) Algorithms for linear time series
    analysis: With R package. Journal of Statistical Software 23(5):1-26
*******************************************************************************/

#include "trenchR.h"

int lev (double *r,int n,double *x,double *y,double *e,double EPS)
{
    VECTOR v, l, b, c;
    int n1;
    int i, j, k, m;
    n1 = n-1;

	if (fabs(r[0] - 1.0) > EPS) // error("r[0] is not equal to 1");
		return 2;
	e[0] = 1.0 - r[1] * r[1];
	if (e[0] < EPS) // nrerror("Singular Matrix-1");
        return 1;
	v = Vector(n1);
	l = Vector(n1);
	b = Vector(n);
	c = Vector(n1);
	v[0] = - r[1];
	l[0] = x[1] - r[1] * x[0];
	b[0] = - r[1];
	b[1] = 1.0;
	y[0] = (x[0] - r[1] * x[1]) / e[0];
	y[1] = l[0] / e[0];

	for (i = 1; i < n1; i++)
	{
	    v[i] = - dot(i + 1,r + 1,b) / e[i-1];
	    e[i] = e[i-1] * (1 - v[i] * v[i]);
	    l[i] = x[i+1] - flipupdot(i + 1,r + 1,y);
	    for (k = 0; k < i + 1; k++)
	    {
	    c[k] = b[i-k];
	    }
	    b[i + 1] = b[i];
	    for (j = i; j > 0; j--)
	    {
	        b[j] = b[j-1] + v[i] *c[j];
        }
	    b[0] = v[i] * c[0];
	    y[i+1] = (l[i] / e[i]) * b[i + 1];
	    for (m = i; m > -1; m--)
	    {
	        y[m] = y[m] + (l[i] / e[i]) * b[m];
        }
	}

	free_vector(v);
	free_vector(l);
	free_vector(b);
	free_vector(c);
	return 0;
}
