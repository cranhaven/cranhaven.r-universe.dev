/*******************************************************************************
ltzd

Takes arguments r,x, and EPSL:
    r: The autocorrelation vector of size nn x 1 for a normal stochastic process.
    x: A column vector of size nnx x 1.
    EPSL: Used to define precision in control statements.

Changes vector y:
    y[0]: Equals to t(x) * inv(R) * x.
    t(.): Denotes the transpose of a matrix.
    inv(.): Denotes the inverse of a matrix.
    R: The autocorrelation matrix formed from r.

Possible values of fault and corresponding fault conditions are:
    0 The program is normally performed
    1 Error ("Singular Matrix")
    2 Error ("Input r[0] is not equal to 1.")
    3 Error ("The length(r) is not equal to the length(x))"

Uses the lev.c function
*******************************************************************************/

#include "trenchR.h"

void ltzd(double *r,int *nn,
	 double *x,int *nnx,double *EPSL,double *y,int *fault)
{
	VECTOR y1, e1 ;
	double EPS;
	int n = *nn, nx = *nnx, i, _fault1;
if (n != nx)
	{
		for (i = 0; i < 2; i++)
			y[i] = 0.0;
		fault[0] = 3;//The length(r) is not equal to the length(x)
		return;
	}
	EPS = *EPSL;
	y1 = Vector(n);
	e1 = Vector(n-1);
	_fault1 = lev(r,n,x,y1,e1,EPS);// y1 solution of the system R * y1 = x
if (_fault1 != 0)
	{
		for (i = 0; i < 2; i++)
			y[i] = 0.0;
		fault[0] = _fault1;
		free_vector(y1);
		free_vector(e1);
		return;
	}
else
    fault[0] = 0;// The program is normally performed
	y[0] = dot(n,x,y1);// s3 equals to t(x) * inv(R) * x.
	free_vector(y1);
	free_vector(e1);
}
