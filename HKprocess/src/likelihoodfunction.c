/*******************************************************************************
likelihoodfunction

Takes arguments r, x, and EPS:
    r: The autocorrelation vector of size nn x 1 for a normal stationary
    process.
    x: A column vector of size nnx x 1.
    EPSL: Used to define precision in control statements.

Changes vector y:
    y[0]: Equal to g1(H),(eq 9,[1]).
    y[1]: Determinant of the Toeplitz autocorrelation matrix nn x nn produced by
    r.
    y[2]: Equal to mu,(eq 8, [1]).
    y[3]: Equal to sigma, (eq 8, [1]).

Possible values of fault and corresponding fault conditions are:
    0 The program is normally performed
    1 Error ("Singular Matrix")
    2 Error ("Input r[0] is not equal to 1.")
    3 Error ("The length(r) is not equal to the length(x))"

Uses the lev.c and levDet.c functions

References
[1] Tyralis H, Koutsoyiannis D (2011) Simultaneous estimation of the parameters
    of the Hurst-Kolmogorov stochastic process. Stochastic Environmental
    Research & Risk Assessment 25(1):21-33. doi:10.1007/s00477-010-0408-x
*******************************************************************************/

#include "trenchR.h"

void likelihoodfunction(double *r,int *nn,
	 double *x,int *nnx,double *EPSL,double *y,int *fault)
{
	VECTOR y1, y2, e1, e2, e3, e4;
	double EPS, s1, s2, s3;
	int n = *nn, nx = *nnx, i, j, k, _fault1;

if (n != nx)
	{
		for (i = 0; i < 4; i++)
			y[i] = 0.0;
		fault[0] = 3;//The length(r) is not equal to the length(x)
		return;
	}

	EPS = *EPSL;
	y1 = Vector(n);
	e1 = Vector(n-1);
	e3 = Vector(n);
	// e3: vector of size n x 1 with all elements equal to 1.
    for (j = 0; j < n; j++)
			e3[j] = 1.0;
    _fault1 = lev(r,n,e3,y1,e1,EPS);// y1: solution of the system R * y1 = e3

if (_fault1 != 0)
	{
		for (i = 0; i < 2; i++)
			y[i] = 0.0;
		fault[0] = _fault1;
		free_vector(y1);
		free_vector(e1);
		free_vector(e3);
		return;
	}

else
    {
    fault[0] = 0;// The program is normally performed
    y2 = Vector(n);
    e2 = Vector(n-1);
    e4 = Vector(n);
    s1 = sum(n,y1);// s1 equals to t(e3) * inv(R) * e3.
    s2 = dot(n,x,y1);// s2 equals to t(x) * inv(R) * e3
    y[2] = s2/s1;
    // e4 equals to x - (s2/s1)*e3
    for (k = 0; k < n; k++)
			e4[k] = x[k]-y[2] * e3[k];
    lev(r,n,e4,y2,e2,EPS);
    // y2: solution of the system R * y2 = x -(s2/s1)*e3
    y[1] = levDet(n-1,e1);// Computation of the logarithm of the determinant.
    s3 = dot(n,e4,y2);
    y[3] = sqrt(s3/n);
	y[0] = -0.5 * y[1]- 0.5 * n *log(s3);
	free_vector(y1);
	free_vector(y2);
	free_vector(e1);
	free_vector(e2);
    free_vector(e3);
    free_vector(e4);
    }
}
