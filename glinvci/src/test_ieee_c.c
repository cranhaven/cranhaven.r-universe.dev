#include<Rinternals.h>
#include<Rmath.h>

SEXP glinvtestfloatIEEE02 (SEXP Rx) {
	SEXP Rout;
	double *x, *out;
	x = REAL(Rx);
	Rout = PROTECT(allocVector(REALSXP, 1));
	out  = REAL(Rout);
	out[0] = exp(x[0]);
	UNPROTECT(1);
	return Rout;
}
void glinvtestfloatIEEE03 (double *x, double *out) {
	out[0] = exp(x[0]);
}
