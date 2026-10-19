#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

typedef SEXP Rdata;

/* input is assumed to be a character vector */
/* missing numbers default to 1.0 */
Rdata lstrtod(Rdata v){
	int i;
	size_t n = length(v);
	Rdata X = PROTECT(NEW_NUMERIC(n));
	const char *str;
	char *endptr;
	double *x=REAL(X);
	double d;
	for (i=0;i<n;i++){
		str = CHAR(STRING_ELT(v,i));
		d=strtod(str,&endptr);
		x[i] = (str==endptr)?1.0:d;
	}
	UNPROTECT(1);
	return X;
}
