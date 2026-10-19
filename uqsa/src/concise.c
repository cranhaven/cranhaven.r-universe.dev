#include <stdlib.h>
#include <stdio.h>
#include <R.h>
#include <Rdefines.h>
#include <math.h>
#include <string.h>
#define Rdata SEXP

int numeric(char c){
	if ('0' <= c && c <= '9') return 1;
	else if (c=='.' || c=='+' || c=='-') return 1;
	else return 0;
}

/* numbe a = v ± u */
struct num {
	union{ /* the estimate of something */
		double v;
		double value;
		double expectedValue;
		double mean;
	};
	union { /* the uncertainty of the estimate */
		double u;
		double error;
		double stdDev;
		double uncertainty;
	};
};

/* this will only read: 12.34(56)E-7, like this, with an E or e*/
struct num read_concise(const char *line){
	struct num a;
	char *ptr_u, *ptr_e;
	int vscale=0;
	double v=strtod(line,&ptr_u);
	int u=0;
	//int ulen=2;
	const char *decimal=strchr(line,'.');
	int digits=(decimal?(ptr_u-decimal-1):0);
	while (ptr_u && *ptr_u && !numeric(*ptr_u)) ptr_u++;
	if (numeric(*ptr_u)) {
		u=strtol(ptr_u,&ptr_e,10);
	} else {
		ptr_e=ptr_u;
	}
	while (ptr_e && *ptr_e && !numeric(*ptr_e)) ptr_e++;
	if (numeric(*ptr_e)) vscale=strtol(ptr_e,NULL,10);
	a.v=v*pow(10,vscale);
	a.u=u*pow(10,-digits+vscale);
	return a;
}

/* This function makes some assumptions about reported errors Some
 * numbers are exact (the error is 0). But, often reported numbers tat
 * stem from measurements are not. In some cases the number refers to
 * a small integer count of things: I have two children, exactly.
 * But, when you have a shipment of 478 cans of olive oil, it may
 * start to get imprecise, maybe it is 477 cans, that wouldn't be an
 * issue. To avoid costly calculations of magnitudes and having rulkes
 * of thumb about errors (like "half of a rulers smallest division"),
 * this function assumes that the numbers it receives are exact, or
 * have an uncertanty appended at the end, separated by some
 * character, like ';'.
 * Examples:
 *  read_number("2")     will be interpreted as   2 ± 0
 *  read_number("478;1") will be interpreted as 478 ± 1.
 */
struct num read_number(const char *line){
	struct num a;
	char *p;
	char *q;
	a.value = strtod(line,&p);
	while (p && *p && !numeric(*p)) p++;
	if (p && *p){
		a.error = strtod(p,&q);
		if (p==q) a.error = INFINITY;
	} else {
		a.error = 0.0;
	}
	return a;
}

Rdata concise(Rdata charVector){
	int j;
	struct num a;
	const char *line;
	Rdata out = PROTECT(allocMatrix(REALSXP,2,length(charVector)));
	double *d = REAL(out);
	for (j=0;j<length(charVector);j++){
		line = CHAR(STRING_ELT(charVector,j));
		if (strchr(line,'(')){
			a=read_concise(line);
		} else {
			a=read_number(line);
		}
		memcpy(d,&a,sizeof(a));
		d+=2;
	}
	UNPROTECT(1);
	return out;
}
