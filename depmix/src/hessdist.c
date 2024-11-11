
/***********************************
*                                  *
*   Hessian of distributions       *
*                                  *
*   Author: Ingmar Visser          *
*                                  *
*   Date: september 22, 2004       *
*                                  *
***********************************/

#include "hessdist.h"

/* 
 *  the second to last integer argument determines for which parameter the
 *  derivative is returned, ie 1 for mu and 2 for sigma, if logsc=1 the log 
 *  will be returned (logsc not implemented by the way)
 */

double squared(double x) {
	return(x*x);
}

double hessnorm(double x, double mu, double sig, int ms1, int ms2, int logsc) {
	double hessian=0.0;
/* 	 hess of mu and mu */
	if(ms1==1 && ms2==1) hessian=dnorm(x,mu,sig,0)*(squared((x-mu)/(squared(sig))) - 1/squared(sig));
/* 	 hess of mu and sig */
	if((ms1==1 && ms2==2)||(ms1==2 && ms2==1)) hessian=dnorm(x,mu,sig,0)*
													  ((((squared(x-mu)/(squared(sig)*sig))-(1/sig)))
													   *((x-mu)/(squared(sig)))
													   +((2*(mu-x))/(squared(sig)*sig)) );
/* 	hess of sig and sig */
	if(ms1==2 && ms2==2) hessian=dnorm(x,mu,sig,0)*((squared(((squared(x-mu))/(squared(sig)*sig))-(1/sig)))
												   +((1/squared(sig))-( (3*squared(x-mu))/(squared(squared(sig))) )) );
	return(hessian);
}

void Rhessnorm(double *x, double *mu, double *sig, int *ms1, int *ms2, int *logsc, double *value) {
	value[0]=hessnorm(x[0],mu[0],sig[0],ms1[0],ms2[0],logsc[0]);
}

