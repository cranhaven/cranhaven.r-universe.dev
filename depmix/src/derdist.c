
/***********************************
*                                  *
*   Derivatives of distributions   *
*                                  *
*   Author: Ingmar Visser          *
*                                  *
*   Date: september 22, 2004       *
*                                  *
***********************************/

#include "derdist.h"

 /* 
  * the second to last integer argument determines for which parameter the
  * derivative is returned, ie 1 for mu and 2 for sigma, if logsc=1 the log 
  * will be returned (logsc not implemented by the way)
  */

double dernorm(double x, double mu, double sig, int ms, int logsc) {
	double deriv=0.0;
	if(ms==1) deriv=((x-mu)/(sig*sig))*dnorm(x,mu,sig,0); /* derivative of mu */
	if(ms==2) deriv=(((x-mu)*(x-mu))/((sig*sig*sig)) - 1.0/sig)*dnorm(x,mu,sig,0); /* derivative of sigma */
	return(deriv);
}

