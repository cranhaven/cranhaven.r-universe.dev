
/***********************************
*                                  *
*   Derivatives of distributions   *
*                                  *
*   Author: Ingmar Visser          *
*                                  *
*   Date: september 22, 2004       *
*                                  *
***********************************/

#ifndef DERDIST
#define DERDIST 1

#include <stdio.h>
#include <stdlib.h>

#include <R.h>
#include <Rmath.h>

/* constant M_PI=3.141592653589793238462643383280 from Rmath.h */

 /* 
  * the second to last integer argument determines for which parameter the
  * derivative is returned, ie 1 for mu and 2 for sigma, if logsc=1 the log 
  * will be returned
  */

/* extern "C" { */

double dernorm(double x, double mu, double sig, int ms, int logsc);

/* 
 *  to be implemented:
 * 
 *  double derlnorm(double x, double lmu, double lsig, int ms, int logsc);
 *  double derunif(double x, double low, double up, int lu, int logsc);
 *  double derweibull(double x, double sh, double sc, int shsc, int logsc);
 *  double dergamma(double x, double sh, double sc, int shsc, int logsc);
 * 
 *  and others .....
 */

/* double squared(double x); */

// the second and third to last integer arguments determine for which 
// combination of parameters the hessian is returned, ie 1 for mu and 
//2 for sigma, if logsc=1 the log will be returned (not functional)

double hessnorm(double x, double mu, double sig, int ms1, int ms2, int logsc);

/* } // end extern "C" */

#endif

