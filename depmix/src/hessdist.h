
/***********************************
*                                  *
*   Hessian of distributions       *
*                                  *
*   Author: Ingmar Visser          *
*                                  *
*   Date: september 22, 2004       *
*                                  *
***********************************/


#ifndef HESSDIST
#define HESSDIST 1
   
#include <stdio.h>
#include <stdlib.h>

#include <R.h>
#include <Rmath.h>

/* extern "C" { */

//constant M_PI=3.141592653589793238462643383280 from Rmath.h

double squared(double x);

// the second and third to last integer arguments determine for which 
// combination of parameters the hessian is returned, ie 1 for mu and 
//2 for sigma, if logsc=1 the log will be returned (not functional)

double hessnorm(double x, double mu, double sig, int ms1, int ms2, int logsc);

//this puts the return value in x (for use directly from R)
/* void Rhessnorm(double *x, double *mu, double *sig, int *ms1, int *ms2, int *logsc, double *value); */

// to be implemented:

// double hesslnorm(double x, double lmu, double lsig, int ms1, int ms2, int logsc);
// double hessunif(double x, double low, double up, int lu1, int lu2, int logsc);
// double hessweibull(double x, double sh, double sc, int shsc1, int shsc2, int logsc);
// double hessgamma(double x, double sh, double sc, int shsc1, int shsc2, int logsc);

/* } */


#endif
