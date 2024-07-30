#ifndef NEWTONRP
#define NEWTONRP

#include <limits>
/* #include <limits.h>	*/	
/* #include <float.h>   */
/* Alternative headers for old compilers not conforming yet the C++ standard  */

namespace newtonrp  {

const double EPSILON = std::numeric_limits<double>::epsilon();  /* Machine precision  */
/* const double EPSILON = DBL_EPSILON;  */	
/* Alternative code for old compilers not conforming yet the C++ standard   */

double lsrch(double x0,double (*f)(double),double (*f1)(double),double (*f2)(double),double lb,double ub,double precision=EPSILON);

/*
  Line search that finds and returns a zero, located between lb and ub,
  of the function pointed by f(), using the Newton-Raphson algorithm.

  The first and second derivatives of f, should be provided in the
  functions pointed respectively by f1() and f2(), and a starting point
  for the search, by the parameter x0.

  The parameter precision controls the magnitude of differences between f() 
  and zero considered to be non-negligable: when the absolute value of
  f() falls below precision, the search is terminated. By default this
  parameter is set to machine precision.
*/

}

#endif
