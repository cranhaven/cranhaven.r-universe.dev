#ifndef MY_MATH_H
#define MY_MATH_H
#define _USE_MATH_DEFINES
#include <math.h>

#define HALF_LOG_2PI 0.5 * (log(2.0 * M_PI))
double logit_inv(double x);
double safe_log1pexp(double x);
double ord_qk(int k, double eta, double* c, int* Kord);

#endif
