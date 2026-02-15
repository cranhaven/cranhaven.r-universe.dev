#ifndef MAPFIT_GAUSS_INTE
#define MAPFIT_GAUSS_INTE

/**
 Description: Gauss quadrature for the following integral
 
 | b
 |  f(x) dx
 | a
 
 gauss_inte_w: make points and weights for n discrete points
 n (in): the number of points. This is the size of both x and w.
 x (out): x-axis points in the interval [-1, 1].
 w (out): weights for the points.
 eps (in): tolerance error.
 
 gauss_inte_fx: make x points for the interval [a,b]
 n (in): the number of points.
 x (in): the x points for interval [-1, 1].
 a, b (in): lower and upper points for the integral
 fx (out): x point for the interval [a, b]
 return value: (b-a)/2
 
 gauss_inte_fv: compute the integral
 n (in): the number of points.
 w (in): weights for the x points in [-1,1]
 c (in): (b-a)/2 ?
 fv (in): function values at x points derived by gauss_inte_fx
 return value: the interal value
 */

#include "traits.h"

namespace gauss_inte {

static double PI = 3.14159265358979324;

template <typename Tv>
void w(Tv& x0, Tv& w0, double eps) {
  const int n = stride_vector_traits<Tv>::size(x0);
  double* x = stride_vector_traits<Tv>::value(x0);
  double* w = stride_vector_traits<Tv>::value(w0);
    
  int i, l, m;
  double p0, p1, p2;
  double q0, q1, q2;
  double tmp, dt;
  
  switch(n) {
  case 1:
    x[0] = 0.0;
    w[0] = 2.0;
    return;
  case 2:
    x[0] = std::sqrt(1.0/3.0);
    w[0] = 1.0;
    x[1] = -x[0];
    w[1] = w[0];
    return;
  case 3:
    x[0] = std::sqrt(0.6);
    w[0] = 5.0/9.0;
    x[1] = 0.0;
    w[1] = 8.0/9.0;
    x[2] = -x[0];
    w[2] = w[0];
    return;
  }
  
  m = n/2;
  for (i=0; i<m; i++) {
    tmp = std::cos((i+1.0-1.0/4.0)/(n+1.0/2.0)*PI);
    do {
      p1 = tmp;
      p2 = (3.0*tmp*tmp-1.0)/2.0;
      q1 = 1.0;
      q2 = 3.0*tmp;
      for (l=3; l<=n; l++) {
        p0 = p1;
        p1 = p2;
        p2 = ((2.0*l-1)*tmp*p1-(l-1)*p0)/l;
        q0 = q1;
        q1 = q2;
        q2 = ((2.0*l-1)*(tmp*q1+p1)-(l-1)*q0)/l;
      }
      dt = p2/q2;
      tmp = tmp - dt;
    } while(std::abs(dt) > std::abs(tmp)*eps);
    x[i] = tmp;
    w[i] = 2.0/(n*p1*q2);
  }
  if (n % 2 != 0) {
    x[n/2] = 0.0;
    tmp = (double) n;
    for (i=1; i<=m; i++)
      tmp = tmp*(0.5 - i)/i;
    w[n/2] = 2.0/(tmp*tmp);
  }
  for (i=0; i<m; i++) {
    x[n-1-i] = -x[i];
    w[n-1-i] = w[i];
  }
  return;
}

template <typename Tv1, typename Tv2>
double fx(const Tv1& x0, double a, double b, Tv2& fx0) {
  const int n = stride_vector_traits<Tv1>::size(x0);
  const double* x = stride_vector_traits<Tv1>::value(x0);
  double* fx = stride_vector_traits<Tv2>::value(fx0);

  double t1 = (b - a)/2.0;
  double t2 = (b + a)/2.0;
  for (int i=0; i<n; i++) {
    fx[i] = t1 * x[i] + t2;
  }
  return t1;
}

template <typename Tv1, typename Tv2>
double fv(const Tv1& w0, double c, Tv2& fv0) {
  const int n = stride_vector_traits<Tv1>::size(w0);
  const double* w = stride_vector_traits<Tv1>::value(w0);
  double* fv = stride_vector_traits<Tv2>::value(fv0);
  
  double sum = 0.0;
  for (int i=0; i<n; i++) {
    sum += w[i]* fv[i];
  }
  sum *= c;
  return sum;
}

}

#endif

