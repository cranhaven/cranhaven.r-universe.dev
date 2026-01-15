#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <iostream>
#include <cmath>     /* erf; pow; etc...*/ 
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

#ifdef _OPENMP
#include <omp.h>
#endif
  
// [[Rcpp::depends(RcppProgress)]]

constexpr double pi = 3.14159265358979323846;

//////// Gaussian kernel

NumericVector erf_cpp(NumericVector x){
  return(2. * pnorm(x * sqrt(2.)) - 1);
}

//' int_0_1( d_k(x,a)/dx * d_k(b,x) dx) (univariate)
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double w_ii_lebesgue(double a, double b, double t, int ct){
  if (ct == 1) {
    double a2 = a*a, b2 = b*b, t2 = t*t;
    return(1/(8*t2*t) * ((2 * (-2 + a + b) * exp((-a2 - b2 -2 + 2 * a + 2 * b)/(2*t2)) * t + exp(-(a-b)*(a-b)/(4 * t2)) *  sqrt(pi) * ((a-b)*(a-b) - 2 * t2) * erf((-2+a+b)/(2* t))) - (2 * (a+b)* t * exp(-((a2 + b2) / (2*t2))) + exp(-(a-b)*(a-b)/(4 * t2)) * sqrt(pi)* ((a-b)*(a-b) - 2 * t2)* erf((a+b)/(2* t)))));
  } else if (ct == 2) {
    if (b > a) {
      double temp = a;
      a = b;
      b = temp;
    }
    double a2 = a*a, b2 = b*b, t2 = t*t;
    return((-6*sqrt(3.)*a*b*t - 3*a*t2 - 3*b*t2 - sqrt(3.)*t2*t)/(4.*exp((sqrt(3.)*(a + b))/t)*t2*t2) + (exp((sqrt(3.)*(-2 + a + b))/t)*(-6*sqrt(3.)*t + 6*sqrt(3.)*a*t + 6*sqrt(3.)*b*t - 6*sqrt(3.)*a*b*t - 6*t2 + 3*a*t2 + 3*b*t2 - sqrt(3.)*t2*t))/(4.*t2*t2) + (exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(3*a*t2 - 3*b*t2 + sqrt(3.)*t2*t))/(4.*t2*t2) + (exp((sqrt(3.)*(-a + b))/t)*(-6*a2*a + 18*a2*b - 18*a*b2 + 6*b2*b + 3*a*t2 - 3*b*t2 + sqrt(3.)*t2*t))/(4.*t2*t2));
  } else if (ct == 3) {
    if (b > a) {
      double temp = a;
      a = b;
      b = temp;
    }
    double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
    return((-50*pow(a - b,3)*exp((sqrt(5.)*(-a + b))/t)*(a2 - 2*a*b + b2 + sqrt(5.)*a*t - sqrt(5.)*b*t + t2) + 
           (3*t*(exp(-(sqrt(5.)*(a + b))/t)*(-50*sqrt(5.)*a2*b2 - 100*a2*b*t - 100*a*b2*t - 10*sqrt(5.)*a2*t2 - 50*sqrt(5.)*a*b*t2 - 10*sqrt(5.)*b2*t2 - 35*a*t3 - 35*b*t3 - 7*sqrt(5.)*t2*t2) +
           exp(sqrt(5.)*(b - a)/t)*t2*(10*sqrt(5.)*a2 - 20*sqrt(5.)*a*b + 10*sqrt(5.)*b2 + 35*a*t - 35*b*t + 7*sqrt(5.)*t2))) - 
           3*t*(-(exp((sqrt(5.)*(-a + b))/t)*t2*(10*sqrt(5.)*a2 - 20*sqrt(5.)*a*b + 10*sqrt(5.)*b2 + 35*a*t - 35*b*t + 7*sqrt(5.)*t2)) + exp((sqrt(5.)*(-2 + a + b))/t)*(50*sqrt(5.) + 200*t + 70*sqrt(5.)*t2 + 70*t3 + 7*sqrt(5.)*t2*t2 + 10*b2*(5*sqrt(5.) + 10*t + sqrt(5.)*t2) - 5*b*(20*sqrt(5.) + 60*t + 14*sqrt(5.)*t2 + 7*t3) + 10*a2*(5*sqrt(5.) + 5*sqrt(5.)*b2 + 10*t + sqrt(5.)*t2 - 10*b*(sqrt(5.) + t)) - 5*a*(20*sqrt(5.) + 60*t + 14*sqrt(5.)*t2 + 7*t3 + 20*b2*(sqrt(5.) + t) - 10*b*(4*sqrt(5.) + 8*t + sqrt(5.)*t2)))))/(108.*t3*t3));
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' int_0_1( d_k(x,a)/dx * d_k(b,x) dnu(x)) (univariate), where nu is the TRUNCATED Gaussian measure with mean xm and variance xv.
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @param xm the mean of the gaussian measure
//' @param xv the variance of the gaussian measure
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double w_ii_tgauss(double a, double b, double t, int ct, double xm, double xv){
  double renorm = 1/(R::pnorm(1.0, xm, xv, 1, 0) - R::pnorm(0.0, xm, xv, 1, 0));
  if (ct == 1) {
    double a2 = a*a, b2 = b*b, t2 = t*t, xm2 = xm*xm, xv2 = xv*xv, srtv = sqrt(xv)*sqrt(t2 + 2*xv);
    return(renorm * (-1) * (((-2*t*srtv*(t2*(-1 + a + b - xm) + (-2 + a + b)*xv)) *
           exp((-1 + 2*xm)/t2 - pow(-1 + xm,2)/(2.*xv) - ((a + b - 2*xm)*(-1 + xm))/t2) + 
           exp((-1 + 2*xm)/t2 - pow(-1 + xm,2)/(2.*xv) + (pow(a + b - 2*xm,2)*xv2 + pow(-1 + xm,2)*pow(t2 + 2*xv,2))/(2.*t2*xv*(t2 + 2*xv)))*sqrt(2*pi) *
           (a*t2*t2*xm + b*t2*t2*xm - 2*t2*xv2 + a2*xv*(t2 + xv) + b2*xv*(t2 + xv) - t2*t2*(xm2 + xv) - a*b*(t2*t2 + 2*t2*xv + 2*xv2)) * 
           erf((-(t2*(-1 + xm)) - (-2 + a + b)*xv)/(sqrt(2.)*t*srtv))) + 
           ((2*t*srtv*(-(t2*xm) + a*(t2 + xv) + b*(t2 + xv))) *
           exp(-((a + b - 2*xm)*xm)/t2 -xm2/(2.*xv)) + 
           exp((pow(a + b - 2*xm,2)*xv2 + xm2*pow(t2 + 2*xv,2))/(2.*t2*xv*(t2 + 2*xv)) -xm2/(2.*xv))*sqrt(2*pi) *
           (a*t2*t2*xm + b*t2*t2*xm - 2*t2*xv2 + a2*xv*(t2 + xv) + b2*xv*(t2 + xv) - t2*t2*(xm2 + xv) - a*b*(t2*t2 + 2*t2*xv + 2*xv2)) *
           erf((t2*xm + (a + b)*xv)/(sqrt(2.)*t*srtv)))) /
             (2.*exp((a2 + b2 - 2*a*xm - 2*b*xm + 4*xm2)/(2.*t2))*sqrt(2*pi)*t2*t*pow(t2 + 2*xv,2.5)));
  }  else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}



//' int_0_1( d_k(x,a)/dx * d_k(b,x) dnu(x)) (univariate), where nu is the Gaussian measure with mean xm and variance xv.
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @param xm the mean of the gaussian measure
//' @param xv the variance of the gaussian measure
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double w_ii_gauss(double a, double b, double t, int ct, double xm, double xv){
  if (ct == 1) {
    double a2 = a*a, b2 = b*b, t2 = t*t, xm2 = xm*xm, xv2 = xv*xv;
    return(-((exp((-((a2 + b2)/t2) - xm2/xv + (t2*xm + (a + b)*xv)*(t2*xm + (a + b)*xv)/(t2*xv*(t2 + 2*xv)))/2.) * 
           (a*t2*t2*xm + b*t2*t2*xm - 2*t2*xv2 + a2*xv*(t2 + xv) + b2*xv*(t2 + xv) - t2*t2*(xm2 + xv) - a*b*(t2*t2 + 2*t2*xv + 2*xv2)))/
             (t2*t*(t2 + 2*xv)*(t2 + 2*xv)*sqrt(t2 + 2*xv))));
  }  else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

// //' int_0_1( d_k(x,a)/dx * d_k(b,x) dnu(x)) (univariate), where nu is the Gaussian measure with mean xm and variance xv.
// //' @param a,b design locations
// //' @param t lengthscale parameter
// //' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
// //' @param xm the mean of the gaussian measure
// //' @param xv the variance of the gaussian measure
// //' @return The scalar integrated derivative as a double.
// //' @noRd
// // [[Rcpp::export]]
// double kd_int_gauss(double a, double t, int ct, double xm, double xv) {
//   if (ct == 1) {
//     return((t*(a - xm))/(exp(pow(a - xm,2)/(2.*(t*t + xv)))*pow(t*t + xv,1.5)));
//   }  else {
//     throw std::invalid_argument("Covariance Type not Supported");
//   }
// }

// //' int_0_1( d_k(x,a)/dx * d_k(b,x) dnu(x)) (univariate), where nu is the Gaussian measure with mean xm and variance xv.
// //' @param a,b design locations
// //' @param t lengthscale parameter
// //' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
// //' @param xm the mean of the gaussian measure
// //' @param xv the variance of the gaussian measure
// //' @return The scalar integrated derivative as a double.
// //' @noRd
// // [[Rcpp::export]]
// double k_int_gauss(double a, double t, int ct, double xm, double xv) {
//   if (ct == 1) {
//     return(t/(exp(pow(a - xm,2)/(2.*(t*t + xv)))*sqrt(t*t + xv)));
//   }  else {
//     throw std::invalid_argument("Covariance Type not Supported");
//   }
// }


//' Gradient of int_0_1( d_k(x,a)/dx * d_k(b,x) dx) (univariate) with respect to a.
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double grad_w_ii_lebesguea(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
  if (ct == 1) {
    return((exp(-(a2 + b2)/(2.*t2)) * (a2*t + 4*a*b*t - b2*t) + 
           exp(-(2 - 2*a -2*b + a2 + b2)/(2.*t2))*(-a2*t - 4*a*b*t + b2*t -4*t + 6*a*t + 2*b*t) + 
           exp(-(a2 + b2 -2*a*b)/(4.*t2)) * (erf((-2 + a + b)/(2.*t)) - erf((a + b)/(2.*t)))*(-a2*a*sqrt(pi) + 3*a2*b*sqrt(pi) - 3*a*b2*sqrt(pi) + b2*b*sqrt(pi) +6*(a - b)*sqrt(pi)*t2)/2.)/(8.*t3*t2));
  } else if (ct == 2) {
    if (a > b) {
      return((3*exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(-6*t + 6*a*t + 6*b*t - 6*a*b*t + sqrt(3.)*a*t2 - sqrt(3.)*b*t2))/(4.*t3*t2) +
             (3*(6*a*b*t + sqrt(3.)*a*t2 - sqrt(3.)*b*t2))/(4.*exp((sqrt(3.)*(a + b))/t)*t3*t2) + (3*exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(2*sqrt(3.)*a2*a - 6*sqrt(3.)*a2*b + 6*sqrt(3.)*a*b2 - 2*sqrt(3.)*b2*b - 6*a2*t + 12*a*b*t - 6*b2*t - 2*sqrt(3.)*a*t2 + 2*sqrt(3.)*b*t2))/(4.*t3*t2));
    } else {
      return((3*exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(-6*t + 6*a*t + 6*b*t - 6*a*b*t + sqrt(3.)*a*t2 - sqrt(3.)*b*t2))/(4.*t3*t2) +
             (3*(6*a*b*t + sqrt(3.)*a*t2 - sqrt(3.)*b*t2))/(4.*exp((sqrt(3.)*(a + b))/t)*t3*t2) + (3*exp((2*sqrt(3.)*a)/t - (sqrt(3.)*(a + b))/t)*(2*sqrt(3.)*a2*a - 6*sqrt(3.)*a2*b + 6*sqrt(3.)*a*b2 - 2*sqrt(3.)*b2*b + 6*a2*t - 12*a*b*t + 6*b2*t - 2*sqrt(3.)*a*t2 + 2*sqrt(3.)*b*t2))/(4.*t3*t2));
    }
    
  } else if (ct == 3) {
    if (a > b) {
      return((5*(150*a2*b2*t + 60*sqrt(5.)*a2*b*t2 + 30*a2*t3 + 30*a*b*t3 - 30*b2*t3 + 9*sqrt(5.)*a*t2*t2 - 9*sqrt(5.)*b*t2*t2))/(108.*exp((sqrt(5.)*(a + b))/t)*t3*t3*t) + (5*exp((2*sqrt(5.)*(-1 + a + b))/t - (sqrt(5.)*(a + b))/t)*(-150*t + 300*a*t - 150*a2*t + 300*b*t - 600*a*b*t + 300*a2*b*t - 150*b2*t + 300*a*b2*t - 150*a2*b2*t - 60*sqrt(5.)*t2 + 120*sqrt(5.)*a*t2 - 60*sqrt(5.)*a2*t2 + 60*sqrt(5.)*b*t2 - 120*sqrt(5.)*a*b*t2 + 60*sqrt(5.)*a2*b*t2 - 30*t3 + 90*a*t3 - 30*a2*t3 - 30*b*t3 - 30*a*b*t3 + 30*b2*t3 + 9*sqrt(5.)*a*t2*t2 - 9*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t) + (5*exp((2*sqrt(5.)*b)/t - (sqrt(5.)*(a + b))/t)*(10*sqrt(5.)*a2*a2*a - 50*sqrt(5.)*a2*a2*b + 100*sqrt(5.)*a2*a*b2 - 100*sqrt(5.)*a2*b2*b + 50*sqrt(5.)*a*b2*b2 - 10*sqrt(5.)*b2*b2*b - 30*sqrt(5.)*a2*a*t2 + 90*sqrt(5.)*a2*b*t2 - 90*sqrt(5.)*a*b2*t2 + 30*sqrt(5.)*b2*b*t2 - 90*a2*t3 + 180*a*b*t3 - 90*b2*t3 - 18*sqrt(5.)*a*t2*t2 + 18*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t));
    } else {
      return((5*(150*a2*b2*t + 60*sqrt(5.)*a2*b*t2 + 30*a2*t3 + 30*a*b*t3 - 30*b2*t3 + 9*sqrt(5.)*a*t2*t2 - 9*sqrt(5.)*b*t2*t2))/(108.*exp((sqrt(5.)*(a + b))/t)*t3*t3*t) + (5*exp((2*sqrt(5.)*(-1 + a + b))/t - (sqrt(5.)*(a + b))/t)*(-150*t + 300*a*t - 150*a2*t + 300*b*t - 600*a*b*t + 300*a2*b*t - 150*b2*t + 300*a*b2*t - 150*a2*b2*t - 60*sqrt(5.)*t2 + 120*sqrt(5.)*a*t2 - 60*sqrt(5.)*a2*t2 + 60*sqrt(5.)*b*t2 - 120*sqrt(5.)*a*b*t2 + 60*sqrt(5.)*a2*b*t2 - 30*t3 + 90*a*t3 - 30*a2*t3 - 30*b*t3 - 30*a*b*t3 + 30*b2*t3 + 9*sqrt(5.)*a*t2*t2 - 9*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t) + (5*exp((2*sqrt(5.)*a)/t - (sqrt(5.)*(a + b))/t)*(10*sqrt(5.)*a2*a2*a - 50*sqrt(5.)*a2*a2*b + 100*sqrt(5.)*a2*a*b2 - 100*sqrt(5.)*a2*b2*b + 50*sqrt(5.)*a*b2*b2 - 10*sqrt(5.)*b2*b2*b - 30*sqrt(5.)*a2*a*t2 + 90*sqrt(5.)*a2*b*t2 - 90*sqrt(5.)*a*b2*t2 + 30*sqrt(5.)*b2*b*t2 + 90*a2*t3 - 180*a*b*t3 + 90*b2*t3 - 18*sqrt(5.)*a*t2*t2 + 18*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t));
    }
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' Gradient of int_0_1( d_k(x,a)/dx * d_k(b,x) dx) (univariate) with respect to b.
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The scalar integrated derivative as a double. 
//' @noRd
// [[Rcpp::export]]
double grad_w_ii_lebesgueb(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
  if (ct == 1) {
    return((exp(-pow(-2 + a + b,2)/(4.*t2) + (a + b)/t2 + (a2 +                                                                                                   \
           2*a*(-2 + b) + (-4 + b)*b)/(4.*t2) - (a2 +                                                                                                             \
           b2)/(2.*t2))*(a2*t - 2*a*b*t + b2*t -                                                                                                                  \
           2*t3))/(8.*t2*t2*t) + (2*a*b*t + 2*b2*t -                                                                                                              \
           2*t3)/(8.*exp((a2 + b2)/(2.*t2))*t2*t2*t) +                                                                                                            \
           (exp((-1 + a + b)/t2 - (a2 +                                                                                                                           \
           b2)/(2.*t2))*(-4*t + 2*a*t + 6*b*t - 2*a*b*t -                                                                                                         \
           2*b2*t + 2*t3))/(8.*t2*t2*t) + (exp(1/t2 - pow(-2 + a + b,2)/(4.*t2) + (a2 + 2*a*(-2 + b) + (-4 +                                                      \
           b)*b)/(4.*t2) - (a2 +                                                                                                                                  \
           b2)/(2.*t2))*(-(a2*t) + 2*a*b*t - b2*t +                                                                                                               \
           2*t3))/(8.*t2*t2*t) + (exp(-pow(-2 + a + b,2)/(4.*t2) -                                                                                                \
           (a2 + b2)/(2.*t2) + (2 + a2 + 2*a*(-1 + b) -                                                                                                           \
           2*b + b2)/(2.*t2))*(-2*a*sqrt(pi)*t2*erf((-2 + a +                                                                                                     \
           b)/(2.*t)) + 2*b*sqrt(pi)*t2*erf((-2 + a +                                                                                                             \
           b)/(2.*t))))/(8.*t2*t2*t) + (exp(-pow(-2 + a + b,2)/(4.*t2) +                                                                                          \
           (a2 + 2*a*(-2 + b) + (-4 + b)*b)/(4.*t2) - (a2 +                                                                                                       \
           b2)/(2.*t2) + (4 + a2 + 2*a*b +                                                                                                                        \
           b2)/(4.*t2))*(2*a*sqrt(pi)*t2*erf((a + b)/(2.*t)) -                                                                                                    \
           2*b*sqrt(pi)*t2*erf((a + b)/(2.*t))))/(8.*t2*t2*t) +                                                                                                   \
           (exp(pow(a + b,2)/(4.*t2) - (a2 +                                                                                                                      \
           b2)/(2.*t2))*((a2*a*sqrt(pi)*erf((-2 + a +                                                                                                             \
           b)/(2.*t)))/2. - (3*a2*b*sqrt(pi)*erf((-2 + a + b)/(2.*t)))/2.                                                                                         \
           + (3*a*b2*sqrt(pi)*erf((-2 + a + b)/(2.*t)))/2. -                                                                                                      \
           (b2*b*sqrt(pi)*erf((-2 + a + b)/(2.*t)))/2. -                                                                                                          \
           a*sqrt(pi)*t2*erf((-2 + a + b)/(2.*t)) +                                                                                                               \
           b*sqrt(pi)*t2*erf((-2 + a + b)/(2.*t)) -                                                                                                               \
           (a2*a*sqrt(pi)*erf((a + b)/(2.*t)))/2. +                                                                                                               \
           (3*a2*b*sqrt(pi)*erf((a + b)/(2.*t)))/2. -                                                                                                             \
           (3*a*b2*sqrt(pi)*erf((a + b)/(2.*t)))/2. +                                                                                                             \
           (b2*b*sqrt(pi)*erf((a + b)/(2.*t)))/2. +                                                                                                               \
           a*sqrt(pi)*t2*erf((a + b)/(2.*t)) - b*sqrt(pi)*t2*erf((a +                                                                                             \
           b)/(2.*t))))/(8.*t2*t2*t));
  } else if (ct == 2) {
    if (a > b) {
      return((3*exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(-2*sqrt(3.)*a2*a + \
             6*sqrt(3.)*a2*b - 6*sqrt(3.)*a*b2 + 2*sqrt(3.)*b2*b +               \
             6*a2*t - 12*a*b*t + 6*b2*t + 2*sqrt(3.)*a*t2 -                      \
             2*sqrt(3.)*b*t2))/(4.*t2*t2*t) + (3*exp((2*sqrt(3.)*(-1 + a +       \
             b))/t - (sqrt(3.)*(a + b))/t)*(-6*t + 6*a*t + 6*b*t - 6*a*b*t -     \
             sqrt(3.)*a*t2 + sqrt(3.)*b*t2))/(4.*t2*t2*t) + (3*(6*a*b*t          \
             - sqrt(3.)*a*t2 + sqrt(3.)*b*t2))/(4.*exp((sqrt(3.)*(a +            \
             b))/t)*t2*t2*t));
    } else {
      return((-3*(-6*a*b*t + sqrt(3.)*a*t2 -                                          \
             sqrt(3.)*b*t2))/(4.*exp((sqrt(3.)*(a + b))/t)*t2*t2*t) -                 \
             (3*exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(6*t - 6*a*t  \
             - 6*b*t + 6*a*b*t + sqrt(3.)*a*t2 -                                      \
             sqrt(3.)*b*t2))/(4.*t2*t2*t) - (3*exp((2*sqrt(3.)*a)/t -                 \
             (sqrt(3.)*(a + b))/t)*(2*sqrt(3.)*a2*a - 6*sqrt(3.)*a2*b +               \
             6*sqrt(3.)*a*b2 - 2*sqrt(3.)*b2*b + 6*a2*t - 12*a*b*t +                  \
             6*b2*t - 2*sqrt(3.)*a*t2 +                                               \
             2*sqrt(3.)*b*t2))/(4.*t2*t2*t));
    }
    
  } else if (ct == 3) {
    if (a > b) {
      return((5*exp((2*sqrt(5.)*b)/t - (sqrt(5.)*(a + b))/t)*(-10*sqrt(5.)*a2*a2*a + 50*sqrt(5.)*a2*a2*b - 100*sqrt(5.)*a2*a*b2 + 100*sqrt(5.)*a2*b2*b - 50*sqrt(5.)*a*b2*b2 + 10*sqrt(5.)*b2*b2*b + 30*sqrt(5.)*a2*a*t2 - 90*sqrt(5.)*a2*b*t2 + 90*sqrt(5.)*a*b2*t2 - 30*sqrt(5.)*b2*b*t2 + 90*a2*t3 - 180*a*b*t3 + 90*b2*t3 + 18*sqrt(5.)*a*t2*t2 - 18*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t) + (5*exp((2*sqrt(5.)*(-1 + a + b))/t - (sqrt(5.)*(a + b))/t)*(-150*t + 300*a*t - 150*a2*t + 300*b*t - 600*a*b*t + 300*a2*b*t - 150*b2*t + 300*a*b2*t - 150*a2*b2*t - 60*sqrt(5.)*t2 + 60*sqrt(5.)*a*t2 + 120*sqrt(5.)*b*t2 - 120*sqrt(5.)*a*b*t2 - 60*sqrt(5.)*b2*t2 + 60*sqrt(5.)*a*b2*t2 - 30*t3 - 30*a*t3 + 30*a2*t3 + 90*b*t3 - 30*a*b*t3 - 30*b2*t3 - 9*sqrt(5.)*a*t2*t2 + 9*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t) + (5*(150*a2*b2*t + 60*sqrt(5.)*a*b2*t2 - 30*a2*t3 + 30*a*b*t3 + 30*b2*t3 - 9*sqrt(5.)*a*t2*t2 + 9*sqrt(5.)*b*t2*t2))/(108.*exp((sqrt(5.)*(a + b))/t)*t3*t3*t));
    } else {
      return((-5*(-150*a2*b2*t - 60*sqrt(5.)*a*b2*t2 + 30*a2*t3 - 30*a*b*t3 - 30*b2*t3 + 9*sqrt(5.)*a*t2*t2 - 9*sqrt(5.)*b*t2*t2))/(108.*exp((sqrt(5.)*(a + b))/t)*t3*t3*t) - (5*exp((2*sqrt(5.)*(-1 + a + b))/t - (sqrt(5.)*(a + b))/t)*(150*t - 300*a*t + 150*a2*t - 300*b*t + 600*a*b*t - 300*a2*b*t + 150*b2*t - 300*a*b2*t + 150*a2*b2*t + 60*sqrt(5.)*t2 - 60*sqrt(5.)*a*t2 - 120*sqrt(5.)*b*t2 + 120*sqrt(5.)*a*b*t2 + 60*sqrt(5.)*b2*t2 - 60*sqrt(5.)*a*b2*t2 + 30*t3 + 30*a*t3 - 30*a2*t3 - 90*b*t3 + 30*a*b*t3 + 30*b2*t3 + 9*sqrt(5.)*a*t2*t2 - 9*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t) - (5*exp((2*sqrt(5.)*a)/t - (sqrt(5.)*(a + b))/t)*(10*sqrt(5.)*a2*a2*a - 50*sqrt(5.)*a2*a2*b + 100*sqrt(5.)*a2*a*b2 - 100*sqrt(5.)*a2*b2*b + 50*sqrt(5.)*a*b2*b2 - 10*sqrt(5.)*b2*b2*b - 30*sqrt(5.)*a2*a*t2 + 90*sqrt(5.)*a2*b*t2 - 90*sqrt(5.)*a*b2*t2 + 30*sqrt(5.)*b2*b*t2 + 90*a2*t3 - 180*a*b*t3 + 90*b2*t3 - 18*sqrt(5.)*a*t2*t2 + 18*sqrt(5.)*b*t2*t2))/(108.*t3*t3*t));
    }
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' gradient of w_ii with respect to t (univariate)
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The the derivative of the scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double grad_w_ii_dt_cpp(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t;
  if (ct == 1) {
    return(-(sqrt(pi)*(erf((b+a)/(2*t))-erf((b+a-2)/(2*t)))*(2*t2-2*b*t+2*a*t-b2+2*a*b-a2)*(2*t2+2*b*t-2*a*t-b2+2*a*b-a2)*exp((-b2+2*a*b-a2)/(4*t2))+2*t*((b+a-2)*(2*t2-(b+a-2)*(b+a-2))*exp((-b2+2*(a+b)-a2-2)/(2*t2))-(b+a)*(2*t2-(b+a)*(b+a))*exp((-a2-b2)/(2*t2))))/(16*t2*t2*t2));
  } else{
    throw std::invalid_argument("Covariance Type not Supported");
  }
}


//' int_0_1( d_k(x, a)/dx_i k(b, x) dx) (univariate)
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double w_ij_lebesgue(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
  if (ct == 1) {
    return(-((2 * (exp(-(a2 + b2)/(2*t2)) - exp((-a2 -b2 + 2 *(a + b -1))/(2*t2)))* t + (a-b) * exp(-(a-b)*(a-b)/(4 * t2)) * sqrt(pi) * (erf((-2+a+b)/(2 * t)) - erf((a+b)/(2* t)))))/(4 * t));
  } else if (ct == 2) {
    if (a > b) {
      return((-6*a*b*t - 3*sqrt(3.)*a*t2 - sqrt(3.)*b*t2 - 2*t3)/(4.*exp((sqrt(3.)*(a + b))/t)*t3) + (exp((sqrt(3.)*(-a + b))/t)*(2*sqrt(3.)*a2*a - 6*sqrt(3.)*a2*b + 6*sqrt(3.)*a*b2 - 2*sqrt(3.)*b2*b + 6*a2*t - 12*a*b*t + 6*b2*t - sqrt(3.)*a*t2 + sqrt(3.)*b*t2 - 2*t3))/(4.*t3) + (exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(3*sqrt(3.)*a*t2 - 3*sqrt(3.)*b*t2 + 2*t3))/(4.*t3) + (exp((sqrt(3.)*(-2 + a + b))/t)*(6*t - 6*a*t - 6*b*t + 6*a*b*t + 4*sqrt(3.)*t2 - 3*sqrt(3.)*a*t2 - sqrt(3.)*b*t2 + 2*t3))/(4.*t3));
    } else {
      return((exp((sqrt(3.)*(a - b))/t)*(2*sqrt(3.)*a2*a - 6*sqrt(3.)*a2*b +                                 \
             6*sqrt(3.)*a*b2 - 2*sqrt(3.)*b2*b - 6*a2*t + 12*a*b*t -                                         \
             6*b2*t + 3*sqrt(3.)*a*t2 - 3*sqrt(3.)*b*t2 -                                                    \
             2*t3))/(4.*t3) + (-6*a*b*t - 3*sqrt(3.)*a*t2 -                                                  \
             sqrt(3.)*b*t2 - 2*t3)/(4.*exp((sqrt(3.)*(a +                                                    \
             b))/t)*t3) + (exp((sqrt(3.)*(-2 + a + b))/t)*(6*t - 6*a*t -                                     \
             6*b*t + 6*a*b*t + 4*sqrt(3.)*t2 - 3*sqrt(3.)*a*t2 -                                             \
             sqrt(3.)*b*t2 + 2*t3))/(4.*t3) +                                                                \
             (exp((2*sqrt(3.)*a)/t - (sqrt(3.)*(a + b))/t)*(-(sqrt(3.)*a*t2) +                               \
             sqrt(3.)*b*t2 + 2*t3))/(4.*t3));
    }
  } else if (ct == 3) {
    if (a > b) {
      return((10*pow(a - b,2)*exp((sqrt(5.)*(-a + b))/t)*(sqrt(5.)*a2*a - sqrt(5.)*b2*b + 10*b2*t - 9*sqrt(5.)*b*t2 + 9*t3 +
             a2*(-3*sqrt(5.)*b + 10*t) + a*(3*sqrt(5.)*b2 - 20*b*t + 9*sqrt(5.)*t2)) +
             (3*t*((-50*a2*b2 - 40*sqrt(5.)*a2*b*t - 20*sqrt(5.)*a*b2*t - 50*a2*t2 -
             90*a*b*t2 - 10*b2*t2 - 25*sqrt(5.)*a*t3 - 11*sqrt(5.)*b*t3 - 18*t2*t2) * exp(-(sqrt(5.)*(a + b))/t) +
             exp(sqrt(5.)*(b - a)/t)*t2*(50*a2 + 50*b2 + 25*sqrt(5.)*a*t + 18*t2 - 25*b*(4*a + sqrt(5.)*t)))) + 
             3*t*(-(exp((sqrt(5.)*(-a + b))/t)*t2*(10*a2 - 20*a*b + 10*b2 + 11*sqrt(5.)*a*t                                   \
             - 11*sqrt(5.)*b*t + 18*t2)) + exp((sqrt(5.)*(-2 + a +                                                            \
             b))/t)*(10*b2*(5 + 2*sqrt(5.)*t + t2) - b*(100 +                                                                 \
             80*sqrt(5.)*t + 110*t2 + 11*sqrt(5.)*t3) + 2*(25 +                                                               \
             30*sqrt(5.)*t + 75*t2 + 18*sqrt(5.)*t3 + 9*t2*t2) +                                                              \
             10*a2*(5 + 5*b2 + 4*sqrt(5.)*t + 5*t2 - 2*b*(5 +                                                                 \
             2*sqrt(5.)*t)) - 5*a*(20 + 20*sqrt(5.)*t + 38*t2 +                                                               \
             5*sqrt(5.)*t3 + 4*b2*(5 + sqrt(5.)*t) - 2*b*(20 +                                                                \
             12*sqrt(5.)*t + 9*t2)))))/(108.*t2*t2*t));
    } else {
      return((10*pow(a - b,2)*exp((sqrt(5.)*(a - b))/t)*(sqrt(5.)*a2*a - sqrt(5.)*b2*b -                                       \
             10*b2*t - 9*sqrt(5.)*b*t2 - 9*t3 - a2*(3*sqrt(5.)*b + 10*t) + a*(3*sqrt(5.)*b2 + 20*b*t + 9*sqrt(5.)*t2)) +
             (3*t*((-t2*(10*b2 + 11*sqrt(5.)*b*t + 18*t2) - 10*a2*(5*b2 + 4*sqrt(5.)*b*t + 5*t2) - 5*a*t*(4*sqrt(5.)*b2 + 18*b*t + 5*sqrt(5.)*t2)) * exp(-sqrt(5.)*(a + b)/t) + 
             exp(sqrt(5.)*(a - b)/t)*t2*(10*b2 + 10*a2 + 11*sqrt(5.)*b*t + 18*t2 - a*(20*b + 11*sqrt(5.)*t)))) +                                                                      \
             3*t*(-(exp((sqrt(5.)*(a - b))/t)*t2*(50*a2 + 50*b2 +                                                                                                                     \
             25*sqrt(5.)*b*t + 18*t2 - 25*a*(4*b + sqrt(5.)*t))) +                                                                                                                    \
             exp((sqrt(5.)*(-2 + a + b))/t)*(10*b2*(5 + 2*sqrt(5.)*t +                                                                                                                \
             t2) - b*(100 + 80*sqrt(5.)*t + 110*t2 + 11*sqrt(5.)*t3) + 
             2*(25 + 30*sqrt(5.)*t + 75*t2 + 18*sqrt(5.)*t3 + 9*t2*t2) + 
             10*a2*(5 + 5*b2 + 4*sqrt(5.)*t + 5*t2 - 2*b*(5 + 2*sqrt(5.)*t)) - 
             5*a*(20 + 20*sqrt(5.)*t + 38*t2 + 5*sqrt(5.)*t3 + 4*b2*(5 + sqrt(5.)*t) - 2*b*(20 + 12*sqrt(5.)*t + 9*t2)))))/(108.*t2*t2*t));
    }
  }else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' int_0_1( d_k(x, a)/dx_i k(b, x) dnu(x)) (univariate), with nu a TRUNCATED gaussian measure with mean xm and variance xv.
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @param xm the mean of the gaussian measure
//' @param xv the variance of the gaussian measure
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double w_ij_tgauss(double a, double b, double t, int ct, double xm, double xv){
  double renorm = 1/(R::pnorm(1.0, xm, xv, 1, 0) - R::pnorm(0.0, xm, xv, 1, 0));
  if (ct == 1) {
    double b2 = b*b, t2 = t*t, tsv = t*sqrt(xv), stxv = sqrt(t2 + 2*xv);
    return((renorm) * (((2*t2*xv*stxv) * 
           exp((-1 + 2*xm)/t2 - pow(-1 + xm,2)/(2.*xv) - ((a + b - 2*xm)*(-1 + xm))/t2) + 
           exp((-1 + 2*xm)/t2 - pow(-1 + xm,2)/(2.*xv) + (pow(a + b - 2*xm,2)*xv*xv + pow(-1 + xm,2)*pow(t2 + 2*xv,2))/(2.*t2*xv*(t2 + 2*xv))) * 
           sqrt(2*pi)*tsv*(-(t2*xm) - b*xv + a*(t2 + xv)) *
           erf((-(t2*(-1 + xm)) - (-2 + a + b)*xv)/(sqrt(2.)*tsv*stxv))) + 
           ((-2*t2*xv*stxv) * 
           exp(-((a + b - 2*xm)*xm)/t2-xm*xm/(2.*xv)) + 
           exp((pow(a + b - 2*xm,2)*xv*xv + xm*xm*pow(t2 + 2*xv,2))/(2.*t2*xv*(t2 + 2*xv)) -xm*xm/(2.*xv)) * 
           sqrt(2*pi)*tsv*(-(t2*xm) - b*xv + a*(t2 + xv)) *
           erf((t2*xm + (a + b)*xv)/(sqrt(2.)*tsv*stxv)))) / 
           (2.*exp((a*a + b2 - 2*a*xm - 2*b*xm + 4*xm*xm)/(2.*t2))*sqrt(2*pi)*t*tsv*pow(t2 + 2*xv,1.5)));
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' int_0_1( d_k(x, a)/dx_i k(b, x) dnu(x)) (univariate), with nu a gaussian measure with mean xm and variance xv.
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @param xm the mean of the gaussian measure
//' @param xv the variance of the gaussian measure
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double w_ij_gauss(double a, double b, double t, int ct, double xm, double xv){
  //double renorm = 1/(R::pnorm(1.0, xm, xv, 1, 0) - R::pnorm(0.0, xm, xv, 1, 0));
  if (ct == 1) {
    double t2 = t*t;
    //return(-((pow(t,2)*xm + b*xv - a*(pow(t,2) + xv))/(exp((-2*b*pow(t,2)*xm + 2*pow(t,2)*pow(xm,2) + pow(a,2)*(pow(t,2) + xv) + pow(b,2)*(pow(t,2) + xv) - 2*a*(pow(t,2)*xm + b*xv))/(2.*pow(t,2)*(pow(t,2) + 2*xv)))*t*pow(pow(t,2) + 2*xv,1.5))));
    return(-((t2*xm + b*xv - a*(t2 + xv))/(exp((-2*b*t2*xm + 2*t2*xm*xm + a*a*(t2 + xv) + b*b*(t2 + xv) - 2*a*(t2*xm + b*xv))/(2.*t2*(t2 + 2*xv)))*t*pow(t2 + 2*xv,1.5))));
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}


//' int_0_1( d_k(x, a)/dx_i k(b, x) dx) (univariate)
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double grad_w_ij_lebesguea(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2 * t; 
  if (ct == 1) {
    return((exp(-(a2 - 2*a*b + b2)/(4.*t2))*sqrt(pi)*(a2 - 2*a*b + b2 - 2*t2)*erf((-2 + a + b)/(2.*t)) + 
           (-2*((-3*a + b)*exp(-(a2 + b2)/(2.*t2)) + (-2 + 3*a - b)*exp(-(2 -2*a - 2*b + a2 + b2)/(2.*t2)))*t - 
           exp(-(a2 - 2*a*b + b2)/(4.*t2))*sqrt(pi)*(a2 - 2*a*b + b2 - 2*t2)*erf((a + b)/(2.*t))))/
             (8.*t3));
  } else if (ct == 2) {
    if (a > b) {
      return((6*sqrt(3.)*a*b*t + 9*a*t2 - 3*b*t2 -                                    \
             sqrt(3.)*t3)/(4.*exp((sqrt(3.)*(a + b))/t)*t2*t2) +                      \
             (exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(6*sqrt(3.)*t - \
             6*sqrt(3.)*a*t - 6*sqrt(3.)*b*t + 6*sqrt(3.)*a*b*t + 6*t2 -              \
             9*a*t2 + 3*b*t2 - sqrt(3.)*t3))/(4.*t2*t2) +                             \
             (exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(-6*a2*a +                 \
             18*a2*b - 18*a*b2 + 6*b2*b + 6*a*t2 -                                    \
             6*b*t2 + 2*sqrt(3.)*t3))/(4.*t2*t2));
    } else {
      return((6*sqrt(3.)*a*b*t + 9*a*t2 - 3*b*t2 -                                    \
             sqrt(3.)*t3)/(4.*exp((sqrt(3.)*(a + b))/t)*t2*t2) +                      \
             (exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(6*sqrt(3.)*t - \
             6*sqrt(3.)*a*t - 6*sqrt(3.)*b*t + 6*sqrt(3.)*a*b*t + 6*t2 -              \
             9*a*t2 + 3*b*t2 - sqrt(3.)*t3))/(4.*t2*t2) +                             \
             (exp((2*sqrt(3.)*a)/t - (sqrt(3.)*(a + b))/t)*(6*a2*a -                  \
             18*a2*b + 18*a*b2 - 6*b2*b - 6*a*t2 +                                    \
             6*b*t2 + 2*sqrt(3.)*t3))/(4.*t2*t2));
    }
  } else if (ct == 3) {
    if (a > b) {
      return((exp((sqrt(5.)*(-a + b))/t)*(500*a2*b2*b - 250*a*b2*b2 -                                                                                      \
             300*sqrt(5.)*a2*b2*t + 200*sqrt(5.)*a*b2*b*t +                                                                                                \
             150*a2*b*t2 - 150*a*b2*t2 +                                                                                                                   \
             60*sqrt(5.)*a2*t3 - 120*sqrt(5.)*a*b*t3 +                                                                                                     \
             210*a*t2*t2))/(108.*t3*t3) + (exp((-2*sqrt(5.)*b)/t +                                                                                         \
             (sqrt(5.)*(-a + b))/t)*(150*sqrt(5.)*a2*b2*t -                                                                                                \
             300*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t +                                                                                           \
             150*sqrt(5.)*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t +                                                                                          \
             600*sqrt(5.)*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t -                                                                                         \
             300*sqrt(5.)*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t -                                                                                        \
             300*sqrt(5.)*a*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t +                                                                                        \
             150*sqrt(5.)*a2*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t +                                                                                       \
             600*a2*b*t2 - 1200*a*exp((2*sqrt(5.)*(-1 + a +                                                                                                \
             b))/t)*t2 + 600*a2*exp((2*sqrt(5.)*(-1 + a +                                                                                                  \
             b))/t)*t2 + 1200*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 -                                                                                    \
             600*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 +                                                                                                \
             150*sqrt(5.)*a2*t3 + 30*sqrt(5.)*a*b*t3 -                                                                                                     \
             330*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 +                                                                                          \
             150*sqrt(5.)*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 +                                                                                         \
             30*sqrt(5.)*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 +                                                                                         \
             75*a*t2*t2 - 75*a*exp((2*sqrt(5.)*(-1 + a +                                                                                                   \
             b))/t)*t2*t2))/(108.*t3*t3) + (exp(-((sqrt(5.)*(a - 3*b))/t) -                                                                                \
             (4*sqrt(5.)*b)/t)*(-30*sqrt(5.)*b2*t3 - 105*b*t2*t2 -                                                                                         \
             21*sqrt(5.)*t2*t2*t))/(108.*t3*t3) + (exp(-((sqrt(5.)*(a - 3*b))/t)  -                                                                        \
             (4*sqrt(5.)*b)/t + (2*sqrt(5.)*(-1 + a + b))/t)*(150*sqrt(5.)*t -                                                                             \
             300*sqrt(5.)*b*t + 150*sqrt(5.)*b2*t + 600*t2 -                                                                                               \
             600*b*t2 + 150*sqrt(5.)*t3 + 30*sqrt(5.)*b*t3 -                                                                                               \
             30*sqrt(5.)*b2*t3 - 30*t2*t2 + 105*b*t2*t2 -                                                                                                  \
             21*sqrt(5.)*t2*t2*t))/(108.*t3*t3) + (exp(-((sqrt(5.)*(a - 3*b))/t)-                                                                          \
             (2*sqrt(5.)*b)/t)*(-50*a2*a2*a + 250*a2*a2*b -                                                                                                \
             500*a2*a*b2 + 50*b2*b2*b - 50*sqrt(5.)*a2*a2*t +                                                                                              \
             200*sqrt(5.)*a2*a*b*t - 50*sqrt(5.)*b2*b2*t -                                                                                                 \
             50*a2*a*t2 + 50*b2*b*t2 +                                                                                                                     \
             60*sqrt(5.)*b2*t3 - 210*b*t2*t2 +                                                                                                             \
             42*sqrt(5.)*t2*t2*t))/(108.*t3*t3));
    } else {
      return((150*sqrt(5.)*a2*b2*t + 600*a2*b*t2 +                                                                                      \
             150*sqrt(5.)*a2*t3 + 30*sqrt(5.)*a*b*t3 -                                                                                  \
             30*sqrt(5.)*b2*t3 + 75*a*t2*t2 - 105*b*t2*t2 -                                                                             \
             21*sqrt(5.)*t2*t2*t)/(108.*exp((sqrt(5.)*(a + b))/t)*t3*t3) +                                                              \
             (exp((2*sqrt(5.)*(-1 + a + b))/t - (sqrt(5.)*(a + b))/t)*(150*sqrt(5.)*t                                                   \
             - 300*sqrt(5.)*a*t + 150*sqrt(5.)*a2*t - 300*sqrt(5.)*b*t +                                                                \
             600*sqrt(5.)*a*b*t - 300*sqrt(5.)*a2*b*t + 150*sqrt(5.)*b2*t                                                               \
             - 300*sqrt(5.)*a*b2*t + 150*sqrt(5.)*a2*b2*t +                                                                             \
             600*t2 - 1200*a*t2 + 600*a2*t2 -                                                                                           \
             600*b*t2 + 1200*a*b*t2 - 600*a2*b*t2 +                                                                                     \
             150*sqrt(5.)*t3 - 330*sqrt(5.)*a*t3 +                                                                                      \
             150*sqrt(5.)*a2*t3 + 30*sqrt(5.)*b*t3 +                                                                                    \
             30*sqrt(5.)*a*b*t3 - 30*sqrt(5.)*b2*t3 - 30*t2*t2-                                                                         \
             75*a*t2*t2 + 105*b*t2*t2 -                                                                                                 \
             21*sqrt(5.)*t2*t2*t))/(108.*t3*t3) + (exp((2*sqrt(5.)*a)/t -                                                               \
             (sqrt(5.)*(a + b))/t)*(50*a2*a2*a - 250*a2*a2*b +                                                                          \
             500*a2*a*b2 - 500*a2*b2*b + 250*a*b2*b2 -                                                                                  \
             50*b2*b2*b - 50*sqrt(5.)*a2*a2*t + 200*sqrt(5.)*a2*a*b*t -                                                                 \
             300*sqrt(5.)*a2*b2*t + 200*sqrt(5.)*a*b2*b*t -                                                                             \
             50*sqrt(5.)*b2*b2*t + 50*a2*a*t2 -                                                                                         \
             150*a2*b*t2 + 150*a*b2*t2 -                                                                                                \
             50*b2*b*t2 + 60*sqrt(5.)*a2*t3 -                                                                                           \
             120*sqrt(5.)*a*b*t3 + 60*sqrt(5.)*b2*t3 -                                                                                  \
             210*a*t2*t2 + 210*b*t2*t2 +                                                                                                \
             42*sqrt(5.)*t2*t2*t))/(108.*t3*t3));
    }
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' int_0_1( d_k(x, a)/dx_i k(b, x) dx) (univariate)
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double grad_w_ij_lebesgueb(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2 * t;
  if (ct == 1) {
    return((-(exp((-a2 + 2*a*b - b2)/(4.*t2))*sqrt(pi)*(a2 - 2*a*b + b2 - 2*t2)*erf((-2 + a + b)/(2.*t))) + 
           (-2*(-((a + b)*exp(-(a2 + b2)/(2.*t2))) + (-2 + a + b)*exp(-(2 - 2 * a - 2 * b + a2 + b2)/(2.*t2)))*t + 
           exp(-(a2 - 2*a*b + b2)/(4.*t2))*sqrt(pi)*(a2 - 2*a*b + b2 - 2*t2)*erf((a + b)/(2.*t)))) / 
           (8.*t3));
  } else if (ct == 2) {
    if (a > b) {
      return((exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(6*a2*a -                  \
             18*a2*b + 18*a*b2 - 6*b2*b - 6*a*t2 +                                    \
             6*b*t2 - 2*sqrt(3.)*t3))/(4.*t2*t2) +                                    \
             (exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(6*sqrt(3.)*t - \
             6*sqrt(3.)*a*t - 6*sqrt(3.)*b*t + 6*sqrt(3.)*a*b*t + 6*t2 -              \
             3*a*t2 - 3*b*t2 + sqrt(3.)*t3))/(4.*t2*t2) +                             \
             (6*sqrt(3.)*a*b*t + 3*a*t2 + 3*b*t2 +                                    \
             sqrt(3.)*t3)/(4.*exp((sqrt(3.)*(a + b))/t)*t2*t2));
    } else {
      return((exp((2*sqrt(3.)*a)/t - (sqrt(3.)*(a + b))/t)*(-6*a2*a + 18*a2*b - 18*a*b2 + 6*b2*b + 6*a*t2 - 6*b*t2 - 2*sqrt(3.)*t3))/(4.*t2*t2) + (exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(6*sqrt(3.)*t - 6*sqrt(3.)*a*t - 6*sqrt(3.)*b*t + 6*sqrt(3.)*a*b*t + 6*t2 - 3*a*t2 - 3*b*t2 + sqrt(3.)*t3))/(4.*t2*t2) + (6*sqrt(3.)*a*b*t + 3*a*t2 + 3*b*t2 + sqrt(3.)*t3)/(4.*exp((sqrt(3.)*(a + b))/t)*t2*t2));
    }
  } else if (ct == 3) {
    if (a > b) {
      return((exp(-((sqrt(5.)*(a - 3*b))/t) - (2*sqrt(5.)*b)/t)*(50*a2*a2*a - 250*a2*a2*b + 500*a2*a*b2 + 50*sqrt(5.)*a2*a2*t - 200*sqrt(5.)*a2*a*b*t + 50*a2*a*t2))/(108.*t3*t3) + (exp((sqrt(5.)*(-a + b))/t)*(-500*a2*b2*b + 250*a*b2*b2 - 50*b2*b2*b + 300*sqrt(5.)*a2*b2*t - 200*sqrt(5.)*a*b2*b*t + 50*sqrt(5.)*b2*b2*t - 150*a2*b*t2 + 150*a*b2*t2 - 50*b2*b*t2 - 60*sqrt(5.)*a2*t3 + 120*sqrt(5.)*a*b*t3 - 60*sqrt(5.)*b2*t3 - 210*a*t2*t2 + 210*b*t2*t2 - 42*sqrt(5.)*t2*t2*t))/(108.*t3*t3) + (exp((-2*sqrt(5.)*b)/t + (sqrt(5.)*(-a + b))/t)*(150*sqrt(5.)*a2*b2*t + 150*sqrt(5.)*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 300*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 150*sqrt(5.)*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 300*sqrt(5.)*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 600*sqrt(5.)*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 300*sqrt(5.)*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 150*sqrt(5.)*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 300*sqrt(5.)*a*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 150*sqrt(5.)*a2*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 300*a2*b*t2 + 300*a*b2*t2 + 600*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 - 900*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 300*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 - 900*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 1200*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 - 300*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 300*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 - 300*a*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 30*sqrt(5.)*a2*t3 + 150*sqrt(5.)*a*b*t3 + 30*sqrt(5.)*b2*t3 + 210*sqrt(5.)*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 - 210*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 + 30*sqrt(5.)*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 - 210*sqrt(5.)*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 + 150*sqrt(5.)*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 + 30*sqrt(5.)*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 + 105*a*t2*t2 + 105*b*t2*t2 + 210*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 - 105*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 - 105*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 + 21*sqrt(5.)*t2*t2*t + 21*sqrt(5.)*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2*t))/(108.*t3*t3));
    } else {
      return((exp((2*sqrt(5.)*a)/t - (sqrt(5.)*(a + b))/t)*(-50*a2*a2*a +       \
             250*a2*a2*b - 500*a2*a*b2 + 500*a2*b2*b -                          \
             250*a*b2*b2 + 50*b2*b2*b + 50*sqrt(5.)*a2*a2*t -                   \
             200*sqrt(5.)*a2*a*b*t + 300*sqrt(5.)*a2*b2*t -                     \
             200*sqrt(5.)*a*b2*b*t + 50*sqrt(5.)*b2*b2*t -                      \
             50*a2*a*t2 + 150*a2*b*t2 -                                         \
             150*a*b2*t2 + 50*b2*b*t2 -                                         \
             60*sqrt(5.)*a2*t3 + 120*sqrt(5.)*a*b*t3 -                          \
             60*sqrt(5.)*b2*t3 + 210*a*t2*t2 - 210*b*t2*t2 -                    \
             42*sqrt(5.)*t2*t2*t))/(108.*t3*t3) + (exp((2*sqrt(5.)*(-1 + a +    \
             b))/t - (sqrt(5.)*(a + b))/t)*(150*sqrt(5.)*t - 300*sqrt(5.)*a*t + \
             150*sqrt(5.)*a2*t - 300*sqrt(5.)*b*t + 600*sqrt(5.)*a*b*t -        \
             300*sqrt(5.)*a2*b*t + 150*sqrt(5.)*b2*t -                          \
             300*sqrt(5.)*a*b2*t + 150*sqrt(5.)*a2*b2*t +                       \
             600*t2 - 900*a*t2 + 300*a2*t2 -                                    \
             900*b*t2 + 1200*a*b*t2 - 300*a2*b*t2 +                             \
             300*b2*t2 - 300*a*b2*t2 +                                          \
             210*sqrt(5.)*t3 - 210*sqrt(5.)*a*t3 +                              \
             30*sqrt(5.)*a2*t3 - 210*sqrt(5.)*b*t3 +                            \
             150*sqrt(5.)*a*b*t3 + 30*sqrt(5.)*b2*t3 +                          \
             210*t2*t2 - 105*a*t2*t2 - 105*b*t2*t2 +                            \
             21*sqrt(5.)*t2*t2*t))/(108.*t3*t3) +                               \
             (150*sqrt(5.)*a2*b2*t + 300*a2*b*t2 +                              \
             300*a*b2*t2 + 30*sqrt(5.)*a2*t3 +                                  \
             150*sqrt(5.)*a*b*t3 + 30*sqrt(5.)*b2*t3 +                          \
             105*a*t2*t2 + 105*b*t2*t2 +                                        \
             21*sqrt(5.)*t2*t2*t)/(108.*exp((sqrt(5.)*(a + b))/t)*t3*t3));
    }
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}


//' gradient of w_ij with respect to t (univariate)
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The the derivative of the scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double grad_w_ij_dt_cpp(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t;
  if (ct == 1) {
    return((-2*(a-b)*t*((b+a)*exp(-(b2+a2)/(2*t2))-(b+a-2)*exp(-(b2+a2-2*a-2*b+2)/(2*t2)))+4*t*(-(b2+a2)*exp(-(b2+a2)/(2*t2))-(-b2+2*(b+a-1)-a2)*exp((-b2+2*(b+a-1)-a2)/(2*t2)))+(2*sqrt(pi)*(a-b)*t2-sqrt(pi)*(a-b)*(a-b)*(a-b))*(erf((b+a-2)/(2*t))-erf((b+a)/(2*t)))*exp(-(a-b)*(a-b)/(4*t2)))/(8*t2*t2)
);
  } else{
    throw std::invalid_argument("Covariance Type not Supported");
  }
}


//' int_0_1 k(x, a) * k(b, x) dx
//' @noRd
// [[Rcpp::export]]
double Ikk_lebesgue(double a, double b, double t, int ct){
  if (ct == 1) {
    return((sqrt(pi)*(erf((b+a)/(2.*t)) - erf((b+a-2)/(2.*t)))*t*exp(-(b-a)*(b-a)/(4.*t*t)))/2.);
  } else if (ct == 2) {
    if (b > a) {
      double temp = a;
      a = b;
      b = temp;
    }
    double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
    return((-6*sqrt(3.)*a*b*t - 9*a*t2 - 9*b*t2 -                            \
           5*sqrt(3.)*t3)/(12.*exp((sqrt(3.)*(a + b))/t)*t2) +               \
           (exp((sqrt(3.)*(-2 + a + b))/t)*(-6*sqrt(3.)*t + 6*sqrt(3.)*a*t + \
           6*sqrt(3.)*b*t - 6*sqrt(3.)*a*b*t - 18*t2 + 9*a*t2 +              \
           9*b*t2 - 5*sqrt(3.)*t3))/(12.*t2) +                               \
           (exp((sqrt(3.)*(-a + b))/t)*(6*a2*a - 18*a2*b +                   \
           18*a*b2 - 6*b2*b + 12*sqrt(3.)*a2*t - 24*sqrt(3.)*a*b*t           \
           + 12*sqrt(3.)*b2*t + 21*a*t2 - 21*b*t2 +                          \
           5*sqrt(3.)*t3))/(12.*t2) + (exp((2*sqrt(3.)*b)/t -                \
           (sqrt(3.)*(a + b))/t)*(9*a*t2 - 9*b*t2 +                          \
           5*sqrt(3.)*t3))/(12.*t2));
  } else if (ct == 3) {
    if (b > a) {
      double temp = a;
      a = b;
      b = temp;
    }
    double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
    return((10*(a - b)*exp((sqrt(5.)*(-a + b))/t)*(5*a2*a2 + 5*b2*b2 -   
           15*sqrt(5.)*b2*b*t + 105*b2*t2 - 54*sqrt(5.)*b*t3 + 
           54*t2*t2 - 5*a2*a*(4*b - 3*sqrt(5.)*t) + 
           15*a2*(2*b2 - 3*sqrt(5.)*b*t + 7*t2) +  
           a*(-20*b2*b + 45*sqrt(5.)*b2*t - 210*b*t2 + 54*sqrt(5.)*t3)) + 
           (3*t*((-50*sqrt(5.)*a2*b2 - 200*a2*b*t - 200*a*b2*t - 50*sqrt(5.)*a2*t2 -
           170*sqrt(5.)*a*b*t2 - 50*sqrt(5.)*b2*t2 - 225*a*t3 - 225*b*t3 - 63*sqrt(5.)*t2*t2)*exp(-sqrt(5.)*(a + b)/t) +
           exp(sqrt(5.)*(b - a)/t)*t2*(50*sqrt(5.)*a2 + 50*sqrt(5.)*b2 - 225*b*t +
           63*sqrt(5.)*t2 + 25*a*(-4*sqrt(5.)*b + 9*t)))) + 
           3*t*(exp((sqrt(5.)*(-a + b))/t)*t2*(50*sqrt(5.)*a2 + 
           50*sqrt(5.)*b2 - 25*a*(4*sqrt(5.)*b - 9*t) - 225*b*t + 
           63*sqrt(5.)*t2) - exp((sqrt(5.)*(-2 + a + b))/t)*(50*sqrt(5.) + 
           400*t + 270*sqrt(5.)*t2 + 450*t3 + 63*sqrt(5.)*t2*t2 + 
           50*b2*(sqrt(5.) + 4*t + sqrt(5.)*t2) - 5*b*(20*sqrt(5.) + 
           120*t + 54*sqrt(5.)*t2 + 45*t3) + 50*a2*(sqrt(5.) +  
           sqrt(5.)*b2 + 4*t + sqrt(5.)*t2 - 2*b*(sqrt(5.) + 2*t)) -  
           5*a*(20*sqrt(5.) + 120*t + 54*sqrt(5.)*t2 + 45*t3 + 
           20*b2*(sqrt(5.) + 2*t) - 2*b*(20*sqrt(5.) + 80*t + 
           17*sqrt(5.)*t2)))))/(540.*t2*t2));
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' int_0_1 k(x, a) * k(b, x) dnu(x), where nu is the TRUNCATED gaussian measure with mean xm and variance xv
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @param xm the mean of the gaussian measure
//' @param xv the variance of the gaussian measure
//' @noRd
// [[Rcpp::export]]
double Ikk_tgauss(double a, double b, double t, int ct, double xm, double xv) {
  double renorm = 1/(R::pnorm(1.0, xm, xv, 1, 0) - R::pnorm(0.0, xm, xv, 1, 0));
  if (ct == 1) {
    double t2 = t*t;
    return((renorm) * (t*(erf((-(t2*(-1 + xm)) - (-2 + a + b)*xv)/(sqrt(2.)*t*sqrt(xv)*sqrt(t2 + 2*xv))) + 
           erf((t2*xm + (a + b)*xv)/(sqrt(2.)*t*sqrt(xv)*sqrt(t2 + 2*xv))))) / 
           (2.*exp((-2*b*t2*xm + 2*t2*xm*xm + a*a*(t2 + xv) + b*b*(t2 + xv) - 2*a*(t2*xm + b*xv))/(2.*t2*(t2 + 2*xv)))*sqrt(t2 + 2*xv)));
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' int_R k(x, a) * k(b, x) dx, where nu is the gaussian maeasure with mean xm and variance xv
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @param xm the mean of the gaussian measure
//' @param xv the variance of the gaussian measure
//' @noRd
// [[Rcpp::export]]
double Ikk_gauss(double a, double b, double t, int ct, double xm, double xv) {
  // double renorm = 1/(R::pnorm(1.0, xm, xv, 1, 0) - R::pnorm(0.0, xm, xv, 1, 0));
  if (ct == 1) {
    double t2 = t*t;
    return(t/(exp((-2*b*t2*xm + 2*t2*xm*xm + a*a*(t2 + xv) + b*b*(t2 + xv) - 2*a*(t2*xm + b*xv))/(2.*t2*(t2 + 2*xv)))*sqrt(t2 + 2*xv)));
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}


//' Derivative of int_0_1 k(x, a) * k(b, x) dx With respect to a.
//' @noRd
// [[Rcpp::export]]
double grad_Ikk_lebesguea(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
  if (ct == 1) {
    return(-exp(-pow(a - b,2)/(4.*t2) - pow(-2 + a +                       \
           b,2)/(4.*t2))/2. + exp(-pow(a - b,2)/(4.*t2) - pow(a +          \
           b,2)/(4.*t2))/2. + (sqrt(pi)*(a*erf((-2 + a + b)/(2.*t)) -      \
           b*erf((-2 + a + b)/(2.*t)) - a*erf((a + b)/(2.*t)) + b*erf((a + \
           b)/(2.*t))))/(4.*exp(pow(a - b,2)/(4.*t2))*t));
  } else if(ct == 2) {
    if (a > b) {
      return((exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(-2*sqrt(3.)*a2*a + 6*sqrt(3.)*a2*b - 6*sqrt(3.)*a*b2 + 2*sqrt(3.)*b2*b - 6*a2*t + 12*a*b*t - 6*b2*t - 2*sqrt(3.)*a*t2 + 2*sqrt(3.)*b*t2))/(4.*t3) + (exp((2*sqrt(3.)*(-1 + a + b))/t - (sqrt(3.)*(a + b))/t)*(-6*t + 6*a*t + 6*b*t - 6*a*b*t - 4*sqrt(3.)*t2 + 3*sqrt(3.)*a*t2 + sqrt(3.)*b*t2 - 2*t3))/(4.*t3) + (6*a*b*t + 3*sqrt(3.)*a*t2 + sqrt(3.)*b*t2 + 2*t3)/(4.*exp((sqrt(3.)*(a + b))/t)*t3));
    } else {
      return((exp((2*sqrt(3.)*a)/t - (sqrt(3.)*(a + b))/t)*(-2*sqrt(3.)*a2*a + \
             6*sqrt(3.)*a2*b - 6*sqrt(3.)*a*b2 + 2*sqrt(3.)*b2*b +             \
             6*a2*t - 12*a*b*t + 6*b2*t - 2*sqrt(3.)*a*t2 +                    \
             2*sqrt(3.)*b*t2))/(4.*t3) + (exp((2*sqrt(3.)*(-1 + a +            \
             b))/t - (sqrt(3.)*(a + b))/t)*(-6*t + 6*a*t + 6*b*t - 6*a*b*t -   \
             4*sqrt(3.)*t2 + 3*sqrt(3.)*a*t2 + sqrt(3.)*b*t2 -                 \
             2*t3))/(4.*t3) + (6*a*b*t + 3*sqrt(3.)*a*t2 +                     \
             sqrt(3.)*b*t2 + 2*t3)/(4.*exp((sqrt(3.)*(a +                      \
             b))/t)*t3));
    }
  } else if (ct == 3) {
    if (a > b) {
      return((exp(-sqrt(5.)*(a - b)/t)*(-10*sqrt(5.)*a2*a2*a + 50*sqrt(5.)*a2*a2*b - 100*sqrt(5.)*a2*a*b2 - 100*a2*a2*t + 400*a2*a*b*t - 90*sqrt(5.)*a2*a*t2))/(108.*t2*t2*t) + (exp((sqrt(5.)*(-a + b))/t)*(100*sqrt(5.)*a2*b2*b - 50*sqrt(5.)*a*b2*b2 + 10*sqrt(5.)*b2*b2*b - 600*a2*b2*t + 400*a*b2*b*t - 100*b2*b2*t + 270*sqrt(5.)*a2*b*t2 - 270*sqrt(5.)*a*b2*t2 + 90*sqrt(5.)*b2*b*t2 - 210*a2*t3 + 420*a*b*t3 - 210*b2*t3 - 42*sqrt(5.)*a*t2*t2 + 42*sqrt(5.)*b*t2*t2))/(108.*t2*t2*t) + (exp(-sqrt(5.)*(a + b)/t)*(150*a2*b2*t - 150*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 300*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 150*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 300*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 600*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 300*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 150*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 300*a*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 150*a2*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t + 120*sqrt(5.)*a2*b*t2 + 60*sqrt(5.)*a*b2*t2 - 180*sqrt(5.)*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 300*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 - 120*sqrt(5.)*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 240*sqrt(5.)*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 - 360*sqrt(5.)*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 120*sqrt(5.)*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 - 60*sqrt(5.)*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 60*sqrt(5.)*a*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 + 150*a2*t3 + 270*a*b*t3 + 30*b2*t3 - 450*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 + 570*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 - 150*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 + 330*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 - 270*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 - 30*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 + 75*sqrt(5.)*a*t2*t2 + 33*sqrt(5.)*b*t2*t2 - 108*sqrt(5.)*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 + 75*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 + 33*sqrt(5.)*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 + 54*t2*t2*t - 54*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2*t))/(108.*t2*t2*t));
    } else {
      return((exp(sqrt(5.)*(a - b)/t)*(-10*sqrt(5.)*a2*a2*a + 50*sqrt(5.)*a2*a2*b - 100*sqrt(5.)*a2*a*b2 + 100*sqrt(5.)*a2*b2*b - 50*sqrt(5.)*a*b2*b2 + 10*sqrt(5.)*b2*b2*b + 100*a2*a2*t - 400*a2*a*b*t + 600*a2*b2*t - 400*a*b2*b*t + 100*b2*b2*t - 90*sqrt(5.)*a2*a*t2 + 270*sqrt(5.)*a2*b*t2 - 270*sqrt(5.)*a*b2*t2 + 90*sqrt(5.)*b2*b*t2 + 210*a2*t3 - 420*a*b*t3 + 210*b2*t3 - 42*sqrt(5.)*a*t2*t2 + 42*sqrt(5.)*b*t2*t2))/(108.*t2*t2*t) + (exp(sqrt(5.)*(-2 + a + b)/t)*(-150*t + 300*a*t - 150*a2*t + 300*b*t - 600*a*b*t + 300*a2*b*t - 150*b2*t + 300*a*b2*t - 150*a2*b2*t - 180*sqrt(5.)*t2 + 300*sqrt(5.)*a*t2 - 120*sqrt(5.)*a2*t2 + 240*sqrt(5.)*b*t2 - 360*sqrt(5.)*a*b*t2 + 120*sqrt(5.)*a2*b*t2 - 60*sqrt(5.)*b2*t2 + 60*sqrt(5.)*a*b2*t2 - 450*t3 + 570*a*t3 - 150*a2*t3 + 330*b*t3 - 270*a*b*t3 - 30*b2*t3 - 108*sqrt(5.)*t2*t2 + 75*sqrt(5.)*a*t2*t2 + 33*sqrt(5.)*b*t2*t2 - 54*t2*t2*t))/(108.*t2*t2*t) + (exp(-sqrt(5.)*(a + b)/t)*(150*a2*b2*t + 120*sqrt(5.)*a2*b*t2 + 60*sqrt(5.)*a*b2*t2 + 150*a2*t3 + 270*a*b*t3 + 30*b2*t3 + 75*sqrt(5.)*a*t2*t2 + 33*sqrt(5.)*b*t2*t2 + 54*t2*t2*t))/(108.*t2*t2*t));
    }
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' Derivative of int_0_1 k(x, a) * k(b, x) dx With respect to b.
//' @noRd
// [[Rcpp::export]]
double grad_Ikk_lebesgueb(double a, double b, double t, int ct){
  double a2 = a*a, b2 = b*b, t2 = t*t, t3 = t2*t;
  if (ct == 1) {
    return(-exp(-pow(a - b,2)/(4.*t2) - pow(-2 + a +                       \
           b,2)/(4.*t2))/2. + exp(-pow(a - b,2)/(4.*t2) - pow(a +          \
           b,2)/(4.*t2))/2. + (sqrt(pi)*(-(a*erf((-2 + a + b)/(2.*t))) +   \
           b*erf((-2 + a + b)/(2.*t)) + a*erf((a + b)/(2.*t)) - b*erf((a + \
           b)/(2.*t))))/(4.*exp(pow(a - b,2)/(4.*t2))*t));
  } else if(ct == 2) {
    if (a > b) {
      return((exp((2*sqrt(3.)*b)/t - (sqrt(3.)*(a + b))/t)*(2*sqrt(3.)*a2*a -  \
             6*sqrt(3.)*a2*b + 6*sqrt(3.)*a*b2 - 2*sqrt(3.)*b2*b +             \
             6*a2*t - 12*a*b*t + 6*b2*t + 2*sqrt(3.)*a*t2 -                    \
             2*sqrt(3.)*b*t2))/(4.*t3) + (exp((2*sqrt(3.)*(-1 + a +            \
             b))/t - (sqrt(3.)*(a + b))/t)*(-6*t + 6*a*t + 6*b*t - 6*a*b*t -   \
             4*sqrt(3.)*t2 + sqrt(3.)*a*t2 + 3*sqrt(3.)*b*t2 -                 \
             2*t3))/(4.*t3) + (6*a*b*t + sqrt(3.)*a*t2 +                       \
             3*sqrt(3.)*b*t2 + 2*t3)/(4.*exp((sqrt(3.)*(a +                    \
             b))/t)*t3));
    } else {
      return((exp((2*sqrt(3.)*a)/t - (sqrt(3.)*(a + b))/t)*(2*sqrt(3.)*a2*a -  \
             6*sqrt(3.)*a2*b + 6*sqrt(3.)*a*b2 - 2*sqrt(3.)*b2*b -             \
             6*a2*t + 12*a*b*t - 6*b2*t + 2*sqrt(3.)*a*t2 -                    \
             2*sqrt(3.)*b*t2))/(4.*t3) + (exp((2*sqrt(3.)*(-1 + a +            \
             b))/t - (sqrt(3.)*(a + b))/t)*(-6*t + 6*a*t + 6*b*t - 6*a*b*t -   \
             4*sqrt(3.)*t2 + sqrt(3.)*a*t2 + 3*sqrt(3.)*b*t2 -                 \
             2*t3))/(4.*t3) + (6*a*b*t + sqrt(3.)*a*t2 +                       \
             3*sqrt(3.)*b*t2 + 2*t3)/(4.*exp((sqrt(3.)*(a +                    \
             b))/t)*t3));
    }
  } else if (ct == 3) {
    if (a > b) {
      return((exp(-((sqrt(5.)*(a - 3*b))/t) - (2*sqrt(5.)*b)/t)*(10*sqrt(5.)*a2*a2*a -                                                                                                                       \
             50*sqrt(5.)*a2*a2*b + 100*sqrt(5.)*a2*a*b2 +                                                                                                                                                    \
             100*a2*a2*t - 400*a2*a*b*t +                                                                                                                                                                    \
             90*sqrt(5.)*a2*a*t2))/(108.*t2*t2*t) + (exp((sqrt(5.)*(-a +                                                                                                                                     \
             b))/t)*(-100*sqrt(5.)*a2*b2*b + 50*sqrt(5.)*a*b2*b2 -                                                                                                                                           \
             10*sqrt(5.)*b2*b2*b + 600*a2*b2*t - 400*a*b2*b*t +                                                                                                                                              \
             100*b2*b2*t - 270*sqrt(5.)*a2*b*t2 +                                                                                                                                                            \
             270*sqrt(5.)*a*b2*t2 - 90*sqrt(5.)*b2*b*t2 +                                                                                                                                                    \
             210*a2*t3 - 420*a*b*t3 + 210*b2*t3 +                                                                                                                                                            \
             42*sqrt(5.)*a*t2*t2 - 42*sqrt(5.)*b*t2*t2))/(108.*t2*t2*t) +                                                                                                                                    \
             (exp((-2*sqrt(5.)*b)/t + (sqrt(5.)*(-a +                                                                                                                                                        \
             b))/t)*(150*a2*b2*t - 150*exp((2*sqrt(5.)*(-1 + a +                                                                                                                                             \
             b))/t)*t + 300*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t -                                                                                                                                           \
             150*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t +                                                                                                                                                     \
             300*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t - 600*a*b*exp((2*sqrt(5.)*(-1 +                                                                                                                        \
             a + b))/t)*t + 300*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t -                                                                                                                                    \
             150*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t +                                                                                                                                                     \
             300*a*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t -                                                                                                                                                   \
             150*a2*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t +                                                                                                                                                  \
             60*sqrt(5.)*a2*b*t2 + 120*sqrt(5.)*a*b2*t2 -                                                                                                                                                    \
             180*sqrt(5.)*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 +                                                                                                                                              \
             240*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 -                                                                                                                                            \
             60*sqrt(5.)*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 +                                                                                                                                            \
             300*sqrt(5.)*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 -                                                                                                                                            \
             360*sqrt(5.)*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 +                                                                                                                                          \
             60*sqrt(5.)*a2*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 -                                                                                                                                          \
             120*sqrt(5.)*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 +                                                                                                                                           \
             120*sqrt(5.)*a*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2 +                                                                                                                                         \
             30*a2*t3 + 270*a*b*t3 + 150*b2*t3 -                                                                                                                                                             \
             450*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 +                                                                                                                                                       \
             330*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 -                                                                                                                                                     \
             30*a2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 +                                                                                                                                                     \
             570*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 -                                                                                                                                                     \
             270*a*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 -                                                                                                                                                   \
             150*b2*exp((2*sqrt(5.)*(-1 + a + b))/t)*t3 +                                                                                                                                                    \
             33*sqrt(5.)*a*t2*t2 + 75*sqrt(5.)*b*t2*t2 -                                                                                                                                                     \
             108*sqrt(5.)*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 +                                                                                                                                           \
             33*sqrt(5.)*a*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 +                                                                                                                                          \
             75*sqrt(5.)*b*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2 + 54*t2*t2*t -                                                                                                                             \
             54*exp((2*sqrt(5.)*(-1 + a + b))/t)*t2*t2*t))/(108.*t2*t2*t));
    } else {
      return((exp((-2*sqrt(5.)*a)/t + (sqrt(5.)*(3*a - b))/t)*(10*sqrt(5.)*a2*a2*a - 50*sqrt(5.)*a2*a2*b + 100*sqrt(5.)*a2*a*b2 - 100*sqrt(5.)*a2*b2*b + 50*sqrt(5.)*a*b2*b2 - 10*sqrt(5.)*b2*b2*b - 100*a2*a2*t + 400*a2*a*b*t - 600*a2*b2*t + 400*a*b2*b*t - 100*b2*b2*t + 90*sqrt(5.)*a2*a*t2 - 270*sqrt(5.)*a2*b*t2 + 270*sqrt(5.)*a*b2*t2 - 90*sqrt(5.)*b2*b*t2 - 210*a2*t3 + 420*a*b*t3 - 210*b2*t3 + 42*sqrt(5.)*a*t2*t2 - 42*sqrt(5.)*b*t2*t2))/(108.*t2*t2*t) + (exp((-2*sqrt(5.)*a)/t + (sqrt(5.)*(-2 + 3*a + b))/t)*(-150*t + 300*a*t - 150*a2*t + 300*b*t - 600*a*b*t + 300*a2*b*t - 150*b2*t + 300*a*b2*t - 150*a2*b2*t - 180*sqrt(5.)*t2 + 240*sqrt(5.)*a*t2 - 60*sqrt(5.)*a2*t2 + 300*sqrt(5.)*b*t2 - 360*sqrt(5.)*a*b*t2 + 60*sqrt(5.)*a2*b*t2 - 120*sqrt(5.)*b2*t2 + 120*sqrt(5.)*a*b2*t2 - 450*t3 + 330*a*t3 - 30*a2*t3 + 570*b*t3 - 270*a*b*t3 - 150*b2*t3 - 108*sqrt(5.)*t2*t2 + 33*sqrt(5.)*a*t2*t2 + 75*sqrt(5.)*b*t2*t2 - 54*t2*t2*t))/(108.*t3*t2) + (exp((-2*sqrt(5.)*a)/t + (sqrt(5.)*(a - b))/t)*(150*a2*b2*t + 60*sqrt(5.)*a2*b*t2 + 120*sqrt(5.)*a*b2*t2 + 30*a2*t3 + 270*a*b*t3 + 150*b2*t3 + 33*sqrt(5.)*a*t2*t2 + 75*sqrt(5.)*b*t2*t2 + 54*t2*t2*t))/(108.*t2*t2*t));
    }
  } else {
    throw std::invalid_argument("Covariance Type not Supported");
  }
}

//' gradient of Ikk with respect to t (univariate)
//' @param a,b design locations
//' @param t lengthscale parameter
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The the derivative of the scalar integrated derivative as a double.
//' @noRd
// [[Rcpp::export]]
double grad_Ikk_dt_cpp(double a, double b, double t, int ct){
  double t2 = t*t;
  if (ct == 1) {
    return(((sqrt(pi)*(2*t2 + (b-a)*(b-a)))*exp(-(b-a)*(b-a)/(4*t2))*(erf((b+a)/(2*t))-erf((b+a-2)/(2*t)))+2*((b+a-2)*exp(-(b*b+a*a-2*a-2*b+2)/(2*t2))-(b+a)*exp(-(b*b+a*a)/(2*t2)))*t)/(4*t*t));
  } else{
    throw std::invalid_argument("Covariance Type not Supported");
  }
}


//TODO: Not used anywhere, remove?
//' int_0_1 k(x, x') dx
//' @param a design location
//' @param t lengthscale parameter
//' @noRd
double IkG_cpp(double a, double t){
  return(sqrt(pi/2.) * t * (erf(a/(sqrt(2.) * t)) - erf((a - 1)/(sqrt(2.) * t))));
}

//TODO: Not used anywhere, remove?
//' int_0_1 dk(x,x')/dx dx
//' @param a design location
//' @param t lengthscale parameter
//' @noRd
double Idk_cpp(double a, double t){
  return(exp(-(a - 1)*(a - 1)/(2 * t * t)) - exp(-a*a/(2*t*t)));
} 


// //' E(kappa_i(X, design))
// //' @title Expectation of the kernel computation
// //' @param design matrix of design points
// //' @param theta lengthscales
// //' @param i1 index of the derivative (WARNING: starts at 0)
// //' @export
// // [[Rcpp::export]]
// NumericVector w_kappa_i(NumericVector w, NumericMatrix design, NumericVector theta, int i1,  int start){
//   int n = design.nrow();
//   int d = design.ncol();
//   
//   for(int j = start; j < n; j++){
//     w(j) = Idk_cpp(design(j, i1), theta(i1));
//     for(int k = 0; k < d; k++){
//       if(k != i1) w(j) *= IkG_cpp(design(j, k), theta(k));
//     }
//   }
//   return(w);
// }

//NumericVector w_kappa_i(NumericVector w, NumericMatrix design, NumericVector theta, 
//    int i1, int start){
//  int n = design.nrow();
//  int d = design.ncol();
//  
//  for(int j = start; j < n; j++){
//    w(j) = Idk_cpp(design(j, i1), theta(i1));
//      for(int k = 0; k < d; k++){
//        if(k != i1) w(j) *= IkG_cpp(design(j, k), theta(k));
//      }
//  }
//  return(w);
//}


//TODO: Rename
//' Computes Int(kappa_i(X, design) . kappa_j(design, X)). This function is preferred for initialization
//' @title Covariance of kernel computations
//' @param design matrix of design points
//' @param theta lengthscales
//' @param i1,i2 index of the derivatives (WARNING: starts at 0)
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The matrix representing the result of the integration.
//' @export
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix W_kappa_ij(NumericMatrix design, NumericVector theta, int i1, int i2, int ct){
  int n = design.nrow();
  int d = design.ncol();
  
  NumericMatrix W(n, n);
  
  if(i1 == i2){
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i = 0; i < n; i++){
      for(int j = i; j < n; j++){
        W(i, j) = w_ii_lebesgue(design(i, i1), design(j, i1), theta(i1), ct);
        for(int k = 0; k < d; k++){
          if(k != i1)
            W(i, j) *= Ikk_lebesgue(design(i, k), design(j, k), theta(k), ct);
        }
        W(j, i) = W(i, j);
      }
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i = 0; i < n; i++){
      for(int j = 0; j < n; j++){
        W(i, j) = w_ij_lebesgue(design(i, i1), design(j, i1), theta(i1), ct) * w_ij_lebesgue(design(j, i2), design(i, i2), theta(i2), ct);
        if(d > 2){
          for(int k = 0; k < d; k++){
            if(k != i1 && k != i2)
              W(i, j) *= Ikk_lebesgue(design(i, k), design(j, k), theta(k), ct);
          }
        }
      }
    }
  }
  
  return(W);
}


//' Creates a submatrix of the same tensor as W_kappa_ij, but this time l,k give the indices of the observations, not variables.
//' @title Covariance of kernel computations
//' @param design matrix of design points
//' @param theta lengthscales
//' @param i1,i2 index of the observations (WARNING: starts at 0)
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return The matrix representing the result of the integration.
//' @export
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix W_kappa_lk(NumericMatrix design, NumericVector theta, int i1, int i2, int ct){
  int d = design.ncol();

  NumericMatrix W(d, d);
  double val;

  for (int d1=0; d1<d; d1++) {
      for (int d2=d1; d2<d; d2++) {
          if (d1==d2) {
              val = w_ii_lebesgue(design(i1, d1), design(i2, d1), theta(d1), ct);
          } else {
              val = w_ij_lebesgue(design(i1, d1), design(i2, d1), theta(d1), ct) * w_ij_lebesgue(design(i1, d2), design(i2, d2), theta(d2), ct);
          }
          for (int d3=0; d3<d; d3++) {
              val *= Ikk_lebesgue(design(i1,d3), design(i2,d3), theta(d3), ct);
          }
          W(d1,d2) = val;
          W(d2,d1) = val;
      }
  }

  return(W);
}

//' Computes Int(kappa_i(X, design1) . kappa_j(design2, X)). This function is preferred for initialization
//' @title Covariance of kernel computations 
//' @param design1,design2 matrices of design points
//' @param theta lengthscales
//' @param i1,i2 index of the derivatives (WARNING: starts at 0)
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return matrix of size nrow(design1) x nrow(design2)
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix W_kappa_ij2(NumericMatrix design1, NumericMatrix design2, NumericVector theta, int i1, int i2, int ct){
  int n1 = design1.nrow();
  int n2 = design2.nrow();
  int d = design1.ncol();
  
  NumericMatrix W(n1, n2);
  
  if(i1 == i2){
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i = 0; i < n1; i++){
      for(int j = 0; j < n2; j++){
        W(i, j) = w_ii_lebesgue(design1(i, i1), design2(j, i1), theta(i1), ct);
        for(int k = 0; k < d; k++){
          if(k != i1)
            W(i, j) *= Ikk_lebesgue(design1(i, k), design2(j, k), theta(k), ct);
        }
      }
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i = 0; i < n1; i++){
      for(int j = 0; j < n2; j++){
        W(i, j) = w_ij_lebesgue(design1(i, i1), design2(j, i1), theta(i1), ct) * w_ij_lebesgue(design2(j, i2), design1(i, i2), theta(i2), ct);
        if(d > 2){
          for(int k = 0; k < d; k++){
            if(k != i1 && k != i2)
              W(i, j) *= Ikk_lebesgue(design1(i, k), design2(j, k), theta(k), ct);
          }
        }
      }
    }
  }
  
  return(W);
}

//' Computes gradient of Int(kappa_i(X, design1) . kappa_j(design2, X)) with respect to the first argument.
//' @title Covariance of kernel computations 
//' @param design1 A vector representing a new point.
//' @param design2 matrices of design points
//' @param theta lengthscales
//' @param i1,i2 index of the derivatives (WARNING: starts at 0)
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return matrix of size nrow(design1) x nrow(design2)
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix grad_W_kappa_ij2(NumericVector design1, NumericMatrix design2, NumericVector theta, int i1, int i2, int ct){
  int n2 = design2.nrow();
  int d = design2.ncol();
  
  NumericVector w(n2);
  NumericMatrix W(d, n2);
  
  if(i1 == i2){
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int j = 0; j < n2; j++){
      w(j) = w_ii_lebesgue(design1(i1), design2(j, i1), theta(i1), ct);
      //no need to do anything if w(j) is equal to zero (to avoid multiply then divide by 0)
      if(w(j) != 0){
        for(int k = 0; k < d; k++){
          if(k != i1)
            w(j) *= Ikk_lebesgue(design1(k), design2(j, k), theta(k), ct);
        }
        for (int p = 0; p < d; p++) {
          if (p == i1) {
            W(p,j) = w(j) * grad_w_ii_lebesguea(design1(i1), design2(j, i1), theta(i1), ct) / w_ii_lebesgue(design1(i1), design2(j, i1), theta(i1), ct);
          } else {
            W(p,j) = w(j) * grad_Ikk_lebesguea(design1(p), design2(j, p), theta(p), ct) / Ikk_lebesgue(design1(p), design2(j, p), theta(p), ct);
          }
        }
      }
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int j = 0; j < n2; j++){
      w(j) = w_ij_lebesgue(design1(i1), design2(j, i1), theta(i1), ct) * w_ij_lebesgue(design2(j, i2), design1(i2), theta(i2), ct);
      
      //no need to do anything if w(j) is equal to zero (to avoid multiply then divide by 0)
      if(w(j) != 0){
        if(d > 2){
          for(int k = 0; k < d; k++){
            if(k != i1 && k != i2)
              w(j) *= Ikk_lebesgue(design1(k), design2(j, k), theta(k), ct);
          }
        }
        for (int p = 0; p < d; p++) {
          if (p == i1) {
            W(p,j) = w(j) * grad_w_ij_lebesguea(design1(i1), design2(j, i1), theta(i1), ct) / w_ij_lebesgue(design1(i1), design2(j, i1), theta(i1), ct);
          } else if (p == i2) {
            W(p,j) = w(j) * grad_w_ij_lebesgueb(design2(j, i2), design1(i2), theta(i2), ct) / w_ij_lebesgue(design2(j, i2), design1(i2), theta(i2), ct);
          } else {
            W(p,j) = w(j) * grad_Ikk_lebesguea(design1(p), design2(j, p), theta(p), ct) / Ikk_lebesgue(design1(p), design2(j, p), theta(p), ct);
          }
        }
      }
    }
  }
  
  return(W);
}

//' Computes gradient of Int(kappa_i(X, design1) . kappa_j(design2, X)) with respect to the second argument.
//' @title Covariance of kernel computations 
//' @param design1 A vector representing a new point.
//' @param design2 matrices of design points
//' @param theta lengthscales
//' @param i1,i2 index of the derivatives (WARNING: starts at 0)
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return matrix of size nrow(design1) x nrow(design2)
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix grad_W_kappa_ij2_w2(NumericVector design1, NumericMatrix design2, NumericVector theta, int i1, int i2, int ct){
  int n2 = design2.nrow();
  int d = design2.ncol();
  
  NumericVector w(n2);
  NumericMatrix W(d, n2);
  
  if(i1 == i2){
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int j = 0; j < n2; j++){
      w(j) = w_ii_lebesgue(design2(j, i1), design1(i1), theta(i1), ct);
      
      //no need to do anything if w(j) is equal to zero (to avoid multiply then divide by 0)
      if(w(j) != 0){
        for(int k = 0; k < d; k++){
          if(k != i1)
            w(j) *= Ikk_lebesgue(design2(j, k), design1(k), theta(k), ct);
        }
        for (int p = 0; p < d; p++) {
          if (p == i1) {
            W(p,j) = w(j) * grad_w_ii_lebesgueb(design2(j, i1), design1(i1), theta(i1), ct) / w_ii_lebesgue(design2(j, i1), design1(i1), theta(i1), ct);
          } else {
            W(p,j) = w(j) * grad_Ikk_lebesgueb(design2(j, p), design1(p), theta(p), ct) / Ikk_lebesgue(design2(j, p), design1(p), theta(p), ct);
          }
        }
      }
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int j = 0; j < n2; j++){
      w(j) = w_ij_lebesgue(design2(j, i1), design1(i1), theta(i1), ct) * w_ij_lebesgue(design1(i2), design2(j, i2), theta(i2), ct);
      
      //no need to do anything if w(j) is equal to zero (to avoid multiply then divide by 0)
      if(w(j) != 0){
        if(d > 2){
          for(int k = 0; k < d; k++){
            if(k != i1 && k != i2)
              w(j) *= Ikk_lebesgue(design2(j, k), design1(k), theta(k), ct);
          }
        }
        for (int p = 0; p < d; p++) {
          if (p == i1) {
            W(p,j) = w(j) * grad_w_ij_lebesgueb(design2(j, p), design1(p), theta(p), ct) / w_ij_lebesgue(design2(j, p), design1(p), theta(p), ct);
          } else if (p == i2) {
            W(p,j) = w(j) * grad_w_ij_lebesguea(design1(p), design2(j, p), theta(p), ct) / w_ij_lebesgue(design1(p), design2(j, p), theta(p), ct);
          } else {
            W(p,j) = w(j) * grad_Ikk_lebesgueb(design2(j, p), design1(p), theta(p), ct) / Ikk_lebesgue(design2(j, p), design1(p), theta(p), ct);
            //double h = 1e-5;
            //double fd = (Ikk_lebesgue(design2(j, p), design1(p) + h, theta(p), ct) - Ikk_lebesgue(design2(j, p), design1(p), theta(p), ct)) / h; 
            //W(p,j) = w(j) * fd / Ikk_lebesgue(design2(j, p), design1(p), theta(p), ct);
          }
        }
      }
    }
  }
  
  return(W);
}


//' Computes Int(kappa_i(X, design) . kappa_j(design, X)). This function is preferred for updates
//' @title Covariance of kernel computations
//' @param W The matrix to store the computation in
//' @param design matrix of design points
//' @param theta lengthscales
//' @param i1,i2 index of the derivatives (WARNING: starts at 0)
//' @param start The column/row index at which to start the computation (doesn't touch the start by start submatrix).
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @return W is modified in-place.
//' @keywords internal
//' @export
// [[Rcpp::export]]
void W_kappa_ij_up(NumericMatrix W, NumericMatrix design, NumericVector theta, 
                   int i1, int i2, int start, int ct){
  int n = design.nrow();
  int d = design.ncol();
  
  if(i1 == i2){
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i = 0; i < n; i++){
      for(int j = i; j < n; j++){
        if (i >= start || j >= start) {
          W(i, j) = w_ii_lebesgue(design(i, i1), design(j, i1), theta(i1), ct);
          for(int k = 0; k < d; k++){
            if(k != i1) 
              W(i, j) *= Ikk_lebesgue(design(i, k), design(j, k), theta(k), ct);
          }
          W(j, i) = W(i, j);
        }
      }
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i = 0; i < n; i++){
      for(int j = 0; j < n; j++){
        if (i >= start || j >= start){
          W(i, j) = w_ij_lebesgue(design(i, i1), design(j, i1), theta(i1), ct) * w_ij_lebesgue(design(j, i2), design(i, i2), theta(i2), ct);
          if(d > 2){
            for(int k = 0; k < d; k++){
              if(k != i1 && k != i2)
                W(i, j) *= Ikk_lebesgue(design(i, k), design(j, k), theta(k), ct); 
            }
          }
        }
      }
    }
  }
}


// Create duplicates of Lebesgue functions to share the Gaussian signature.
double ol_Ikk_lebesgue(double a, double b, double t, int ct, double xm, double xv) {return(Ikk_lebesgue(a, b, t, ct));}
double ol_w_ii_lebesgue(double a, double b, double t, int ct, double xm, double xv) {return(w_ii_lebesgue(a, b, t, ct));}
double ol_w_ij_lebesgue(double a, double b, double t, int ct, double xm, double xv) {return(w_ij_lebesgue(a, b, t, ct));}

//' Computes Int(kappa_i(X, design) . kappa_j(design, X)). This function is preferred for initialization
//' @title Covariance of kernel computations
//' @param measure An integer giving the measure of integration. 0 for Lebesgue/Uniform on [0,1]^m, 1 for (truncated) Gaussian on [0,1]^m, 2 for Gaussian on R^m.
//' @param design matrix of design points
//' @param Ki The inverse covariance matrix
//' @param Kir The inverse covariance matrix times the response.
//' @param theta lengthscales
//' @param ct Covariance type, 1 means Gaussian, 2 means Matern 3/2, 3 means Matern 5/2
//' @param xm The mean vector associated with the Gaussian measure. Ignored if uniform. 
//' @param xv The variance vector associated with the Gaussian measure (diagonal of covariance matrix, vars assumed independent). Ignored if uniform. 
//' @return The matrix representing the result of the integration.
//' @export
//' @keywords internal
// [[Rcpp::export]]
List quick_C(int measure, NumericMatrix design, NumericMatrix Ki, NumericVector Kir, NumericVector theta, NumericVector xm, NumericVector xv, int ct, bool verbose){
  std::function<double(double, double, double, int, double, double)> Ikk_func;
  std::function<double(double, double, double, int, double, double)> w_ii_func;
  std::function<double(double, double, double, int, double, double)> w_ij_func;

  switch (measure) { // Lebesgue
    case 0:
      {
      Ikk_func = ol_Ikk_lebesgue;
      w_ii_func = ol_w_ii_lebesgue;
      w_ij_func = ol_w_ij_lebesgue;
      break;
      }
    case 1:
      {
      Ikk_func = Ikk_gauss;
      w_ii_func = w_ii_gauss;
      w_ij_func = w_ij_gauss;
      break;
      }
    case 2:
      {
      Ikk_func = Ikk_tgauss;
      w_ii_func = w_ii_tgauss;
      w_ij_func = w_ij_tgauss;
      break;
      }
  } 
  int n = design.nrow();
  int d = design.ncol();

  NumericMatrix C(d, d);
  NumericMatrix Wij;
  List sl;
  List W(d);
  std::vector<std::vector<NumericMatrix>> Wvec(d);
  std::vector<NumericMatrix> sv;
  for (int d1=0; d1<d; d1++) {
      sl = List(d);
      W(d1) = sl;
      sv = std::vector<NumericMatrix>(d);
      for (int d2=0; d2<d; d2++) {
          Wij = NumericMatrix(n,n);
          sv[d2] = Wij;
      }
      Wvec[d1] = sv;
  }

  double coef;
  //double w;
  double logw;
  double wnew;
  double wijlk;
  // double wold;
  double ires;
  //NumericVector wvec(d);
  NumericVector logwvec(d);

  Progress pb(n, verbose);

  // Compute main terms.
  for (int n1 = 0; n1 < n; n1++) {
    if (!pb.increment()) { // Update progress bar and check for interupts.
        break;
    }
    //sl = List(n);
    for (int n2 = 0; n2 < n; n2++) {
      //Wij = NumericMatrix(d,d);
      coef = (Kir(n1) * Kir(n2) - Ki(n1,n2));
      //w = 1.0;
      //for (int d0 = 0; d0 < d; d0++) {
      //  ires = Ikk_func(design(n1, d0), design(n2, d0), theta(d0), ct, xm(d0), xv(d0));
      //  w *= ires;
      //  wvec(d0) = ires;
      //}

      logw = 0.;
      for (int d0 = 0; d0 < d; d0++) {
        ires = log(Ikk_func(design(n1, d0), design(n2, d0), theta(d0), ct, xm(d0), xv(d0)));
        logw += ires;
        logwvec(d0) = ires;
      }

      for (int d1 = 0; d1 < d; d1++) {
        for (int d2 = d1; d2 < d; d2++) {
          if (d1 == d2) {
            wnew = w_ii_func(design(n1, d1), design(n2, d1), theta(d1), ct, xm(d1), xv(d1));
            //C(d1,d2) += coef * w * wnew / wvec[d1];
            //C(d1,d2) += coef * exp(logw + log(wnew) - logwvec[d1]);
            wijlk = wnew * exp(logw - logwvec[d1]);
            C(d1,d2) += coef * wijlk;
          } else {
            wnew = w_ij_func(design(n1, d1), design(n2, d1), theta(d1), ct, xm(d1), xv(d1)) * w_ij_func(design(n2, d2), design(n1, d2), theta(d2), ct, xm(d2), xv(d2));
            //C(d2, d1) = C(d1,d2) += coef * w * wnew / (wvec[d1] * wvec[d2]);
            //C(d2, d1) = C(d1,d2) += coef * exp(logw + log(wnew) - (logwvec[d1] + logwvec[d2]));
            wijlk = wnew * exp(logw - (logwvec[d1] + logwvec[d2]));
            C(d2, d1) = C(d1,d2) += coef * wijlk;
          }
          // Store Wijs in case we want to update or do sequential design.
          sv = Wvec[d1];
          Wij = sv[d2];
          Wij(n1,n2) = wijlk;
          sv = Wvec[d2];
          Wij = sv[d1];
          Wij(n1,n2) = wijlk;
        }
      }
    }
  }

  // Repackage Wvec as a list W
  for (int d1=0; d1<d; d1++) {
      sl = W(d1);
      for (int d2=0; d2<d; d2++) {
          sl(d2) = Wvec[d1][d2];
      }
  }

  double M_num = -1;
  switch (ct) {
      case 1:
          M_num = 1.;
          break;
      case 2:
          M_num = 3.;
          break;
      case 3:
          M_num = 1.666667;
          break;
  }

  // Augment diag
  for (int d0 = 0; d0 < d; d0++) {
    C(d0,d0) += M_num / pow(theta(d0), 2);
  }

  List ret = List::create(Named("C") = C , Named("W") = W);

  return(ret);
}

//TODO: Rename w_ii etc
//' Derivative of the W matrix wrt theta
//' @param design matrix of design points, one per row, in [0,1]^d
//' @param theta Gaussian kernel hyperparameters (TODO: define parameterization)
//' @param W W matrix matrix
//' @param i1,i2 index of C[i1,i2]
//' @param it :index of theta for partial derivative
//' @param ct : covariance type (only 1 is supported for now)
//' @noRd
// [[Rcpp::export]]
NumericMatrix grad_W_t(NumericMatrix design, NumericVector theta, NumericMatrix W, int i1, int i2, int it, int ct){
  int n = design.nrow();
  double tmp;
  NumericMatrix W_copy(clone(W)); // to avoid modifying the W matrix outside
  
  if(it == i1){
    if(i1 == i2){
      for(int i = 0; i < n; i++){
        for(int j = i; j < n; j++){
          if(W_copy(i,j) != 0){
            tmp = W_copy(i,j) * grad_w_ii_dt_cpp(design(i, i1), design(j, i1), theta(i1), ct) / w_ii_lebesgue(design(i, i1), design(j, i1), theta(i1), ct);
            W_copy(i,j) = tmp;
            W_copy(j,i) = tmp;
          }
        }
      }
    }else{
      for(int i = 0; i < n; i++){
        for(int j = 0; j < n; j++){
          if(W_copy(i,j) != 0)
            W_copy(i,j) = W_copy(i,j) * grad_w_ij_dt_cpp(design(i, i1), design(j, i1), theta(i1), ct) / w_ij_lebesgue(design(i, i1), design(j, i1), theta(i1), ct);
        }
      }
    }
  }else{
    if(it == i2){
      for(int i = 0; i < n; i++){
        for(int j = 0; j < n; j++){
          if(W_copy(i,j) != 0)
            W_copy(i,j) = W_copy(i,j) * grad_w_ij_dt_cpp(design(j, i2), design(i, i2), theta(i2), ct) / w_ij_lebesgue(design(j, i2), design(i, i2), theta(i2), ct);
        }
      }
    }else{
      if(i1 == i2){
        for(int i = 0; i < n; i++){
          for(int j = i; j < n; j++){
            if(W_copy(i,j) != 0){
             tmp =  W_copy(i,j) * grad_Ikk_dt_cpp(design(i, it), design(j, it), theta(it), ct) / Ikk_lebesgue(design(i, it), design(j, it), theta(it), ct);
              W_copy(i,j) = tmp;
              W_copy(j,i) = tmp;
            }
          }
        }
      }else{
        // Only the symmetric is different from the previous case
        for(int i = 0; i < n; i++){
          for(int j = 0; j < n; j++){
            if(W_copy(i,j) != 0)
              W_copy(i,j) = W_copy(i,j) * grad_Ikk_dt_cpp(design(i, it), design(j, it), theta(it), ct) / Ikk_lebesgue(design(i, it), design(j, it), theta(it), ct);
          }
        }
      }
    }
  }
  return(W_copy);
}


