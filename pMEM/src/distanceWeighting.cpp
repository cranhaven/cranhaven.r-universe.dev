/***************************************************************************\
 *
 * (c) 2023-2024 Guillaume Guénard
 *     Department de sciences biologiques,
 *     Université de Montréal
 *     Montreal, QC, Canada
 *
 * Calculations of predictive Moran's Eigenvector Maps (pMEM), which extends
 * classical MEM for making predictions between the sampling points. It also
 * implements 
 *
 \**************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

#include <complex>
using namespace std;

NumericVector dwfReal(const NumericVector& x, const int method,
                      const NumericVector& par) {
  
  int i, n = x.size();
  double tmp, aa, bb = 1.0/par[0], cc, dd;
  NumericVector y = clone(x);
  
  switch(method) {
  case 1:
    for(i = 0; i < n; i++)
      y[i] = (x[i] < par[0]) ? 1.0 - x[i]*bb : 0.0;
    break;
  case 2:
    if(par.size() != 2)
      stop("The power DWF requires a shape parameter!");
    for(i = 0; i < n; i++)
      y[i] = (x[i] < par[0]) ? 1.0 - pow(x[i]*bb,par[1]) : 0.0;
    break;
  case 3:
    if(par.size() != 2)
      stop("The hyperbolic DWF requires a shape parameter!");
    aa = -par[1];
    cc = -pow(2.0,aa);
    dd = 1.0/(1.0 + cc);
    for(i = 0; i < n; i++)
      y[i] = (x[i] < par[0]) ? (pow(x[i]*bb + 1.0,aa) + cc)*dd : 0.0;
    break;
  case 4:
    for(i = 0; i < n; i++)
      if(x[i] < par[0]) {
        tmp = x[i]*bb;
        y[i] = 1.0 - 1.5*tmp + 0.5*tmp*tmp*tmp;
      }
      else
        y[i] = 0.0;
    break;
  case 5:
    for(i = 0; i < n; i++)
      y[i] = exp(-x[i]*bb);
    break;
  case 6:
    for(i = 0; i < n; i++) {
      tmp = x[i]*bb;
      tmp *= tmp;
      y[i] = exp(-tmp);
    }
    break;
  case 7:
    for(i = 0; i < n; i++)
      if(x[i] != 0.0) {
        tmp = M_PI*x[i]*bb;
        y[i] = sin(tmp)/tmp;
      }
      else
        y[i] = 1.0;
    break;
  default:
    stop("Unknown weighting method!");
  }
  
  return(y);
}

complex<double> fromRcomplex(const Rcomplex & x) {
  complex<double> y;
  y.real(x.r);
  y.imag(x.i);
  return(y);
}

Rcomplex toRcomplex(const complex<double>& x) {
  Rcomplex y;
  y.r = x.real();
  y.i = x.imag();
  return(y);
}

ComplexVector dwfCplx(const ComplexVector& x, const int method,
                      const NumericVector& par) {
  
  int i, n = x.size();
  double aa, bb = 1.0/par[0], cc, dd;
  complex<double> tmp;
  ComplexVector y = clone(x);
  
  switch(method) {
  case 1:
    for(i = 0; i < n; i++) {
      tmp = fromRcomplex(x[i]);
      tmp = (abs(tmp) < par[0]) ? 1.0 - tmp*bb : 0.0;
      y[i] = toRcomplex(tmp);
    }
    break;
  case 2:
    if(par.size() < 2)
      stop("Method 2 requires a shape parameter!");
    for(i = 0; i < n; i++) {
      tmp = fromRcomplex(x[i]);
      tmp = (abs(tmp) < par[0]) ? 1.0 - pow(tmp*bb,par[1]) : 0.0;
      y[i] = toRcomplex(tmp);
    }
    break;
  case 3:
    if(par.size() < 2)
      stop("The hyperbolic DWF requires a shape parameter!");
    aa = -par[1];
    cc = -pow(2.0,aa);
    dd = 1.0/(1.0 + cc);
    for(i = 0; i < n; i++) {
      tmp = fromRcomplex(x[i]);
      tmp = (abs(tmp) < par[0]) ? (pow(tmp*bb + 1.0,aa) + cc)*dd : 0.0;
      y[i] = toRcomplex(tmp);
    }
    break;
  case 4:
    for(i = 0; i < n; i++) {
      tmp = fromRcomplex(x[i]);
      if(abs(tmp) < par[0]) {
        tmp *= bb;
        tmp = 1.0 - 1.5*tmp + 0.5*tmp*tmp*tmp;
      } else
        tmp = 0.0;
      y[i] = toRcomplex(tmp);
    }
    break;
  case 5:
    for(i = 0; i < n; i++) {
      tmp = fromRcomplex(x[i]);
      tmp *= bb;
      tmp = exp(-tmp);
      y[i] = toRcomplex(tmp);
    }
    break;
  case 6:
    for(i = 0; i < n; i++) {
      tmp = fromRcomplex(x[i]);
      tmp *= bb;
      tmp *= tmp;
      tmp = exp(-tmp);
      y[i] = toRcomplex(tmp);
    }
    break;
  case 7:
    for(i = 0; i < n; i++) {
      tmp = fromRcomplex(x[i]);
      if(abs(tmp) != 0.0) {
        tmp = M_PI*tmp*bb;
        tmp = sin(tmp)/tmp;
      } else
        tmp = 1.0;
      y[i] = toRcomplex(tmp);
    }
    break;
  default:
    stop("Unknown weighting method!");
  }
  
  return(y);
}
