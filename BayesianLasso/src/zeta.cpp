
#define ARMA_DONT_USE_WRAPPER
#include <RcppArmadillo.h>
#include <RcppNumerical.h>

// [[Rcpp::depends(RcppArmadillo)]] 
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

using namespace Rcpp;
using namespace arma;
using namespace Eigen;
using namespace Numer;
using namespace std;

typedef Map<MatrixXd> MapMat;
typedef Map<VectorXd> MapVec;

////////////////////////////////////////////////////////////////////////////////

#include <R.h>
#include <Rmath.h>

/* Global Variables */
#define TINY1   1.0E-30
#define TINY2   1.0E-7

///////////////////////////////////////////////////////////////////////////////

#include <Rmath.h>

/* Global Variables */
#define TINY   1.0E-30


double  zetaOneLentz_c(double x, double tol, int maxiter)
{
  int j;
  double fcurr = 0.0;
  double fprev = 0.0;
  double Cprev = 0.0;
  double Ccurr = 0.0;
  double Dprev = 0.0;
  double Dcurr = 0.0;
  double Delta = 0.0;
  double a = 0.0;
  double b = 1.0;
  double x2 = x*x;
  double x2inv = 1/x2;
  
  fprev = b;
  Cprev = fprev;
  
  for (j=0;j<maxiter;j++)
  {
    a = (1.0 + (double)j)*x2inv;
    Dcurr = b + a*Dprev;
    if (Dcurr==0.0) { 
      Dcurr = TINY;
    }
    Ccurr = b + a/Cprev;
    if (Ccurr==0.0) {
      Ccurr = TINY;
    }
    Dcurr = 1/Dcurr;
    Delta = Ccurr*Dcurr;
    fcurr = fprev*Delta;
    if (fabs(Delta-1.0)<tol) {
      break;
    }
    fprev = fcurr;
    Cprev = Ccurr;
    Dprev = Dcurr;
  }
  fcurr = -x*fcurr;
  return fcurr;
}

///////////////////////////////////////////////////////////////////////////////


double zetaOne_c(double x) 
{
  double f;
  double logdn;
  double logpn;
  double tol = 1.0E-12;
  int maxiter = 1000;    
  double mhalflog2pi = -0.918938533204673;
  
  if (x < (-7.0)) {
    f = zetaOneLentz_c(x, tol, maxiter);
  } else {
    logdn = mhalflog2pi - 0.5*x*x;
    logpn = R::pnorm5(x, 0.0, 1.0, 1, 1);
    f = exp(logdn - logpn);
  }
  return f;
}

////////////////////////////////////////////////////////////////////////////////


double zeta_c(int k, double x) 
{
  double z1 = zetaOne_c(x); 
  double res = z1;
  if (k==2) {
    res = -x*z1 - z1*z1;
  }
  return res;
}
