
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

// The target distribution
double f_fun_c(double x, double a_val, double b_val, double c_val) {
  double val = a_val*log(x) - b_val*x - c_val*sqrt(x);
  return val;
}

// It's graadient
double g_fun_c(double x, double a_val, double b_val, double c_val) {
  double val = a_val/x - b_val - 0.5*c_val/sqrt(x);
  return val;
}

// It's Hessian
double h_fun_c(double x, double a_val, double b_val, double c_val) {
  double val = -a_val/(x*x)  + 0.25*c_val/(sqrt(x)*x);
  return val;
}


double slice_sampler_precision_c(double x, double a_val, double b_val, double c_val) 
{
  double TOL = 1.0E-8;
  double MAXITER = 100;
  
  // Calculate the mode
  double z_star = (-0.5*c_val + sqrt(0.25*c_val*c_val + 4*a_val*b_val))/(2*b_val);
  double x_star = z_star*z_star;
  
  // Target at the mode
  double f_star = f_fun_c(x_star, a_val, b_val, c_val);
  double p_star = exp(f_star);
    
  // Calculate approximate variaiance and sd
  double h_star = h_fun_c(x_star, a_val, b_val, c_val); 
  double sigma2 = -1/h_star;
  double sigma  = sqrt(sigma2);
      
  // Calculate target at initial x
  double x0 = x;
  double f0 = f_fun_c(x0, a_val, b_val, c_val) - f_star;
  double p0 = exp(f0);
        
  if (p0==0.0) {
    // Something has gone horribly wrong because the 
    // initial x is far outside the effective domain
    x0 = x_star;
    f0 = f_fun_c(x0, a_val, b_val, c_val) - f_star;
    p0 = exp(f0);
  }
        
  // Sample uniform vertically
  double u = R::runif(0,p0);
  double log_u = log(u);
          
  // Initial guesses of the left and right end-points based on Laplace approximation
  double xL = x_star - sqrt(2*sigma2*( - log_u));
  double xR = x_star + sqrt(2*sigma2*( - log_u));

  // Set left end-point initial guess to be small if Laplace approximation 
  // gives negative end-point
  if (xL<0) {
    xL = 1.0E-12;
  }
  
  double fL;
  double gL;
  double fR;
  double gR;
  
  // Use Newton's method to find left end point
  for (int i=0; i<MAXITER; ++i) {
    fL = f_fun_c(xL, a_val, b_val, c_val) - f_star;
    gL = g_fun_c(xL, a_val, b_val, c_val);
    xL = xL - (fL - log_u)/gL;
    if (abs(gL)<TOL) { break; }
  }
            
  // Use Newton's method to find right end-point
  for (int i=0; i<MAXITER; ++i) {
    fR = f_fun_c(xR, a_val, b_val, c_val) - f_star;
    gR = g_fun_c(xR, a_val, b_val, c_val);
    xR = xR - (fR - log_u)/gR;
    if (abs(gR)<TOL) { break; }
  }
            
  // Sample uniformly between left and right end-points
  double x_new = R::runif(xL, xR);
              
  return x_new;
}

 
