
#define ARMA_DONT_USE_WRAPPER
#include <RcppArmadillo.h>
#include <RcppNumerical.h>

//[[Rcpp::depends(RcppClock)]]
#include <RcppClock.h>

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
  


arma::vec rinvgauss_c(arma::vec vmu, arma::vec vlambda) 
{
  int n = vmu.n_rows;
  arma::vec vphi = 1/vlambda;
  arma::vec vr = zeros(n);  
  uvec cond1 = find((vmu> 0) && (vphi> 0));
  uvec cond2 = find((vmu<=0) || (vphi<=0));
  
  // Take care of samples with bad inputs
  if (cond2.n_elem>0) {
    vr.elem(cond2).fill(datum::nan);
    n = cond1.n_elem;
  }
  
  // For samples with good arguments calculate vy and vx
  vphi.elem(cond1) = vphi.elem(cond1) % vmu.elem(cond1);
  arma::vec vy = chi2rnd( 1.0, n );
  arma::vec vx = 1.0 + vphi.elem(cond1)/2.0 % (vy - sqrt(4*vy/vphi.elem(cond1) + vy%vy));

/** 
  * Note: The line above should yield all vx>0, but it occasionally doesn't due to
   * numerical precision issues. The line below detects this and recomputes
   * the relevant elements of vx using a 2nd-order Taylor expansion of the
   * sqrt function, which is a good approximation whenever the problem occurs.
   **/
  
  uvec cond3 = find(vx<=0);
  if (cond3.n_elem>0) {
    vx.elem(cond3) = (1/(vy.elem(cond3) % vphi.elem(cond3)));
  }
  
  arma::vec vu = randu(n);
  uvec cond4 = find(vu< (1.0/(1+vx)));
  uvec cond5 = find(vu>=(1.0/(1+vx)));
  
  arma::vec temp = vr.elem(cond1);
  temp.elem(cond4) = vx.elem(cond4);
  temp.elem(cond5) = 1/vx.elem(cond5);
  vr.elem(cond1) = temp;
  
  return (vmu%vr);
}

/**
Draw from inverse-Gaussian distribution while avoiding potential numerical problems
**/


arma::vec rinvgaussian_c(arma::vec vmu, arma::vec vlambda) {
  arma::vec vm = vmu / sqrt(vmu % vlambda);
  arma::vec vl = vlambda / sqrt(vmu % vlambda);
  arma::vec result = sqrt(vmu % vlambda) % rinvgauss_c(vm, vl);
  return result;
}

////////////////////////////////////////////////////////////////////////////////

// The target distribution
double f_fun_invgauss_c(double x, double a_val, double b_val, double c_val) {
  double val = -a_val*log(x) - b_val*x - c_val/x;
  return val;
}

// It's gradient
double g_fun_invgauss_c(double x, double a_val, double b_val, double c_val) {
  double val = -a_val/x - b_val + c_val/(x*x);
  return val;
}

// It's Hessian
double h_fun_invgauss_c(double x, double a_val, double b_val, double c_val) {
  double val = a_val/(x*x)  - 2.0*c_val/(x*x*x);
  return val;
}


double rinvgaussian_slice(double x, double a_val, double b_val, double c_val) 
{
  double TOL = 1.0E-8;
  double MAXITER = 1000;
  
  // Calculate the mode
  double x_star = (- a_val + sqrt(a_val*a_val + 4.0*b_val*c_val))/(2.0*b_val);
  
  // Target at the mode
  double f_star = f_fun_invgauss_c(x_star, a_val, b_val, c_val);
  
  // Calculate approximate variance and sd
  double h_star = h_fun_invgauss_c(x_star, a_val, b_val, c_val); 
  double sigma2 = -1/h_star;
  double sigma  = sqrt(sigma2);
  
  // Calculate target at initial x
  double x0 = x;
  double f0 = f_fun_invgauss_c(x0, a_val, b_val, c_val) - f_star;
  double p0 = exp(f0);
  
  //Rcout << "x_star= " << x_star << "\n";
  //Rcout << "f_star= " << f_star << "\n";
  //Rcout << "h_star= " << h_star << "\n";
  //Rcout << "sigma2= " << sigma2 << "\n";
  //Rcout << "sigma= " << sigma << "\n";
  //Rcout << "x0= " << x0 << "\n";
  //Rcout << "f0= " << f0 << "\n";
  //Rcout << "p0= " << p0 << "\n";
  
  if (p0==0.0) {
    // Something has gone horribly wrong because the 
    // initial x is far outside the effective domain
    x0 = x_star;
    f0 = f_fun_invgauss_c(x0, a_val, b_val, c_val) - f_star;
    p0 = exp(f0);
  }
  
  // Sample uniform vertically
  double u = R::runif(0,p0);
  double log_u = log(u);
  
  // Initial guesses of the left and right end-points based on Laplace approximation
  double xL = x_star - sqrt(2.0*sigma2*( -log_u));
  double xR = x_star + sqrt(2.0*sigma2*( -log_u));
  
  // Set left end-point initial guess to be small if Laplace approximation 
  // gives negative end-point
  if (xL<1.0E-6) {
    xL = 1.0E-6;
  }
  
  double fL;
  double gL;
  double fR;
  double gR;
  
  // Use Newton's method to find left end point
  for (int i=0; i<MAXITER; ++i) {
    fL = f_fun_invgauss_c(xL, a_val, b_val, c_val) - f_star;
    gL = g_fun_invgauss_c(xL, a_val, b_val, c_val);
    xL = xL - (fL - log_u)/gL;
    if (abs(gL)<TOL) { break; }
  }
  
  // Use Newton's method to find right end-point
  for (int i=0; i<MAXITER; ++i) {
    fR = f_fun_invgauss_c(xR, a_val, b_val, c_val) - f_star;
    gR = g_fun_invgauss_c(xR, a_val, b_val, c_val);
    xR = xR - (fR - log_u)/gR;
    if (abs(gR)<TOL) { break; }
  }
  
  //if (xL<1.0E-6) {
  //  xL = 1.0E-6;
  //}
  
  //if (xR<2.0E-6) {
  //  xR = 2.0E-6;
  //}
  
  // Sample uniformly between left and right end-points
  double x_new = R::runif(xL, xR);
  
  return x_new;
}


arma::vec rinvgaussian_slice_c(arma::vec vx, arma::vec vmu, arma::vec vlambda) 
{
  int n = vmu.n_rows;
  arma::vec result(n);
  arma::vec vb_val = vlambda/(2.0*vmu%vmu);
  arma::vec vc_val = 0.5*vlambda;
  for (int i=0; i<n; ++i) {
    
    //Rcout << "vx(i)= " << vx(i) << "\n";
    //Rcout << "a_val= " << 1.5 << "\n";
    //Rcout << "b_val= " << vb_val(i) << "\n";
    //Rcout << "c_val= " << vc_val(i) << "\n";
    
    double val = rinvgaussian_slice(vx(i), 1.5, vb_val(i), vc_val(i));
    result(i) = val;
  }
  return result;
}



