
#define ARMA_DONT_USE_WRAPPER
#include <RcppArmadillo.h>
#include <RcppNumerical.h>
#include "rinvgaussian.h"
#include "slice_sampler.h"
#include "lasso_distribution.h"

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
// typedef arma::mat mat;


// [[Rcpp::export]]
List Modified_Hans_Gibbs(arma::mat X, arma::vec y, double a1, double b1, double u1, double v1,  
                     int nsamples, arma::vec beta_init, double lambda_init, double sigma2_init,
                     int verbose)
{
  int n = X.n_rows;
  int p = X.n_cols;
  
  const int maxiter = nsamples;
  
  // Initialise storage of samples for MCMC
  arma::mat mBeta(maxiter,p);
  arma::vec vsigma2(maxiter);
  arma::vec vlambda2(maxiter);
  
  // Initialize storage for Rao-Blackwellization
  arma::mat mA(maxiter,p);
  arma::mat mB(maxiter,p);
  arma::mat mC(maxiter,p);
  
  const vec one_n = ones(n);
  const vec one_p = ones(p);
  arma::mat XTX;
  arma::vec dgXTX  = (X%X).t() * one_n;
  if (n>p) {
    XTX = X.t() * X;
  } 
  arma::mat  XTX1 = X.t() * X;
  arma::vec XTy = X.t() * y;
  double yTy = sum(y%y);
  //arma::vec yTX = XTy.t();
  
  //Rcout << "A: \n";
  
  // Set the current values of the parameters 
  arma::vec vb = ones(p); // expected value of auxiliary variables under q
  arma::vec vnu = ones(p);
  const double u_til = u1 + 0.5*p;
  
  // Assign initial values
  arma::vec vbeta = beta_init;
  double sigma2 = sigma2_init;
  double sigma = sqrt(sigma2);
  double lambda = lambda_init;
  double lambda2 = lambda_init*lambda_init;
  
  // Constant values 
  const double a_til = a1 + 0.5*(n + p);
  double b_til = b1;
  
  
 
  
  for (int i = 0; i < maxiter; ++i) 
  {  // Rcout <<  i << "A"<<  "\n";
    // Sample from vb|rest
    vnu = sigma/(lambda*abs(vbeta));
    
    vb  = rinvgaussian_c(vnu, one_p);  
   
    double num;
    double denom;
    
    arma::vec va_vals = dgXTX/sigma2;
    arma::vec vb_vals = zeros(p);
    arma::vec vc_vals = one_p*sqrt(lambda2/sigma2);
    
    arma::vec vu = randu(p);
    if (n>p) {
      arma::vec XTy_hat = XTX * vbeta;
      for (int j=0; j<p; ++j) {
        arma::vec single_val_vec(1);
        single_val_vec[0] = vu[j];
        
        arma::vec vx_j = XTX.col(j);
        XTy_hat = XTy_hat - vx_j*vbeta[j]; // This might not be exactly right
        num = XTy[j] - XTy_hat[j];
        vb_vals[j] = num/sigma2;
        denom = dgXTX[j] + lambda2*vb[j];
        vbeta[j] =  qlasso_internal(single_val_vec, va_vals[j], vb_vals[j], vc_vals[j])[0];
        XTy_hat = XTy_hat + vx_j*vbeta[j];
      }
    } else {
      arma::vec vy_hat = X * vbeta;
      for (int j=0; j<p; ++j) {
        arma::vec single_val_vec(1);
        single_val_vec[0] = vu[j];
        
        arma::vec vx_j = X.col(j);
        arma::vec vy_hat_mj = vy_hat - vx_j*vbeta[j];
        num = XTy[j] -  as_scalar(vx_j.t() * vy_hat_mj);
        vb_vals[j] = num/sigma2;
        vbeta[j] = qlasso_internal(single_val_vec, va_vals[j], vb_vals[j], vc_vals[j])[0];
        // if(i==7){
        //  Rcout << j << vbeta[j] << "B"<<  "\n";
        // }
        vy_hat = vy_hat_mj +  vx_j*vbeta[j];
      }
    }
    
    ////////////////////////////////////////////////////////////////////////////
    
    // Slice from lambda2|rest
    double sum_abs_vbeta = sum(abs(vbeta));
    //Rcout << "iter: " << i << "\n";
    arma::vec XTX_b = XTX1 * vbeta;

    //Rcout << "iter: " << i << "\n";
    double bT_XTX_b = sum(vbeta%XTX_b);
    double bT_XTy = sum(vbeta%XTy);
    double RSS  =  yTy - 2*bT_XTy + bT_XTX_b;//sum(pow(vy-mX*vbeta,2.0));

    
    // Slice sample from sigma2|rest
    lambda = sqrt(lambda2);
    
    double a_val = (a_til-1);
    double b_val = b1 + 0.5*RSS;
    double c_val = lambda*sum_abs_vbeta;
    // Slice sampler for tau and then invert.
    double tau = 1/sigma2;
    tau = slice_sampler_precision_c(tau, a_val, b_val, c_val);
    sigma2 = 1/tau;
    sigma = sqrt(sigma2);
    
    ////////////////////////////////////////////////////////////////////////////
    
    a_val = u_til - 1;
    b_val = v1;
    c_val = sum_abs_vbeta/sigma;
    
    // Slice sampler for lambda2
     lambda2 = slice_sampler_precision_c(lambda2, a_val, b_val, c_val);
     lambda  = sqrt(lambda2);
    
    ////////////////////////////////////////////////////////////////////////////
    
    if (verbose!=0) {
      
      if ((i%verbose)==0) {
        Rcout << "iter: " << i << " lambda2: " << lambda2 << " sigma2: " << sigma2 << "\n";
      }
    }
    
    // Store MCMC samples
    mBeta.row(i) = vbeta.as_row();
    vsigma2[i] = sigma2;
    vlambda2[i] = lambda2;
    
    // Storage sufficient statistics for Rao-Blackwellization
    mA.row(i) = va_vals.as_row();
    mB.row(i) = vb_vals.as_row();
    mC.row(i) = vc_vals.as_row();
  }
  
  return List::create(_["mBeta"] = mBeta, 
                      _["vsigma2"] = vsigma2, 
                      _["vlambda2"] = vlambda2, 
                      _["mA"] = mA, 
                      _["mB"] = mB,
                      _["mC"] = mC);
}


