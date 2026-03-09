
#define ARMA_DONT_USE_WRAPPER
#include <RcppArmadillo.h>
#include <RcppNumerical.h>
#include "rinvgaussian.h"

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

// bayesian_lasso_gibbs_c
// Gibbs sampling for Bayesian lasso regression
// Assumes that lambda is fixed
// Assumes that n>p

// [[Rcpp::export]]
List Modified_PC_Gibbs(arma::mat X, arma::vec y,double a1, double b1, double u1, double v1, 
                       int nsamples, double lambda_init, double sigma2_init, 
                       int verbose) {
  int n = X.n_rows;
  int p = X.n_cols;
  
  // Summary statistics that can be calculated once at the beginning
  arma::mat XTX = X.t()*X;
  arma::vec XTy = X.t()*y;
  //double yTy = vy.t()*vy;
  
  int maxiter = nsamples;
  
  // Initialise storage of samples
  arma::mat mBeta(maxiter,p);
  arma::vec vsigma2(maxiter);
  arma::vec vlambda2(maxiter);
  
  // Initialize storage for Rao-Blackwellization
  arma::mat mM(maxiter,p);
  arma::mat mV(maxiter,p);
  arma::vec va_til(maxiter);
  arma::vec vb_til(maxiter);
  arma::vec vu_til(maxiter);
  arma::vec vv_til(maxiter);
  
  // Initialisation
  arma::vec va = ones(p); // expected value of auxiliary variables under q
  double sigma2 = sigma2_init;
  double lambda2 = lambda_init*lambda_init;
  double a_til = a1 + 0.5*(n + p);
  double b_til;
  double u_til = u1 + 0.5*p;
  double v_til;
  
  arma::mat vmu_til;
  arma::mat mSigma_til;
  arma::vec vbeta;
  arma::mat mQ;
  
  // Parameters of va|rest which is inverse Gaussian
  arma::vec vmu;
  arma::vec vlambda = ones(p);
  
  // Main loop
  for (int i = 0; i < maxiter; ++i) 
  {
    // Sample from beta|rest
    mQ = inv(XTX + diagmat(lambda2*va));
    vmu_til = mQ*XTy;
    mSigma_til = sigma2*mQ;
    vbeta = mvnrnd(vmu_til, mSigma_til, 1);
    
    // Sample from sigma2|rest
    b_til = b1 + 0.5*(sum(pow(y-X*vbeta,2.0)) + lambda2*sum(va%pow(vbeta,2.0)));
    sigma2 = 1/randg(distr_param(a_til,1/b_til));
    
    // Sample from lambda2|rest
    v_til = v1 + 0.5*sum(va%pow(vbeta,2.0))/sigma2;
    lambda2 = randg(distr_param(u_til,1/v_til));
    
    // Update q(va) 
    vmu = sqrt(sigma2/(lambda2*(pow(vbeta,2.0))));
    va = rinvgaussian_c(vmu, vlambda);
    
    if (verbose!=0) {
      if ((i%verbose)==0) {
        Rcout << "iter: " << i << "\n";
      }
    }
    
    // Storing samples
    mBeta.row(i) = vbeta.as_row();
    vsigma2[i] = sigma2;
    vlambda2[i] = lambda2;
    
    // Storage for Rao-Blackwellization
    mM.row(i) = vmu_til.as_row();
    mV.row(i) = diagvec(mSigma_til).as_row();
    va_til[i] = a_til;
    vb_til[i] = b_til;
    vu_til[i] = u_til;
    vv_til[i] = v_til;
  }
  
  return List::create(_["mBeta"] = mBeta, 
                      _["vsigma2"] = vsigma2, 
                      _["vlambda2"] = vlambda2, 
                      _["mM"] = mM, 
                      _["mV"] = mV,
                      _["va_til"] = va_til, 
                      _["vb_til"] = vb_til,
                      _["vu_til"] = vu_til, 
                      _["vv_til"] = vv_til);
}


// looping through each column and element wise multiplication
// this is included in another file I think
// // [[Rcpp::export]]
// arma::mat matTimesVec(arma::mat mat, arma::vec v) {
//   for(int i; i < mat.n_cols; i++){
//     mat.col(i)  %=  v;
//   }
//   return mat;
// }

// form a diagonal matrix with the vector and then use matrix multiplication
// this is included in another file I think
// // [[Rcpp::export]]
// arma::mat matTimesVec2(arma::mat mat, arma::vec v) {
//   return arma::diagmat(v) * mat;
// }

// use the functionality described at http://arma.sourceforge.net/docs.html#each_colrow 
// to "Apply a vector operation to each column or row of a matrix "
// this is included in another file I think
// // [[Rcpp::export]]
// arma::mat matTimesVec3(arma::mat mat, arma::vec v) {
//   mat.each_col() %= v;
//   return mat; 
// }

arma::mat crossprod_cpp(arma::mat mX, arma::vec vw, arma::mat mD, double trunc) 
{
  int n = mX.n_rows;
  int p = mX.n_cols;
  
  arma::mat out = mD;
  for (int i = 0; i<p; ++i) {
    if (vw[i]>trunc) {
      arma::vec vx = mX.col(i);
      out = out + (vw[i]*vx) * vx.t();
    }
  }
  return out;
}


