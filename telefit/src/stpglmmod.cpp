/*
	gibbs sampler to forecast using a fitted spatially varying coefficient model.
 */

 // disable assertions
 #define EIGEN_NO_DEBUG

 #include <RcppArmadillo.h>
 #include <RcppEigen.h>
#include "GibbsSampler.h"
#include "numAlg.h"
#include "covs.h"
#include "distributions.h"

using namespace Rcpp;
using namespace arma;

using Eigen::VectorXd;
using Eigen::MatrixXd;
using Eigen::Map;

VectorXd linpred(MatrixXd X, VectorXd beta, MatrixXd A, VectorXd a,
  VectorXd w, int k, int ns, int nt) {
  // build RESP-GLM linear predictor:
  //  \v\eta = X \beta + \tilde A (1 \otimes \tilde a') + w
  //
  // Parameters:
  //  X - n_s n_t x p block row matrix of covariates (1 block per timepoint)
  // beta - p x 1 vector of regression coefficients
  // A - k x n_t matrix of spatial basis function coefficients across time
  // a - k n_s x 1 block vector of teleconnection coefs. (1 block per location)
  // w - n_s n_t x 1 block vector of spatial effects (1 block per timepoint)
  // k - number of remote spatial basis functions
  // ns - number of areal units
  // nt - number of timepoints

  Map<MatrixXd> aM(a.data(), k, ns);
  MatrixXd telM = aM.transpose()*A;
  Map<VectorXd> tel(telM.data(), ns*nt);

  return X * beta + tel + w;
}
