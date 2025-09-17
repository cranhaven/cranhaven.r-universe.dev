/*
	basic tools to implement models and methods from Rue and Held
 */

 // disable assertions
 #define EIGEN_NO_DEBUG

#include <RcppEigen.h>


namespace mcstat2 {
namespace glm {

	using namespace Rcpp;

  using Eigen::VectorXd;
  using Eigen::MatrixXd;
  using Eigen::Map;

  // supported glm families
  enum glmfamily { poisson };

  // evaluate glm log-likelihood
  //
  // Parameters:
  //  y - observations
  //  eta - GLM means
  //  n - number of observations
  //  family - specification of likelihood family
  double ll(const double* y, const double* eta, const int n,
    const glmfamily family);

  /*
   Follow Rue and Held (2005) Section 4.4.1 to construct a GMRF approximation
   for the posterior distribution of a latent GMRF with respect to a
   conditionally independent GLM response.  This function only focuses on the
   quadratic expansion of the GLM likelihood function so that the approximation
   can be paired with any number of latent structures.

   Parameters:
     b - (pre-initialized output) array in which to store b vector in eqn. 4.27
     c - (pre-initialized output) array in which to store c vector in eqn. 4.27
     x0 - values around which the Taylor approximation should be centered
     y - observations
     n - GMRF dimension
  */
  void gmrf_approx(double* b, double* c, const double* x0, const double* y,
    int n, const glmfamily family);

}}
