#ifndef _DISTRIBUTIONS_H
#define _DISTRIBUTIONS_H

// disable assertions
#define EIGEN_NO_DEBUG

#include <RcppArmadillo.h>
#include <RcppEigen.h>

namespace mcstat2 {
	
	using namespace Rcpp;
	using namespace arma;
    using Eigen::MatrixXd;
	
    // sample x0 | x1 when (x0,x1) ~ N(0, Sigma), where Sigma is partitioned
    // into S00, S01, and S11 components
    vec mvnorm_cond(MatrixXd &x1, mat &S00_arma, mat &S01_arma, mat  &S11_arma);
	
}



#endif
