// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                        :::::::::::::::::::: //
// ::::::::::::::::::::    QR decomposition    :::::::::::::::::::: //
// ::::::::::::::::::::                        :::::::::::::::::::: //
// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
#include <RcppEigen.h>

#define EIGEN_USE_BLAS
#define EIGEN_USE_LAPACKE

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppEigen)]]

using namespace Eigen;

// ::::::::::::::::::::::::::::::::::::::::::::::::::
// Givens rotation
// function to compute the Givens rotation a two input values a, b, such that **out * (a, b) = (d, 0)
// a double, b double
Eigen::Vector2d givens (const double& a, const double& b) {

  double tau, c = 0.0, s = 0.0;
  
  if (b == 0) {
    c = 1.0; 
    s = 0.0;
  } else {
    if (std::abs(b) > std::abs(a)) {
      tau = -a / b;
      s   = 1.0 / sqrt(1.0 + std::pow(tau, 2));
      c   = s * tau;
      
      if (b > 0.0) {
        c = -c;
        s = -s;
      } 
    } else {
      tau = -b / a;
      c   = 1.0 / std::sqrt(1.0 + std::pow(tau, 2));
      s   = c * tau;
      
      if (a < 0.0) {
        c = -c;
        s = -s;
      } 
    }
  }
  
  /* Get output      */
  Eigen::Vector2d out;
  out << c, s;
  
  /* Return output      */
  return out;
}

// ::::::::::::::::::::::::::::::::::::::::::::::::::
// Householder reflections
// function to compute the Householder rotation of vector input x,
// such that P * (x) = (c, 0, ..., 0), with d in R and P = I - beta * vv'
// x vector
Eigen::VectorXd householder (const Eigen::VectorXd& x) {
  
  const int m = x.rows();
  double beta = 0.0, mu = 0.0;
  Eigen::VectorXd v(m); v.setZero();
  
  if (m > 1) {
    const double sigma = x.segment(1, m-1).transpose() * x.segment(1, m-1);
    v(0)              = 1.0;
    v.segment(1, m-1) = x.segment(1, m-1);
    
    if ((sigma == 0.0) && (x(0) >= 0.0)) {
      mu   = x(0);
      beta = 0.0;
    } else if ((sigma == 0.0) && (x(0) < 0.0)) {
      mu   = -x(0);
      beta = 2.0;
    } else {
      mu = sqrt(sigma + std::pow(x(0), 2));
      if (x(0) < 0.0) {
        v(0) = x(0) - mu;
      } else {
        v(0) = -sigma / (x(0) + mu);
      }
      beta = 2.0 * std::pow(v(0), 2) / (sigma + std::pow(v(0), 2));
      v   /= v(0);
    }
  } else {
    v = Eigen::VectorXd::Zero(1);
    beta = 0.0;
  }
  
  /* Get output      */
  Eigen::VectorXd out(m+2);
  out << v, beta, mu;
  
  /* Return output      */
  return out;
}

// ::::::::::::::::::::::::::::::::::::::::::::::::::
// Householder (to be used for the L update only)
// function to compute the Householder rotation of vector input x, such that
// P * (x) = (c, 0, ..., 0), with d in R and P = I - beta * vv'
// x vector
Eigen::VectorXd houseL (const Eigen::VectorXd& x) {

  const int m = x.rows();
  double beta = 0.0, mu = 0.0;
  Eigen::VectorXd v(m); v.setZero();
  
  if (m > 1) {
    const double sigma = x.segment(1, m-1).transpose() * x.segment(1, m-1);
    v(0)              = 1.0;
    v.segment(1, m-1) = x.segment(1, m-1);
    
    if ((sigma == 0.0) && (x(0) >= 0.0)) {
      beta = 0.0;
    } else if ((sigma == 0.0) && (x(0) < 0.0)) {
      beta = 2.0;
    } else {
      mu = sqrt(sigma + std::pow(x(0), 2));
      if (x(0) < 0.0) {
        v(0) = x(0) - mu;
      } else {
        v(0) = -sigma / (x(0) + mu);
      }
      beta = 2.0 * std::pow(v(0), 2) / (sigma + std::pow(v(0), 2));
      v   /= v(0);
    }
  } else {
    v    = Eigen::VectorXd::Zero(1);
    beta = 0.0;
  }
  
  /* Get output      */
  Eigen::VectorXd out(m+1);
  out << v, beta;
  
  /* Return output      */
  return out;
}






// end file
