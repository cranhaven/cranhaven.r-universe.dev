/*
 * RcppIncompleteCholesky.cpp
 *
 *
 *  Created on: Apr 07, 2020
 *      Author: Christoph Koesner
 */

#include <RcppEigen.h>
#include <memory>
#include "KernelGauss.h"
#include "KernelHermite.h"

// [[Rcpp::depends(RcppEigen)]]

/*
 * Computes the kernel matrix given a kernel object.
 * evaluates only the lower triangular matrix in assumption of the kernel function to be symmetric
 */ 
void kernelMatrix(Eigen::VectorXd& x, Eigen::VectorXd& y, Eigen::MatrixXd& m, Kernel& k){
  int rows = x.rows();
  int cols = y.rows();
  int i = 0, j = 0;
  for(i = 0; i < rows; i++){
    for(j = 0; j < cols; j++){
      m(i,j) = k.calc(x(i), y(j));
    }
  }
}

/*
 * wrapper for computing function of the kernel matrix with Gauss kernel
 */
// [[Rcpp::export]]
SEXP kernelMatrixGauss(Eigen::Map<Eigen::VectorXd> x, Eigen::Map<Eigen::VectorXd> y, double sigma){
  std::unique_ptr<Kernel> k(new KernelGauss(sigma));
  Eigen::MatrixXd G(x.rows(), y.rows());
  Eigen::VectorXd xvec = x;
  Eigen::VectorXd yvec = y;

  kernelMatrix(xvec, yvec, G, *k);
  return(Rcpp::wrap(G));
}

/*
 * wrapper for computing function of the kernel matrix with Hermite kernel
 */
// [[Rcpp::export]]
SEXP kernelMatrixHermite(Eigen::Map<Eigen::VectorXd> x, Eigen::Map<Eigen::VectorXd> y, double sigma, int d){
  std::unique_ptr<Kernel> k(new KernelHermite(sigma, d));
  Eigen::MatrixXd G(x.rows(), y.rows());
  Eigen::VectorXd xvec = x;
  Eigen::VectorXd yvec = y;
  
  kernelMatrix(xvec, yvec, G, *k);
  return(Rcpp::wrap(G));
}
