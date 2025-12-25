// Part of the sparseHessianFD package
// Copyright (C) 2013-2016 Michael Braun

#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Core>
#include <Eigen/Sparse>


//' @title Estimate sparse Hessian
//' @description Estimate Hessian using triangular subsitution algorithm
//' @param Y Matrix of finite differences of gradients
//' @param colors Vector of length nvars that identifies color of each variable
//' @param jCol,ipntr Column indices and row pointers for non-zero elements of lower triangle of Hessian (row-oriented compressed format).
//' @param delta Perturbation factor used to compute finite differences of gradients.
//' @param nvars Dimension of Hessian (number of variables)
//' @param nnz Number of non-zero elements in the lower triangle of the Hessian.
//' @return A sparse Hessian of class dgCMatrix.
//' @details For internal use.  You should not have to call this function directly.
//[[Rcpp::export]]
Rcpp::S4 subst(const Rcpp::NumericMatrix& Y,
	       const Rcpp::IntegerVector& colors,	  
	       const Rcpp::IntegerVector& jCol,
	       const Rcpp::IntegerVector& ipntr,
	       const double& delta,
	       const int& nvars,
	       const int& nnz) {

  typedef Eigen::Triplet<double> T;
  std::vector<T> Trips;
  Trips.reserve(nnz*2);
  const double inv_delta = 1/delta;
 
  Eigen::MatrixXd B(Y.cols(), nvars);
  B.setZero();

  for (int i=nvars-1; i>=0; --i) {
    int colI = colors(i); 
    for (auto j=jCol.begin()+ipntr(i); j != jCol.begin()+ipntr(i+1); j++) {	
      int colJ = colors(*j);
      double z = Y(i, colJ) * inv_delta  - B(colJ, i);
      B(colI, *j) += z;
      Trips.emplace_back(i, *j, z);   
      if (i != *j) {
	Trips.emplace_back(*j, i, z);
      }
    }
  }

  Eigen::SparseMatrix<double> M(nvars, nvars);
  M.setFromTriplets(Trips.begin(), Trips.end());
  M.makeCompressed();

  return(Rcpp::wrap(M)); 
}

