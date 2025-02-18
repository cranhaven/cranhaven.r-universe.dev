// ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //
// ::::::::::::::::::::                                         :::::::::::::::::::: //
// ::::::::::::::::::::    General utilities functions          :::::::::::::::::::: //
// ::::::::::::::::::::                                         :::::::::::::::::::: //
// :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: //

// Utility routines

// Authors:
//           Bernardi Mauro, University of Padova
//           Last update: october 7, 2024

// [[Rcpp::depends(RcppArmadillo)]]
#include "utils.h"

void subtractone(int& val) {
  val -= 1;  // Subtract 1 from each element
}

// [[Rcpp::export]]
arma::uvec set_diff (arma::uvec x,
                     arma::uvec y) {
  x = arma::unique(x);
  y = arma::unique(y);
  for (size_t j = 0; j < y.n_elem; j++) {
    arma::uvec q1 = arma::find(x == y[j]);
    if (!q1.empty()) {
      x.shed_row(q1(0));
    }
  }
  return x;
}

Eigen::VectorXd vec2scalar_prod (const Eigen::VectorXd& y,
                                 const double& x) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  unsigned int n = 0;
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                  */
  n = y.size();

  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vector and matrices declaration                       */
  Eigen::VectorXd y_prod(n);                      y_prod.setZero();
  
  /* :::::::::::::::::::::::::::::::::::::::::
   Perform product                 */
  for (int i=0; i<n; i++) {
    y_prod(i) = y(i) * x;
  }
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return output                                        */
  return y_prod;
}

// Function to convert a matrix to a vector (vec operator)
Eigen::VectorXd mat2vec (const Eigen::MatrixXd& X) {
  return Eigen::Map<const Eigen::VectorXd>(X.data(), X.size());
}

// Function to convert a symmetric matrix to a vector (vech operator)
Eigen::VectorXd symmat2vech (const Eigen::MatrixXd& X,
                             const bool byrow) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  int index = 0;
  int size = (n * (n + 1)) / 2; // Size of the lower triangular part including diagonal
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::VectorXd res(size);                 res.setZero();
  
  // checks
  if (n != p) {
    Rcpp::stop("Input matrix must be square.");
  }
  if (byrow) {
    res = symmat2vech_byrow(X, n, p);
  } else {
    res = symmat2vech_bycol(X, n, p);
  }
  return res;
}

// Function to convert a symmetric matrix to a vector (vech operator)
Eigen::VectorXd symmat2vech_byrow (const Eigen::MatrixXd& X,
                                   const int n,
                                   const int p) {
    
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  int index = 0;
  int size = (n * (n + 1)) / 2; // Size of the lower triangular part including diagonal
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::VectorXd result(size);                 result.setZero();
  
  // checks
  index = 0;
  for (int i = 0; i < n; i++) {
    result.segment(index, i+1) = X.block(0, i, i+1, 1);
    index                     += i+1;
  }
  return result;
}

Eigen::VectorXd symmat2vech_bycol (const Eigen::MatrixXd& X,
                                   const int n,
                                   const int p) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   variable declaration                                  */
  int index = 0;
  int size = (n * (n + 1)) / 2; // Size of the lower triangular part including diagonal
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::VectorXd result(size);                 result.setZero();
  
  // checks
  index = 0;
  for (int i = 0; i < n; i++) {
    result.segment(index, n-i) = (X.block(i, i, 1, n-i)).transpose();
    index                     += n-i;
  }
  return result;
}

Eigen::MatrixXd mat_slicing_byrow (const Eigen::MatrixXd& X,
                                   const Eigen::ArrayXi index) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Y(p, n);         Y.setZero();          
  Y = X.transpose();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return                                                   */
  //return((Y(Eigen::placeholders::all, index)).transpose());
  return((Y(Eigen::all, index)).transpose());
}

Eigen::MatrixXd mat_slicing_byrow2 (const Eigen::MatrixXd& X,
                                    const std::vector<int> index) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Y(p, n);         Y.setZero();
  Y = X.transpose();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return                  */
  //return((Y(Eigen::placeholders::all, index)).transpose());
  return((Y(Eigen::all, index)).transpose());
}

Eigen::MatrixXd mat_slicing_byrow3 (const Eigen::MatrixXd& X,
                                    std::vector<int> index) {
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   get dimensions                                  */
  const int n = X.rows();
  const int p = X.cols();
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   vectors and matrices declaration                    */
  Eigen::MatrixXd Y(p, n);         Y.setZero();
  Y = X.transpose();
  std::for_each(index.begin(), index.end(), subtractone);
  
  /* ::::::::::::::::::::::::::::::::::::::::::::::
   return                                             */
  //return((Y(Eigen::placeholders::all, index)).transpose());
  return((Y(Eigen::all, index)).transpose());
}



































// end of file
