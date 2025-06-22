/*
 *  SBMTrees: Sequential imputation with Bayesian Trees Mixed-Effects models
 *  Copyright (C) 2024 Jungang Zou
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/GPL-2
 */

#ifndef ARMADILLO_H_
#define ARMADILLO_H_
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#endif

#include <Rcpp.h>

#include <unordered_map>
#include <string>


using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
double max_d(double x, double y){
  if (x > y)
    return x;
  return y;
}

// [[Rcpp::export]]
NumericVector seqD(double x, double y, double by = 1) {
  
  // length of result vector
  int nRatio = (y - x) / by;
  NumericVector anOut(nRatio + 1);
  
  // compute sequence
  int n = 0;
  for (double i = x; i <= y; i = i + by) {
    anOut[n] = i;
    n += 1;
  }
  
  return anOut;
}

// [[Rcpp::export]]
IntegerVector seqC(int x, int y, int by = 1) {
  // length of result vector
  int nRatio = (y - x) / by;
  IntegerVector anOut(nRatio + 1);
  
  // compute sequence
  int n = 0;
  for (int i = x; i <= y; i = i + by) {
    anOut[n] = i;
    n += 1;
  }
  
  return anOut;
}

// [[Rcpp::export]]
double prodC(NumericVector x) {
  
  int n = x.size();
  double out;
  
  out = x[0];
  for(int i = 1; i < n; ++i) {
    out = out * x[i];
  }
  return out;
}


// [[Rcpp::export]]
NumericVector sum_array_by_name(NumericVector X){
  CharacterVector location_name = X.names();
  CharacterVector cluster = unique(location_name);
  NumericVector sum_array(cluster.length());
  sum_array.names() = cluster;
  for(int i = 0 ; i < X.length(); ++i){
    String cluster_name = location_name[i];
    sum_array[cluster_name] = sum_array[cluster_name] + X[i];
  }
  return sum_array;
}


// [[Rcpp::export]]
NumericVector set_tol(NumericVector X, double tol){
  LogicalVector index = X <= tol;
  X[index] = tol;
  return X;
}

// [[Rcpp::export]]
NumericVector set_value(NumericVector X, double tol){
  for(int i = 0 ; i < X.length(); ++i)
      X[i] = tol;
  return X;
}


// [[Rcpp::export]]
LogicalVector character_vector_equals(CharacterVector X, CharacterVector Y){
  LogicalVector equals(X.length());
  for(int i = 0; i < X.length(); ++i){
    if (X[i] == Y[0]){
      equals[i] = true;
    }
  }
  return equals;
}





// [[Rcpp::export]]
NumericMatrix row_matrix(NumericMatrix X, LogicalVector index){
  long nrow = sum(index);
  long ncol = X.cols();
  long n = X.rows();
  NumericMatrix Y(nrow, ncol);
  IntegerVector Z_row = seqC(0, n-1)[index];
  for(int i = 0; i < nrow; ++i){
    Y(i, _) = X(Z_row[i], _);
  }
  return Y;
}

// [[Rcpp::export]]
NumericMatrix matrix_add(NumericMatrix X, NumericMatrix Z){
  return wrap(as<mat>(X) + as<mat>(Z));
}


// [[Rcpp::export]]                                                                                                                                           
bool contains(std::string s, Rcpp::List L) {                                                                                                                  
  Rcpp::CharacterVector nv = L.names();                                                                                                                     
  for (int i=0; i<nv.size(); i++) {                                                                                                                         
    if (std::string(nv[i]) == s) {                                                                                                                        
      return true;                                                                                                                                      
    }                                                                                                                                                     
  }                                                                                                                                                         
  return false;                                                                                                                                             
} 


// [[Rcpp::export]]                                                                                                                                           
int contains_index(CharacterVector L, std::string s) {                                                                                                                  
  for (int i=0; i<L.length(); i++) {                                                                                                                         
    if (std::string(L[i]) == s) {                                                                                                                        
      return i;                                                                                                                                      
    }                                                                                                                                                     
  }                                                                                                                                                         
  return -1;                                                                                                                                             
}   


// [[Rcpp::export]]  
NumericMatrix row_matrix_unique_rowname(NumericMatrix X, CharacterVector rowname){
  long nrow = rowname.length();
  long ncol = X.cols();
  NumericMatrix Y(nrow, ncol);
  int k = 0;
  CharacterVector X_name = rownames(X);
  CharacterVector rowname_copy = clone(rowname);
  CharacterVector new_rowname;
  for(int i = 0 ; i < X.rows(); ++i){
    for(int j = 0 ; j < rowname_copy.length(); ++j){
      if(std::string(X_name[i]) == std::string(rowname_copy[j])){
        NumericVector X_current = X(i,_);
        Y(k, _) = clone(X_current);
        k++;
        new_rowname.push_back(X_name[i]);
        rowname_copy.erase(j);
        break;
      }
    }
  }
  rownames(Y) = new_rowname;
  return Y;
}

// [[Rcpp::export]]  
NumericVector row_matrix_rowname(NumericMatrix X, String rowname){
  CharacterVector X_name = rownames(X);
  NumericVector X_current;
  for(int i = 0 ; i < X.rows(); ++i){
      if(std::string(X_name[i]) == std::string(rowname)){
        X_current = X(i,_);
        return clone(X_current);
    }
  }
  return clone(X_current);
}

// [[Rcpp::export]]  
NumericMatrix matrix_mul_scalar(NumericMatrix X, double scalar){
  return wrap(as<mat>(X) * scalar);
}

// [[Rcpp::export]]  
NumericMatrix::Row row_matrix_by_rowname(NumericMatrix X, String rowname){
  CharacterVector X_name = rownames(X);
  for(int i = 0 ; i < X.rows(); ++i){
    if(std::string(X_name[i]) == std::string(rowname)){
      return X(i,_);
    }
  }
  return X(0,_);
}


// [[Rcpp::export]]
int count_if(LogicalVector x) {
  int counter = 0;
  for(int i = 0; i < x.size(); i++) {
    if(x[i] == TRUE) {
      counter++;
    }
  }
  return counter;
}

// [[Rcpp::export]]
LogicalVector logic_and(LogicalVector x, LogicalVector y){
  LogicalVector z;
  if(x.length() != y.length()){
    return z;
  }else{
    for(int i = 0 ; i < x.length(); ++i){

        z.push_back(x[i] && y[i]);
     
    }
  }
  return z;
}
  
// [[Rcpp::export]]
bool any(LogicalVector x){
  for(int i = 0 ; i < x.length() ; ++i){
    if(x[i])
      return true;
  }
  return false;
}


// [[Rcpp::export]]
NumericMatrix solve(NumericMatrix m) {
  return wrap(inv(as<mat>(m)));
}

// [[Rcpp::export]]
NumericMatrix solve_pos_def(NumericMatrix m) {
  return wrap(inv_sympd(as<mat>(m)));
}


// [[Rcpp::export]]
NumericMatrix matrix_multiply(NumericMatrix mat1, NumericMatrix mat2) {
  // Convert NumericMatrix to Armadillo matrices
  return wrap(as<mat>(mat1) * as<mat>(mat2));
}

// [[Rcpp::export]]
NumericVector rtgamma(int n, double shape, double scale, double lower, double upper) {
  // Compute the CDF at the truncation limits
  double F_lower = R::pgamma(lower, shape, scale, /*lower_tail=*/1, /*log_p=*/0);
  double F_upper = R::pgamma(upper, shape, scale, /*lower_tail=*/1, /*log_p=*/0);
  
  // Error handling: Ensure that F_lower < F_upper
  if (F_lower >= F_upper) {
    stop("Invalid truncation limits or gamma parameters.");
  }
  
  // Allocate vector for samples
  NumericVector samples(n);
  
  // Generate samples
  for (int i = 0; i < n; ++i) {
    // Uniform random number between F_lower and F_upper
    double u = R::runif(F_lower, F_upper);
    
    // Inverse CDF (quantile function) to get the sample
    samples[i] = R::qgamma(u, shape, scale, /*lower_tail=*/1, /*log_p=*/0);
  }
  
  return samples;
}

// [[Rcpp::export]]
NumericMatrix make_symmetric(NumericMatrix m) {
  return wrap(symmatu(as<mat>(m)));
}

// [[Rcpp::export]]
NumericMatrix vector_mul_generate_matrix(NumericVector v) {
  NumericMatrix m(v.size(), 1, v.begin());
  return wrap(as<mat>(m) * as<mat>(m).t());
}

// [[Rcpp::export]]
NumericMatrix make_nonsingular(NumericMatrix s){
  double d = s.nrow();
  double tol = std::pow(1e-30, (1/d));
  NumericMatrix temp = NumericMatrix(d, d);
  temp.fill_diag(tol);
  return matrix_add(s, temp);
}

// // [[Rcpp::export]]
// List create_subject_to_B(CharacterVector subject_id){
//   List subject_to_B;
//   CharacterVector unique_subject = unique(subject_id);
//   for(int i = 0 ; i < unique_subject.length(); ++i){
//     String subject = unique_subject[i];
//     subject_to_B.push_back(i, subject);
//   }
//   return subject_to_B;
// }

// [[Rcpp::export]]
std::unordered_map<std::string, int> create_subject_to_B(CharacterVector subject_id){
  std::unordered_map<std::string, int> subject_to_B;
  CharacterVector unique_subject = unique(subject_id);
  for (int i = 0 ; i < unique_subject.length(); ++i){
    subject_to_B[std::string(unique_subject[i])] = i;
  }
  return subject_to_B;
}


// [[Rcpp::export]]
std::unordered_map<std::string, int> create_row_id_to_row(IntegerVector row_id){
  std::unordered_map<std::string, int> row_id_to_row;
  for (int i = 0 ; i < row_id.length(); ++i){
    row_id_to_row[std::to_string(row_id[i])] = i;
  }
  return row_id_to_row;
}


// // [[Rcpp::export]]
// Rcpp::CharacterVector get_keys(std::unordered_map<std::string, int> myMap) {
//   Rcpp::CharacterVector keys;
//   // for (const auto& kv : myMap) {
//   //   keys.push_back(kv.first); // kv.first is the key
//   // }
//   return keys;
// }


// [[Rcpp::export]]
double innerProduct(const NumericVector& x, const NumericVector& y) {
  // Convert NumericVector to arma::vec
  arma::vec x_vec = as<arma::vec>(x);
  arma::vec y_vec = as<arma::vec>(y);
  
  // Calculate the inner product of vectors x and y
  return arma::dot(x_vec, y_vec);
}

// [[Rcpp::export]]
NumericMatrix cov(const Rcpp::NumericMatrix& m, double regularization = 1e-6) {
  // Convert NumericMatrix to arma::mat
  arma::mat arma_mat = Rcpp::as<arma::mat>(m);
  
  // Calculate the covariance matrix
  arma::mat covariance = arma::cov(arma_mat);
  arma::mat regularized_cov = covariance + regularization * arma::eye(covariance.n_rows, covariance.n_cols);
  
  return wrap(regularized_cov);
}

// [[Rcpp::export]]
bool isPositiveDefinite(const Rcpp::NumericMatrix& m) {
  // Attempt Cholesky decomposition
  arma::mat arma_mat = Rcpp::as<arma::mat>(m);
  arma::mat chol_result;
  bool success = arma::chol(chol_result, arma_mat);
  
  return success;
}

// [[Rcpp::export]]
NumericMatrix fix_riwish(const Rcpp::NumericMatrix& m, double regularization = 1e-6) {
  // Convert NumericMatrix to arma::mat
  arma::mat arma_mat = Rcpp::as<arma::mat>(m);
  
  arma::mat regularized = arma_mat + regularization * arma::eye(arma_mat.n_rows, arma_mat.n_cols);
  
  return make_symmetric(wrap(regularized));
}




// [[Rcpp::export]]
NumericVector matrix_slice_parallel(NumericMatrix A, int i, bool row){
  int n = A.nrow();
  int p = A.ncol();
  
  NumericVector vec;
  if(row){
    for(int j = 0 ; j < p ; ++j){
      vec.push_back(A(i, j));
    }
  }else{
    for(int j = 0 ; j < n ; ++j){
      vec.push_back(A(j, i));
    }
  }
  return vec;
}

// [[Rcpp::export]]
arma::mat rmvnormArma(int n, const arma::vec& mean, const arma::mat& sigma) {
  int k = mean.n_elem;  // dimension of the multivariate normal distribution
  arma::mat Y = arma::randn(n, k);  // generate n*k matrix of standard normal random variables
  return arma::repmat(mean.t(), n, 1) + Y * arma::chol(sigma);  // transform to multivariate normal
}

// [[Rcpp::export]]
arma::mat cholArma(const arma::mat& sigma) {
    // generate n*k matrix of standard normal random variables
  return arma::chol(sigma);  // transform to multivariate normal
}

// [[Rcpp::export]]
arma::mat rwishart(const int df, const arma::mat& S) {
  arma::uword m = S.n_cols;
  arma::uword i, j;
  arma::mat A(m, m, arma::fill::zeros);
  for ( i = 1; i < m; ++i ) {
    for ( j = 0; j < i; ++j ) {
      A(i, j) = R::rnorm(0.0, 1.0);
    }
  }
  for ( i = 0; i < m; ++i ) {
    A(i, i) = sqrt(R::rchisq(df - i));
  }
  arma::mat B = A.t() * arma::chol(S);
  return B.t() * B;
}


// [[Rcpp::export]]
arma::mat riwishArma(const int df, const arma::mat& S) {
  return inv_sympd(rwishart(df, inv_sympd(S))); // inv_sympd is used to calculate the inverse of positive definite function
}


// [[Rcpp::export]]
double rinvgamma(double a, double b) {
  double s = R::rgamma(a, 1 / b);
  return 1 / s;
}

// [[Rcpp::export]]
double qinvgamma(double p, double shape, double scale) {
  // Get the quantile of the Gamma distribution
  NumericVector quant = {p};
  double gamma_quantile = qgamma(quant, shape, 1 / scale)[0];
  
  // Transform to Inverse Gamma by taking the reciprocal
  return 1.0 / gamma_quantile;
}


// [[Rcpp::export]]
double quadratic_form(NumericMatrix X, NumericVector mu, NumericMatrix Sigma) {
  arma::mat X_ = as<arma::mat>(X).each_row() - as<arma::rowvec>(mu);
  arma::mat Inv_Sigma = inv_sympd(as<arma::mat>(Sigma));
  arma::mat mul = X_ * Inv_Sigma * X_.t();
  return arma::trace(mul);
}


// [[Rcpp::export]]
IntegerVector rowSums_I(NumericMatrix mat) {
  int nRows = mat.nrow();
  
  IntegerVector rowSums;
  //Rcout << mat.size() << std::endl;
  //Rcpp::Rcout << "Matrix dimensions: " << mat.nrow() << "x" << mat.ncol() << std::endl;
  for (int i = 0; i < nRows; ++i) {
    rowSums.push_back(sum(mat(i, _)));
  }
  
  return rowSums;
}
