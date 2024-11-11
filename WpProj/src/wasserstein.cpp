#include "wasserstein.h"

double wasserstein_p(const refVec & mass,
                     const refMat & cost, const double p,
                     const refVecI & from,  const refVecI & to) {
  
  int N = from.size();
  
  double loss = 0.0;
  
  for (int n = 0; n < N; n ++){
    int a_idx = from(n);
    int b_idx = to(n);
    // double mass = mass_a(n);
    double cur_cost = cost(a_idx, b_idx);
    
    loss += std::pow(cur_cost, p) * mass(n);
  }
  
  return(std::pow(loss, 1.0/p));
}

double wasserstein_2(const refVec & mass,
                     const refMat & cost,
                     const refVecI & from,  const refVecI & to) {
  
  int N = from.size();
  
  double loss = 0.0;
  
  for (int n = 0; n < N; n ++){
    // int a_idx = from(n);
    // int b_idx = to(n);
    // double mass = mass_a(n);
    double cur_cost = cost(from(n), to(n));
    loss += cur_cost * cur_cost * mass(n);
  }
  
  return( std::sqrt(loss) );
}

double wasserstein_1(const refVec & mass,
                     const refMat & cost,
                     const refVecI & from,  const refVecI & to) {
  
  int N = from.size();
  
  double loss = 0.0;
  
  for (int n = 0; n < N; n ++){
    int a_idx = from(n);
    int b_idx = to(n);
    // double mass = mass_a(n);
    // double cur_cost = cost(a_idx, b_idx);
    loss += cost(a_idx, b_idx) * mass(n);
  }
  
  return(loss);
}

double wasserstein(const refVec & mass,
                   const refMat & cost, const double p, 
                   const refVecI & from,  const refVecI & to) {
  double loss;
  if( p == 2.0) {
    loss = wasserstein_2(mass, cost, from, to);
  } else if (p == 1.0) {
    loss = wasserstein_1(mass, cost, from, to);
  } else {
    loss = wasserstein_p(mass, cost, p, from, to);
  }
  
  return(loss);
}

double wasserstein_p_iid_p(refMat X, refMat Y, double p){
  
  if(X.cols() != Y.cols()){
    Rcpp::stop("Number of columns of first matrix don't match number of columns of second matrix");
  }
  
  if(X.rows() != Y.rows()){
    Rcpp::stop("Number of rows of first matrix don't match number of rows of second matrix");
  }
  
  sort_matrix_by_row(X);
  sort_matrix_by_row(Y);
  
  return (X-Y).cwiseAbs().array().pow(p).mean();
}

double wasserstein_p_iid(refMat X, refMat Y, double p){
  
  if(X.cols() != Y.cols()){
    Rcpp::stop("Number of columns of first matrix don't match number of columns of second matrix");
  }
  
  if(X.rows() != Y.rows()){
    Rcpp::stop("Number of rows of first matrix don't match number of rows of second matrix");
  }
  
  sort_matrix_by_row(X);
  sort_matrix_by_row(Y);
  
  vector loss = (X-Y).cwiseAbs().array().pow(p).colwise().mean();
  loss = loss.array().pow(1.0/p);
  return loss.mean();
}

double wasserstein_2_iid(refMat X, refMat Y){
  
  if(X.cols() != Y.cols()){
    Rcpp::stop("Number of columns of first matrix don't match number of columns of second matrix");
  }
  
  if(X.rows() != Y.rows()){
    Rcpp::stop("Number of rows of first matrix don't match number of rows of second matrix");
  }
  
  sort_matrix_by_row(X);
  sort_matrix_by_row(Y);
  
  vector loss = (X-Y).cwiseAbs2().array().colwise().mean().sqrt();
  return loss.mean();
}

double wasserstein_2_iid_2(refMat X, refMat Y){
  
  if(X.cols() != Y.cols()) {
    Rcpp::stop("Number of columns of first matrix don't match number of columns of second matrix");
  }
  
  if(X.rows() != Y.rows()) {
    Rcpp::stop("Number of rows of first matrix don't match number of rows of second matrix");
  }
  
  sort_matrix_by_row(X);
  sort_matrix_by_row(Y);
  
  return (X-Y).cwiseAbs2().mean();
}

double wasserstein_1_iid(refMat X, refMat Y){
  
  if(X.cols() != Y.cols()){
    Rcpp::stop("Number of columns of first matrix don't match number of columns of second matrix");
  }
  
  if(X.rows() != Y.rows()){
    Rcpp::stop("Number of rows of first matrix don't match number of rows of second matrix");
  }
  
  sort_matrix_by_row(X);
  sort_matrix_by_row(Y);
  
  return (X-Y).cwiseAbs().array().mean();
}

//[[Rcpp::export]]
double wasserstein_(const Rcpp::NumericVector & mass_,
             const Rcpp::NumericMatrix & cost_, const double p, 
             const Rcpp::IntegerVector & from_,  const Rcpp::IntegerVector & to_) {
  
  int N = from_.size();
  const vecMap mass(Rcpp::as<vecMap >(mass_));
  // const vecMap mass_b(Rcpp::as<vecMap >(mass_b_));
  const matMap cost(Rcpp::as<matMap >(cost_));
  vectorI from(N);
  vectorI to(N);
  // const vecMapI from(Rcpp::as<vecMapI >(from_));
  // const vecMapI to(Rcpp::as<vecMapI >(to_));
  
  for(int n = 0; n < N; n ++) {
    to(n) = to_(n) - 1;
    from(n) = from_(n) - 1;
  }
  
  return( wasserstein(mass, cost, p, from, to) );
}

//[[Rcpp::export]]
double wasserstein_p_iid_(const SEXP & X_, const SEXP & Y_, double p) {
  
  const matMap Xtemp(Rcpp::as<matMap >(X_));
  const matMap Ytemp(Rcpp::as<matMap >(Y_));
  
  matrix X = Xtemp;
  matrix Y = Ytemp;
  
  if ( p == 2.0 ) {
    // Rcout << "w2";
    return wasserstein_2_iid(X,Y);
  } else if ( p == 1.0 ) {
    // Rcout << "wp";
    return wasserstein_1_iid(X,Y);
  } else {
    return wasserstein_p_iid(X,Y,p);
  }
}

//[[Rcpp::export]]
double wasserstein_p_iid_p_(const SEXP & X_, const SEXP & Y_, double p) {
  
  const matMap Xtemp(Rcpp::as<matMap >(X_));
  const matMap Ytemp(Rcpp::as<matMap >(Y_));
  
  matrix X = Xtemp;
  matrix Y = Ytemp;
  
  if ( p == 2.0 ) {
    // Rcout << "w2";
    return wasserstein_2_iid_2(X,Y);
  } else if ( p == 1.0 ) {
    // Rcout << "wp";
    return wasserstein_1_iid(X,Y);
  } else {
    return wasserstein_p_iid_p(X,Y,p);
  }
}
