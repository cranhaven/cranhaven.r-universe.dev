#include <RcppArmadillo.h>
#include "lvmcomp_omp.h"
#include "calcu_sigma_cmle.h"
#include "my_Logistic.h"
#include "depend_funcs.h"

//' @useDynLib lvmcomp
//' @importFrom Rcpp evalCpp
// [[Rcpp::export]]
Rcpp::List stem_mirtc(const arma::mat &response, const arma::mat &Q,
                     arma::mat A0, arma::vec d0, arma::mat theta0, 
                     arma::mat sigma0, int T, bool parallel){
  if(!parallel)
    omp_set_num_threads(1);
  else
    omp_set_num_threads(omp_get_num_procs());
  int N = response.n_rows;
  int J = response.n_cols;
  int K = A0.n_cols;
  int i, j, mcn = 0;
  //arma::vec x = arma::linspace(-5,5,10);
  arma::vec x(3);
  x(0) = -4;
  x(1) = 0;
  x(2) = 4;
  arma::mat inv_sigma(K, K);
  arma::mat res = arma::zeros(T, K*K+J*K+J);
  arma::vec glm_res(K+1);
  arma::uvec rv(1), cv;
  for(mcn=0;mcn<T;++mcn){
    Rprintf("\rStep  %d\t| ", mcn+1);
    Rcpp::checkUserInterrupt();
    // E step
    inv_sigma = arma::inv(sigma0);
#pragma omp parallel for
    for(i=0;i<N;++i){
      theta0.row(i) = sample_theta_i_myars(x, theta0.row(i).t(), response.row(i).t(), inv_sigma, A0, d0).t();
    }
    // M step
    sigma0 = calcu_sigma_cmle_cpp(theta0);
    for(j=0;j<J;++j){
      rv(0) = j;
      cv = arma::find(Q.row(j));
      glm_res = my_Logistic_cpp(theta0.cols(cv), response.col(j), A0.submat(rv, cv), d0(j));
      A0.submat(rv, cv) = glm_res.subvec(0, cv.n_rows-1);
      d0(j) = glm_res(cv.n_rows);
    }
    res.row(mcn) = arma::join_cols(arma::join_cols(arma::vectorise(sigma0),arma::vectorise(A0)),d0).t();
  }
  return Rcpp::List::create(Rcpp::Named("res") = res,
                            Rcpp::Named("A0") = A0,
                            Rcpp::Named("d0") = d0,
                            Rcpp::Named("sigma0") = sigma0,
                            Rcpp::Named("theta0") = theta0);
}
// [[Rcpp::export]]
Rcpp::List stem_pcirtc(const arma::mat &response, const arma::mat &Q,  
                                    arma::mat A0, arma::mat D0, arma::mat theta0,
                                    arma::mat sigma0, int T, bool parallel){
  if(!parallel)
    omp_set_num_threads(1);
  else
    omp_set_num_threads(omp_get_num_procs());
  int N = response.n_rows;
  int J = response.n_cols;
  int K = A0.n_cols;
  int M = D0.n_cols;
  int mcn = 0;
  arma::vec x(3);
  x(0) = -1;
  x(1) = 0;
  x(2) = 1;
  // arma::vec x = arma::linspace(-4,4,9);
  arma::mat inv_sigma(K, K);
  arma::mat res = arma::zeros(T, K*K+J*K+J*M);
  // struct timeval t1, t2;
  // double elapsedTime;
  // clock_t tt;
  for(mcn=0;mcn<T;++mcn){
    Rprintf("\rStep  %d\t| ", mcn+1);
    Rcpp::checkUserInterrupt();
    // E step
    inv_sigma = arma::inv(sigma0);
    // tt = clock();
#pragma omp parallel for
    for(int i=0;i<N;++i){
      //Rprintf("i=%d\n",i);
      theta0.row(i) = sample_theta_i_myars_partial_credit(x, theta0.row(i).t(), response.row(i).t(), inv_sigma, A0, D0).t();
    }
    // Rprintf("sampling elaps time is:%f ms | ", (clock()-tt)*1000.0 / CLOCKS_PER_SEC);
    // M step
    // tt = clock();
    sigma0 = calcu_sigma_cmle_cpp(theta0);
    for(int j=0;j<J;++j){
      arma::uvec rv(1), cv;
      rv(0) = j;
      cv = arma::find(Q.row(j));
      arma::vec glm_res = my_Logistic_cpp_partial(theta0.cols(cv), response.col(j), A0.submat(rv, cv).t(), D0.row(j).t());
      A0.submat(rv, cv) = glm_res.subvec(0, cv.n_rows-1);
      D0.row(j) = glm_res.subvec(cv.n_rows, cv.n_rows+M-1).t();
    }
    // Rprintf("optimization elaps time is:%f ms", (clock()-tt)*1000.0 / CLOCKS_PER_SEC);
    res.row(mcn) = arma::join_cols(arma::join_cols(arma::vectorise(sigma0),arma::vectorise(A0)),arma::vectorise(D0)).t();
  }
  return Rcpp::List::create(Rcpp::Named("res") = res,
                            Rcpp::Named("A0") = A0,
                            Rcpp::Named("D0") = D0,
                            Rcpp::Named("sigma0") = sigma0,
                            Rcpp::Named("theta0") = theta0);
}
