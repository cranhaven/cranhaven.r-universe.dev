#include <RcppArmadillo.h>
#include <stdio.h>
#include <R.h>
#include "lbfgs.h"
#include "my_Logistic.h"

// [[Rcpp::depends(RcppArmadillo)]]
double neg_loglik_logi(arma::mat XX, arma::vec YY, arma::vec beta, double d){
  arma::vec tmp = XX * beta + d;
  return (-arma::accu(tmp % YY) + arma::accu(log(1+exp(tmp)))) / YY.n_elem;
}
arma::vec neg_loglik_deri(arma::mat XX, arma::vec YY, arma::vec beta, double d){
  int K = XX.n_cols;
  arma::vec res = arma::zeros(K+1);
  arma::vec tmp = XX * beta + d;
  tmp = -YY + 1/(1+exp(-tmp));
  res.subvec(0,K-1) = XX.t() * tmp;
  res.subvec(K,K) = arma::accu(tmp);
  return res / YY.n_elem;
}
// [[Rcpp::export]]
double neg_loglik_logi_partial_credit(arma::mat theta, arma::vec response_j, arma::vec A_j, arma::vec D_j){
  int M = D_j.n_rows;
  int N = response_j.n_rows;
  arma::vec thetaA_j = theta * A_j;
  arma::mat tmp = thetaA_j * arma::linspace(0, M-1, M).t();
  tmp.each_row() += D_j.t();
  arma::vec tmp_res(N);
  for(int i=0;i<N;++i){
    tmp_res(i) = tmp(i, response_j(i)) - log(sum(exp(tmp.row(i))));
  }
  return -mean(tmp_res) - 0.5*arma::sum(arma::log(5 - arma::abs(D_j))) / N;
}
// [[Rcpp::export]]
arma::vec neg_loglik_deri_partial_credit(arma::mat theta, arma::vec response_j, arma::vec A_j, arma::vec D_j){
  int M = D_j.n_rows;
  int N = theta.n_rows;
  int K = theta.n_cols;
  arma::vec thetaA_j = theta * A_j;
  arma::mat tmp = thetaA_j * arma::linspace(0, M-1, M).t();
  arma::vec tmp1 = arma::zeros(N);
  arma::vec tmp2 = arma::zeros(M);
  arma::vec tmp_res = arma::zeros(K+M);
  tmp.each_row() += D_j.t();
  tmp = exp(tmp);
  tmp1 = response_j - sum( tmp.each_row() % arma::linspace(0, M-1, M).t(), 1 ) / sum( tmp, 1 );
  tmp_res.subvec(0,K-1) = -theta.t() * tmp1;
  for(int i=0;i<N;++i){
    tmp2(response_j(i)) += 1;
  }
  // tmp2 -= tmp.t() * (1 / sum( tmp, 1 ));
  // tmp2(0) = 0;
  tmp2 -= tmp.t() * (1 / sum( tmp, 1 ));
  tmp2 -= arma::sign(D_j) / (5.0 - arma::abs(D_j));
  tmp2(0) = 0;
  tmp_res.subvec(K, K+M-1) = -tmp2 * 0.5;
  return tmp_res / N;
}

static lbfgsfloatval_t evaluate(
    void *instance,
    const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g,
    const int n,
    const lbfgsfloatval_t step
)
{
  int i,j,k=0;
  
  lbfgsfloatval_t fx = 0.0;
  lbfgsfloatval_t *params = (lbfgsfloatval_t *)instance;
  const int N = params[k++];
  const int K = params[k++];
  
  arma::mat XX = arma::zeros(N,K);
  arma::vec YY = arma::zeros(N);
  arma::vec beta = arma::zeros(K);
  double d = 0;
  
  for(i=0;i<N;++i){
    for(j=0;j<K;++j){
      XX(i,j) = params[k++];
    }
  }
  for(i=0;i<N;++i){
    YY(i) = params[k++];
  }
  
  k=0;
  for(i=0;i<K;++i){
    beta(i) = x[k++];
  }
  d = x[k++];
  fx = neg_loglik_logi(XX, YY, beta, d);
  // Rprintf("fx= %f\n", fx);
  arma::vec g_res = neg_loglik_deri(XX, YY, beta, d);
  for(i=0;i<(K+1);++i){
    g[i] =  g_res(i);
  }
  return fx;
}
static lbfgsfloatval_t evaluate_partial_credit(
    void *instance,
    const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g,
    const int n,
    const lbfgsfloatval_t step
)
{
  int i,j,k=0;
  
  lbfgsfloatval_t fx = 0.0;
  lbfgsfloatval_t *params = (lbfgsfloatval_t *)instance;
  const int N = params[k++];
  const int K = params[k++];
  const int M = params[k++];
  
  arma::mat XX = arma::zeros(N,K);
  arma::vec YY = arma::zeros(N);
  arma::vec beta = arma::zeros(K);
  arma::vec D = arma::zeros(M);
  
  for(i=0;i<N;++i){
    for(j=0;j<K;++j){
      XX(i,j) = params[k++];
    }
  }
  for(i=0;i<N;++i){
    YY(i) = params[k++];
  }
  
  k=0;
  for(i=0;i<K;++i){
    beta(i) = x[k++];
  }
  for(i=0;i<M;++i){
    D(i) = x[k++];
  }
  
  fx = neg_loglik_logi_partial_credit(XX, YY, beta, D);
  // Rprintf("fx= %f\n", fx);
  arma::vec g_res = neg_loglik_deri_partial_credit(XX, YY, beta, D);
  for(i=0;i<(K+M);++i){
    g[i] =  g_res(i);
  }
  return fx;
}
// [[Rcpp::export]]
arma::vec my_Logistic_cpp(arma::mat XX,  arma::vec YY, arma::vec beta0, double d0){
  double ret = 0;
  const int N = XX.n_rows;
  const int K = XX.n_cols;
  int param_number = K+1;
  int i,j;
  lbfgsfloatval_t fx=0;
  lbfgsfloatval_t *x = lbfgs_malloc(param_number);
  lbfgs_parameter_t param;
  lbfgs_parameter_init(&param);
  //  printf("epsilon=%f\n", param.epsilon);
  lbfgsfloatval_t *params = lbfgs_malloc(N*(K+1)+20);
  /* Initialize the constant params. */
  int k=0;
  params[k++] = N;
  params[k++] = K;
  
  for(i=0;i<N;++i){
    for(j=0;j<K;++j){
      params[k++] = XX(i,j);
    }
  }
  for(i=0;i<N;++i){
    params[k++] = YY(i);
  }
  
  /* Initialize the variables. */
  k=0;
  for(i=0;i<K;++i){
    x[k++] = beta0(i);
  }
  x[k++] = d0;
  
  ret = lbfgs(param_number, x, &fx, evaluate, NULL, params, &param);
  
  k=0;
  for(i=0;i<K;++i){
    beta0(i) = x[k++];
  }
  d0 = x[k++];
  /* Report the result. */
  arma::vec return_res(K+1);
  return_res.subvec(0, K-1) = beta0;
  return_res(K) = d0;
  return return_res;
}
// [[Rcpp::export]]
arma::vec my_Logistic_cpp_partial(arma::mat XX,  arma::vec YY, arma::vec beta0, arma::vec D0){
  double ret = 0;
  const int N = XX.n_rows;
  const int K = beta0.n_elem;
  const int M = D0.n_elem;
  int param_number = K+M;
  int i,j;
  lbfgsfloatval_t fx=0;
  lbfgsfloatval_t *x = lbfgs_malloc(param_number);
  lbfgs_parameter_t param;
  lbfgs_parameter_init(&param);
  //  printf("epsilon=%f\n", param.epsilon);
  lbfgsfloatval_t *params = lbfgs_malloc(N*(K+1)+20);
  /* Initialize the constant params. */
  int k=0;
  params[k++] = N;
  params[k++] = K;
  params[k++] = M;
  
  for(i=0;i<N;++i){
    for(j=0;j<K;++j){
      params[k++] = XX(i,j);
    }
  }
  for(i=0;i<N;++i){
    params[k++] = YY(i);
  }
  
  /* Initialize the variables. */
  k=0;
  for(i=0;i<K;++i){
    x[k++] = beta0(i);
  }
  for(i=0;i<M;++i){
    x[k++] = D0(i);
  }
  
  ret = lbfgs(param_number, x, &fx, evaluate_partial_credit, NULL, params, &param);
  
  k=0;
  for(i=0;i<K;++i){
    beta0(i) = x[k++];
  }
  for(i=0;i<M;++i){
    D0(i) = x[k++];
  }
  /* Report the result. */
  arma::vec return_res(K+M);
  return_res.subvec(0, K-1) = beta0;
  return_res.subvec(K, K+M-1) = D0;
  return return_res;
}
