// -*- mode: C++; c-indent-level: 2; c-basic-offset: 2; indent-tabs-mode: nil; -*-

#include <RcppArmadillo.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

using namespace Rcpp;

// Functions used to predict EM survival
double sob_lognormal(const double& t, const double& m, const double& sigma) {
  return R::pnorm((m -log(t))/sigma, 0, 1, true, false);
}

double sob_lognormal_mix(const double& t, const arma::rowvec& m, const arma::vec& sigma, const arma::vec& eta) {
  double res = 0.0;
  for (int i = 0; i < m.n_elem; i++) {
    res += eta(i) * sob_lognormal(t, m(i), sigma(i));
  }
  return res;
}

double hazard_lognormal_mix(const double& t, const arma::rowvec& m, const arma::vec& sigma, const arma::vec& eta) {
  double sob_mix = sob_lognormal_mix(t, m, sigma, eta);
  double dlnorm_mix = 0;

  for (int i = 0; i < m.n_elem; i++) {
    dlnorm_mix += eta(i) * R::dlnorm(t, m(i), sigma(i), false);
  }
  return dlnorm_mix/sob_mix;
}

// [[Rcpp::export]]
arma::vec predict_survival_em_cpp(const arma::vec& t, const arma::mat& m, const arma::vec& sigma, const arma::vec& eta, const int& r) {
  int n = t.n_elem;
  arma::vec out(t.n_elem);

  for(int i = 0; i < n; i++) {
    out(i) = sob_lognormal_mix(t(i), m.row(r - 1), sigma, eta);
  }
  return out;
}

// [[Rcpp::export]]
arma::vec predict_hazard_em_cpp(const arma::vec& t, const arma::mat& m, const arma::vec& sigma, const arma::vec& eta, const int& r) {
  int n = t.n_elem;
  arma::vec out(t.n_elem);

  for(int i = 0; i < n; i++) {
    out(i) = hazard_lognormal_mix(t(i), m.row(r - 1), sigma, eta);
  }
  return out;
}

// [[Rcpp::export]]
arma::mat predict_survival_gibbs_cpp(const arma::vec& eval_time, const arma::rowvec& predictors, const arma::field<arma::mat>& beta_start, const arma::mat sigma_start, const arma::mat eta_start,
                                     const bool& interval, const double& level) {
  arma::vec surv(eval_time.n_elem);
  arma::vec surv_low(eval_time.n_elem);
  arma::vec surv_high(eval_time.n_elem);
  
  int Niter = sigma_start.n_rows;
  int mixture_components = sigma_start.n_cols;
  double t;
  
  arma::vec levels = { 1.0 - level, level };
  arma::vec quantiles(2);
  
  arma::vec surv_pred(Niter);  
  arma::rowvec sigma;
  arma::rowvec eta;
  arma::mat beta(mixture_components, beta_start(0).n_cols);
  arma::mat beta_mat(Niter, beta_start(0).n_cols);
  arma::rowvec m;
  arma::mat eta_mat = arma::join_rows(eta_start,  arma::zeros<arma::vec>(eta_start.n_rows));
  
  for(int time = 0; time < eval_time.n_elem; time++) {
    t = eval_time(time);

    for(int i = 0; i < Niter; i++) {
      // completing the eta_matrix
      eta_mat(i, mixture_components - 1) = 1 - arma::sum(eta_start.row(i));
      
      for(int c = 0; c < mixture_components; c++) {
        beta_mat = beta_start(c);
        beta.row(c) = beta_mat.row(i);
      }
      
      m = predictors * beta.t();
      eta = eta_mat.row(i);
      sigma = sigma_start.row(i);
      
      surv_pred(i) = sob_lognormal_mix(t, m, sigma.t(), eta.t());
    }

    surv(time) = arma::mean(surv_pred);
    
    if(interval) {
      quantiles = arma::quantile(surv_pred, levels);
      
      surv_low(time) = quantiles(0);
      surv_high(time) = quantiles(1);
    }
  }

  arma::mat out;
  
  if(interval) {
    
    out = arma::join_rows(surv, arma::join_rows(surv_low, surv_high));
  } else {
    out = surv;
  }

  return out;
}

// [[Rcpp::export]]
arma::mat predict_hazard_gibbs_cpp(const arma::vec& eval_time, const arma::rowvec& predictors, const arma::field<arma::mat>& beta_start, const arma::mat sigma_start, const arma::mat eta_start,
                                     const bool& interval, const double& level) {
  arma::vec surv(eval_time.n_elem);
  arma::vec surv_low(eval_time.n_elem);
  arma::vec surv_high(eval_time.n_elem);
  
  int Niter = sigma_start.n_rows;
  int mixture_components = sigma_start.n_cols;
  double t;
  
  arma::vec levels = { 1.0 - level, level };
  arma::vec quantiles(2);
  
  arma::vec surv_pred(Niter);  
  arma::rowvec sigma;
  arma::rowvec eta;
  arma::mat beta(mixture_components, beta_start(0).n_cols);
  arma::mat beta_mat(Niter, beta_start(0).n_cols);
  arma::rowvec m;
  arma::mat eta_mat = arma::join_rows(eta_start,  arma::zeros<arma::vec>(eta_start.n_rows));
  
  for(int time = 0; time < eval_time.n_elem; time++) {
    t = eval_time(time);

    for(int i = 0; i < Niter; i++) {
      // completing the eta_matrix
      eta_mat(i, mixture_components - 1) = 1 - arma::sum(eta_start.row(i));
      
      for(int c = 0; c < mixture_components; c++) {
        beta_mat = beta_start(c);
        beta.row(c) = beta_mat.row(i);
      }
      
      m = predictors * beta.t();
      eta = eta_mat.row(i);
      sigma = sigma_start.row(i);
      
      surv_pred(i) = hazard_lognormal_mix(t, m, sigma.t(), eta.t());
    }

    surv(time) = arma::mean(surv_pred);
    
    if(interval) {
      quantiles = arma::quantile(surv_pred, levels);
      
      surv_low(time) = quantiles(0);
      surv_high(time) = quantiles(1);
    }
  }

  arma::mat out;
  
  if(interval) {
    
    out = arma::join_rows(surv, arma::join_rows(surv_low, surv_high));
  } else {
    out = surv;
  }

  return out;
}
