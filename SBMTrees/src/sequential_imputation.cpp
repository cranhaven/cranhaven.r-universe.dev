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

#ifndef BMTREES_H_
#define BMTREES_H_
#include "bmtrees.h"
#endif

#ifndef UPDATE_H_
#define UPDATE_H_
#include <Rcpp.h>
#include "create_subject_to_B.h"
#include "DP.h"
#include "cal_random_effects.h"
#include "update_B.h"
#include "update_Covariance.h"
#include <cmath>
#endif

#include <vector>
#include <ctime>

// #ifdef _OPENMP
// #include <omp.h>
// #endif

// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>


using namespace Rcpp;





// [[Rcpp::export]]
List sequential_imputation_cpp(NumericMatrix X, NumericVector Y, LogicalVector type, NumericMatrix Z, CharacterVector subject_id, LogicalMatrix R, bool binary_outcome = false, int nburn = 0, int npost = 3, int skip = 1, bool verbose = true, bool CDP_residual = false, bool CDP_re = false, Nullable<long> seed = R_NilValue, double tol = 1e-20, int ncores = 0, int ntrees = 200, bool fit_loss = false, int resample = 0, double pi_CDP = 0.99) {
  //Rcpp::Environment base("package:base");
  //Rcpp::Environment G = Rcpp::Environment::global_env();
  
  int n = X.nrow();
  int p = X.cols();
  List imputation_X_DP = List::create();
  List imputation_Y_DP = List::create();
  int  skip_indicator = -1;
 
  std::vector<bmtrees> chain_collection; 
  bool outcome_is_missing = (sum(R(_, p)) != 0);
  
  if (outcome_is_missing){
    Rcout << "Outcome variable has missing values" << std::endl;
  }
  
  LogicalVector Y_miss_ind = R(_,p);
  LogicalVector Y_obs_ind = 1 - R(_,p);
  LogicalVector no_loss_ind;
  
  bool intercept = (sum(R(_, 0)) == 0) && (0 == sd(X(_, 0)));
  IntegerVector rowSums_R = rowSums_I(as<NumericMatrix>(R));
  if(intercept){
    if(fit_loss){
      no_loss_ind = (1 - (rowSums_R > p));
    }else{
      no_loss_ind = (1 - (rowSums_R == p));
    }
    
  }else{
    if(fit_loss){
      no_loss_ind = (1 - (rowSums_R > p - 1));
    }else{
      no_loss_ind = (1 - (rowSums_R == p - 1));
    }
  }
  CharacterVector X_names = colnames(X);
  if (true){
    Rcout << "Start initializing models" << std::endl;
    Rcout << std::endl;
  }
  Progress progin(p, !verbose);
  for(int i = 0; i < p; ++i){
    if (Progress::check_abort() )
      return -1.0;
    progin.increment();
    if(i == p - 1){
      // fit outcome model
      NumericVector Y_obs = Y[no_loss_ind];
      
      NumericMatrix X_obs = row_matrix(X, no_loss_ind);
      NumericMatrix Z_obs = row_matrix(Z, no_loss_ind);
      CharacterVector subject_id_obs = subject_id[no_loss_ind];
      IntegerVector row_id_obs = seqC(1, Y.length())[no_loss_ind];
      chain_collection.push_back(bmtrees(clone(Y_obs), clone(X_obs), clone(Z_obs), clone(subject_id_obs), clone(row_id_obs), binary_outcome, CDP_residual, CDP_re, tol, ntrees, resample, pi_CDP, true));
      break;
    }
    
    NumericMatrix X_t = X(_, Range(0,i));
    NumericMatrix X_train = row_matrix(X_t, no_loss_ind);
    NumericVector y_t = X(_, i + 1);
    NumericVector y_train = y_t[no_loss_ind];
    NumericMatrix Z_train = row_matrix(Z, no_loss_ind);
    CharacterVector subject_id_train = subject_id[no_loss_ind];
    IntegerVector row_id_obs = seqC(1, y_t.length())[no_loss_ind];
    chain_collection.push_back(bmtrees(clone(y_train), clone(X_train), clone(Z_train), clone(subject_id_train), clone(row_id_obs), type[i+1], CDP_residual, CDP_re, tol, ntrees, resample, pi_CDP, (sum(R(_, i + 1)) != 0)));
  }
  if (true){
    Rcout << std::endl;
    Rcout << "Complete initialization" << std::endl;
    Rcout << std::endl;
  }
  Progress progr(nburn + npost, !verbose);
  for (int step = 0; step < nburn + npost; ++step){
    if (Progress::check_abort() )
      return -1.0;
    progr.increment();
    if(step >= nburn){
      if (step == nburn)
        skip_indicator = 0;
      skip_indicator = skip_indicator + 1;
    }
    
    // start to update model
    if(verbose){
      Rcout << "*********************************************" << std::endl;
      Rcout << step + 1 << "/" << nburn + npost << std::endl;
      Rcout << "Start model training" << std::endl;
    }
    
    
    
    for(int i = 0; i < p; ++i){
      if(i == p - 1 ){
        NumericMatrix X_train = row_matrix(X, no_loss_ind);
        NumericVector y_train = Y[no_loss_ind];
        
        chain_collection[i].update_X_Y(clone(X_train), clone(y_train));
      }else{
        if(sum(R(_, i + 1)) != 0){
          NumericMatrix X_t = X(_, Range(0,i));
          NumericMatrix X_train = row_matrix(X_t, no_loss_ind);
          NumericVector y_t = X(_, i + 1);
          NumericVector y_train = y_t[no_loss_ind];
          
          chain_collection[i].update_X_Y(clone(X_train), clone(y_train));
        }
      }
    }
    if(verbose)
      Rcout << "single core" << std::endl;
    for(int i = 0; i < p; ++i){
      if(i == p - 1 ){
        if(verbose)
          Rcout << "fit outcome model" << std::endl;
        chain_collection[i].update_all(false);
      }else{
        if(sum(R(_, i + 1)) != 0){
          if(verbose)
            Rcout << "fit model for " << i + 1 + int(!intercept) << "th covariates" << std::endl;
          chain_collection[i].update_all(false);
        }
      }
    }

    if(verbose){
      Rcout << "Finish model training" << std::endl;
      Rcout << std::endl;
      Rcout << "Start imputation:" << std::endl;
    }
    NumericMatrix prob_collection_dom_log(n, p);
    NumericMatrix prob_collection_num_log_expectation(n, p);
    for(int i = 0 ; i < p ; ++i){
      if(i == p - 1){
        NumericVector y_predict_mu = chain_collection[i].predict_expectation(clone(X), clone(Z), clone(subject_id), seqC(1, Y.length()));
        for(int j = 0; j < n; ++j){
          if(R(j, i + 1)){
            prob_collection_dom_log(j, i) = chain_collection[i].predict_probability_log(Y[j], y_predict_mu[j], j);
            prob_collection_num_log_expectation(j, i) = chain_collection[i].predict_probability_log_expectation(Y[j], y_predict_mu[j]);
          }
        }
        break;
      }
      if(sum(R(_, i + 1)) == 0){
        continue;
      }
      NumericMatrix X_train = X(_, Range(0,i));
      NumericVector y_train = X(_, i + 1);
      NumericVector y_predict_mu = chain_collection[i].predict_expectation(clone(X_train), clone(Z), clone(subject_id), seqC(1, Y.length()));

      for(int j = 0; j < n; ++j){
        if(R(j, i + 1)){
          prob_collection_dom_log(j, i) = chain_collection[i].predict_probability_log(y_train[j], y_predict_mu[j], j);
          prob_collection_num_log_expectation(j, i) = chain_collection[i].predict_probability_log_expectation(y_train[j], y_predict_mu[j]);
        }
      }
    }
    // 
    // 
    // imputation propose
    for (int i = 0; i < p - 1; ++i) {
      if(verbose){
        std::string blank(30 - as<std::string>(X_names[i+1]).length(), ' ');
        Rcout << X_names[i+1] << blank;
      }
      if(sum(R(_, i + 1)) == 0){
        if(verbose)
          Rcout << "No missing data." << std::endl;
        continue;
      }

      NumericMatrix prob_collection_num_log(n, p);
      NumericMatrix prob_collection_dom_log_expectation(n, p);
      NumericMatrix X_train = X(_, Range(0,i));
      NumericVector y_train = X(_, i + 1);
      // sample new value
      NumericVector new_y_train(Y.length());
      if(type[i + 1] == 0)
        new_y_train = chain_collection[i].predict_sample(X_train, Z, subject_id, seqC(1, Y.length()));
      else
        new_y_train = new_y_train + 1;
      NumericVector y_predict_mu = chain_collection[i].predict_expectation(X_train, Z, subject_id, seqC(1, Y.length()));
      for(int j = 0; j < n; ++j){
        prob_collection_num_log(j,i) = chain_collection[i].predict_probability_log(new_y_train[j], y_predict_mu[j], j);
        prob_collection_dom_log_expectation(j,i) = chain_collection[i].predict_probability_log_expectation(new_y_train[j], y_predict_mu[j]);
      }
      for(int j = i + 2; j < p; ++j){
        if (j >= p){
          break;
        }
        NumericMatrix X_predict;
        if (j == i + 2){
          NumericMatrix X_predict_1 = X(_, Range(0, i));
          X_predict = cbind(X_predict_1, new_y_train);
        }else{
          NumericMatrix X_predict_1 = X(_, Range(0, i));
          NumericMatrix X_predict_2 = X(_, Range(i+2, j-1));
          X_predict = cbind(X_predict_1, new_y_train, X_predict_2);
        }
        NumericVector y_predict_mu = chain_collection[j - 1].predict_expectation(X_predict, Z, subject_id, seqC(1, Y.length()));


        for(int k = 0; k < n; ++k){
          if(R(k, j)){
            prob_collection_num_log(k, j - 1) = chain_collection[j - 1].predict_probability_log(X(k,j), y_predict_mu[k], k);
          }
        }
      }
      NumericMatrix X_predict;
      if (i + 2 < p){
        NumericMatrix X_predict_1 = X(_, Range(0, i));
        NumericMatrix X_predict_2 = X(_, Range(i+2, p-1));
        X_predict = cbind(X_predict_1, new_y_train, X_predict_2);
      }else{
        NumericMatrix X_predict_1 = X(_, Range(0, i));
        X_predict = cbind(X_predict_1, new_y_train);
      }
      y_predict_mu = chain_collection[p - 1].predict_expectation(X_predict, Z, subject_id, seqC(1, Y.length()));
      for(int k = 0; k < n; ++k){
        if(R(k, p)){
          prob_collection_num_log(k, p - 1) = chain_collection[p - 1].predict_probability_log(Y[k], y_predict_mu[k], k);
        }
      }
      int missing = 0;
      int replace = 0;
      for(int k = 0 ; k < n; ++k){
        if (R(k, i + 1) == 1){
          if(type[i + 1] == 0){
            missing++;
            NumericVector num_log = prob_collection_num_log(k, _);// Rcpp::Range(i + 1, p - 1));
            NumericVector dom_log = prob_collection_dom_log(k, _);// Rcpp::Range(i + 1, p - 1));
            num_log = num_log[Range(i, p - 1)];
            dom_log = dom_log[Range(i, p - 1)];
            double log_accept = sum(num_log) - sum(dom_log) + prob_collection_num_log_expectation(k, i) - prob_collection_dom_log_expectation(k, i);// + prob_collection_num_log(k, i) - prob_collection_dom_log(k, i);
            if(log(runif(1)[0]) < log_accept){
              replace++;
              X(k, i + 1) = new_y_train[k];
            }
          }else{
            missing++;
            NumericVector num_log = prob_collection_num_log(k, _);
            num_log = exp(num_log[Range(i, p - 1)]);
            NumericVector zero_num_log = 1 - num_log;
            double accept_p = sum(num_log) / (sum(num_log) + sum(zero_num_log));
            int previous = X(k, i + 1);
            X(k, i + 1) = R::rbinom(1, accept_p);
            if(previous != X(k, i + 1)){
              replace++;
            }
          }
        }
      }
      double ar = replace;
      ar = ar / missing;
      if(verbose)
        Rcout << "Replace proportion:" << ar << std::endl;
    }
    if(outcome_is_missing){
      NumericVector new_y_train = chain_collection[p - 1].predict_sample(X, Z, subject_id, seqC(1, Y.length()));  // this is conditional expectation E(Y|X, Z)
      //NumericVector y_predict_mu = chain_collection[p - 1].predict_expectation(X, Z, subject_id, seqC(1, Y.length()));
      //NumericVector prob_collection_dom_log_expectation_y(n);
      //NumericVector prob_collection_num_log_y(n);
      int missing = 0;
      int replace = 0;
      for(int k = 0 ; k < n; ++k){
        if (R(k, p) == 1){
          //prob_collection_num_log_y[k] = chain_collection[p - 1].predict_probability_log(new_y_train[k], y_predict_mu[k], k);
          //prob_collection_dom_log_expectation_y[k] = chain_collection[p - 1].predict_probability_log_expectation(new_y_train[k], y_predict_mu[k]);
          missing++;
          //double num_log_y = prob_collection_num_log_y[k];
          //double dom_log_y = prob_collection_dom_log(k, p - 1);
          //double log_accept = 1*(num_log_y - dom_log_y) + prob_collection_num_log_expectation(k, p - 1) - prob_collection_dom_log_expectation_y[k];
          //if(log(runif(1)[0]) < log_accept){
          replace++;
          Y[k] = new_y_train[k];
          //}
        }
      }
      double ar = replace;
      ar = ar / missing;
      if(verbose){
        std::string blank(23, ' ');
        Rcout << "OUTCOME" << blank;
        Rcout << "Replace proportion:" << ar << std::endl;
      }
    }
    if (skip_indicator == skip){
      imputation_X_DP.push_back(clone(X));
      imputation_Y_DP.push_back(clone(Y));
      skip_indicator = 0;
    }
  }

  return List::create(
    Named("imputation_X_DP")=imputation_X_DP, Named("imputation_Y_DP")=imputation_Y_DP
  );
}








// [[Rcpp::export]]
List BMTrees_mcmc(NumericMatrix X, NumericVector Y, Nullable<NumericMatrix> Z, CharacterVector subject_id, LogicalVector obs_ind, bool binary = false, long nburn = 0, long npost = 3, bool verbose = true, bool CDP_residual = false, bool CDP_re = false, Nullable<long> seed = R_NilValue, double tol = 1e-40, long ntrees = 200, int resample = 0, double pi_CDP = 0.99){
  NumericMatrix Z_obs;
  NumericMatrix Z_test;
  NumericVector Y_obs = Y[obs_ind];
  NumericMatrix X_obs = row_matrix(X, obs_ind);
  if(!Z.isNull()){
    NumericMatrix z = as<NumericMatrix>(Z);
    Z_obs = row_matrix(z, obs_ind);
    Z_test = row_matrix(z, !obs_ind);
  }
  
  CharacterVector subject_id_obs = subject_id[obs_ind];
  IntegerVector row_id_obs = seqC(1, Y.length())[obs_ind];
  bmtrees model = bmtrees(clone(Y_obs), clone(X_obs), clone(Z_obs), subject_id_obs, row_id_obs, binary, CDP_residual, CDP_re, tol, ntrees, resample, pi_CDP);
  
  NumericVector Y_test = Y[!obs_ind];
  NumericMatrix X_test = row_matrix(X, !obs_ind);
  
  CharacterVector subject_id_test = subject_id[!obs_ind];
  IntegerVector row_id_test = seqC(1, Y.length())[!obs_ind];
  
  int d;
  if(Z.isNull()){
    d = 1;
  }else{
    d = as<NumericMatrix>(Z).ncol();
  }
  
  long N = Y_obs.length();
  long N_test = Y_test.length();
  int n_subject = unique(subject_id_obs).length();
  
  
  List post_trees;
  NumericMatrix post_tree_pre_mean(npost, 1);
  NumericMatrix post_M(npost, 1);
  NumericMatrix post_M_re(npost, 1);
  NumericMatrix post_alpha(npost, n_subject);
  NumericMatrix post_x_hat(npost, N);
  NumericMatrix post_sigma(npost, 1);
  NumericMatrix post_B_lambda(npost, 1);
  NumericMatrix post_lambda(npost, 1);
  NumericMatrix post_Sigma(npost, d * d);
  NumericMatrix post_B(npost, n_subject * d);
  NumericMatrix post_tau_samples(npost, N);
  NumericMatrix post_B_tau_samples(npost, n_subject * d);
  NumericMatrix post_random_effect(npost, N);
  NumericMatrix post_y_predict(npost, N);
  NumericMatrix post_y_predict_new(npost, N);
  NumericMatrix post_y_expectation(npost, N);
  NumericMatrix post_y_sample(npost, N);
  NumericMatrix post_tau_position(npost, (int)sqrt(N));
  NumericMatrix post_tau_pi(npost, (int)sqrt(N));
  NumericMatrix post_B_tau_pi(npost, (int)sqrt(n_subject));
  NumericMatrix post_B_tau_position(npost, (int)sqrt(n_subject) * d);
  
  NumericMatrix post_y_expectation_test(npost, N_test);
  NumericMatrix post_y_sample_test(npost, N_test);
  
  List tau;
  List B_tau;
  Progress progr(nburn + npost, !verbose);
  for(int i = 0 ; i < nburn + npost; ++i){
    if(verbose){
      Rcout << std::endl;
      Rcout << "*********************************************" << std::endl;
    }
    if (Progress::check_abort() )
      return -1.0;
    progr.increment();
    
    model.update_all(verbose);
    
    if(verbose)
      Rcout << i << " " << nburn + npost << std::endl;
    List post_sample = model.posterior_sampling();
    if(i >= nburn){
      post_tree_pre_mean(i - nburn, 0) = post_sample["tree_pre_mean"];
      post_sigma(i - nburn, 0) = post_sample["sigma"];
      post_x_hat(i - nburn, _) = as<NumericVector>(post_sample["tree_pre"]);
      post_Sigma(i - nburn, _) = as<NumericVector>(post_sample["Sigma"]);
      post_B(i - nburn, _) = as<NumericVector>(post_sample["B"]);
      post_random_effect(i - nburn, _) = as<NumericVector>(post_sample["re"]);
      post_y_expectation(i - nburn, _) = model.predict_expectation(clone(X_obs), clone(Z_obs), subject_id_obs, row_id_obs);
      post_y_sample(i - nburn, _) = model.predict_sample(clone(X_obs), clone(Z_obs), subject_id_obs, row_id_obs);
      
      
      post_y_expectation_test(i - nburn, _) = model.predict_expectation(clone(X_test), clone(Z_test), subject_id_test, row_id_test);
      post_y_sample_test(i - nburn, _) = model.predict_sample(clone(X_test), clone(Z_test), subject_id_test, row_id_test);
      
      if(CDP_residual){
        tau = post_sample["tau"];
        post_tau_position(i - nburn, _) = as<NumericVector>(tau["y"]);
        post_tau_pi(i - nburn, _) = as<NumericVector>(tau["pi"]);
        post_lambda(i - nburn, 0) = (double)tau["lambda"];
        post_M(i - nburn, 0) = (double)tau["M"];
      }
      if (CDP_re){
        B_tau = post_sample["B_tau"];
        post_B_tau_pi(i - nburn, _) = as<NumericVector>(B_tau["pi"]);
        post_B_tau_position(i - nburn, _) = as<NumericVector>(B_tau["y"]);
        post_B_lambda(i - nburn, 0) = (double)B_tau["lambda"];
        post_M_re(i - nburn, 0) = (double)B_tau["M"];
      }
      post_tau_samples(i - nburn, _) = as<NumericVector>(post_sample["tau_samples"]);
      post_B_tau_samples(i - nburn, _) = as<NumericVector>(post_sample["B_tau_samples"]);
    }
    
    if(verbose){
      Rcout << "*********************************************" << std::endl;
      Rcout << std::endl;
    }
  }
  return List::create(
    Named("post_x_hat") = post_x_hat,
    Named("post_Sigma") = post_Sigma,
    Named("post_lambda") = post_lambda,
    Named("post_B_lambda") = post_B_lambda,
    Named("post_B") = post_B,
    Named("post_M") = post_M,
    Named("post_M_re") = post_M_re,
    Named("post_random_effect") = post_random_effect,
    
    Named("post_tau_position") = post_tau_position,
    Named("post_tau_pi") = post_tau_pi,
    Named("post_tau_samples") = post_tau_samples,
    
    Named("post_B_tau_position") = post_B_tau_position,
    Named("post_B_tau_pi") = post_B_tau_pi,
    Named("post_B_tau_samples") = post_B_tau_samples,
    
    Named("post_sigma") = post_sigma,
    Named("post_y_expectation") = post_y_expectation,
    Named("post_y_sample") = post_y_sample,
    Named("post_y_expectation_test") = post_y_expectation_test,
    Named("post_y_sample_test") = post_y_sample_test
  );
  
}
