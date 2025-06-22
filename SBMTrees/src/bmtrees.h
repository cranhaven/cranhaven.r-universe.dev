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

#ifndef DIST_H_
#define DIST_H_
#include <RcppDist.h>
// [[Rcpp::depends(RcppArmadillo, RcppDist)]]
#endif

#ifndef UTILS_H_
#define UTILS_H_
#include "utils.h"
#endif

#ifndef RCPP_H_
#define RCPP_H_
#include <Rcpp.h>
#endif

#ifndef BART_M_H_
#define BART_M_H_
#include "bart_model.h"
#endif




#ifndef UPDATE_H_
#define UPDATE_H_
#include "DP_lambda.h"
#include "cal_random_effects.h"
#include "update_B.h"
#include "update_Covariance.h"
#include <cmath>
#endif

using namespace Rcpp;

class bmtrees{
public:
  bmtrees(NumericVector Y, NumericMatrix X, Nullable<NumericMatrix> Z, CharacterVector subject_id, IntegerVector row_id, bool binary = false, bool CDP_residual = false, bool CDP_re = false, double tol=1e-40, int ntrees = 200, int resample = 0, double pi_CDP = 0.99, bool train = true) {     // Constructor
    if(train){
      this->tol = tol;
      this->CDP_residual = CDP_residual;
      this->CDP_re = CDP_re;
      this->resample = resample;
      
      if(Z.isNull()){
        d = 1;
      }else{
        d = as<NumericMatrix>(Z).ncol();
      }
      this->binary = binary;
      this->Y_original = clone(Y);
      this->Y = clone(Y);
      this->X = clone(X);
      this->subject_id = subject_id;
      this->row_id = row_id;
      //this->sigma = s;
      
      
      
      N = Y.length();
      p = X.ncol();
      n_subject = unique(subject_id).length();
      alpha = 1 + NumericVector(n_subject);
      subject_to_B = create_subject_to_B(subject_id);
      n_obs_per_subject = max(table(subject_id));
      
      
      row_id_to_id = create_row_id_to_row(row_id);
      
      Z_mean = NumericVector(d);
      Z_sd = 1 + NumericVector(d);
      
      z = NumericMatrix(N, d);
      if(!Z.isNull()){
        NumericMatrix z0 = as<NumericMatrix>(Z);
        for(int i = 0; i < d; ++i){
          Z_mean[i] = mean(z0(_, i));
          Z_sd[i] = sd(z0(_, i));
          if(Z_sd[i] == 0){
            Z_sd[i] = 1;
            Z_mean[i] = 0;
          }
          z(_, i) = (z0(_, i) - Z_mean[i]) / Z_sd[i];
        }
      }
      
      //Rcout << z << std::endl;
      if(binary){
        NumericVector y_ = clone(this->Y);
        y_ = y_ * 2 - 1;
        this->Y = clone(y_);
        Y_mean = 0;
        Y_sd = 1;
      }else{
        Y_mean = mean(this->Y_original);
        /*Y_sd = sd(this->Y_original);
         
         if(Y_sd == 0){
         Y_sd = 1;
         }
         
         this->Y = (this->Y_original - Y_mean)/Y_sd;*/
        this->Y = this->Y_original - Y_mean;
        //this->Y = this->Y_original;
      }
      
      
      tau_samples = NumericVector(N);
      B_tau_samples = NumericMatrix(n_subject, d);
      Environment pkg_env = Environment::namespace_env("SBMTrees");
      Function get_inverse_wishart_matrix2 = pkg_env["get_inverse_wishart_matrix2"];
      List lmm = get_inverse_wishart_matrix2(this->X, this->Y, z, subject_id, subject_to_B);
      NumericMatrix coe = as<NumericMatrix>(lmm["coe"]);
      inverse_wishart_matrix = as<NumericMatrix>(lmm["covariance"]);
      if(CDP_re){
        M_re = pow(n_subject, (double)(runif(1, 0, 0.5)[0]));
        B_tau = DP(List::create(Named("p") = d, Named("cov") = as<NumericMatrix>(inverse_wishart_matrix)), M_re, sqrt(n_subject), n_subject, true);
        B_tau_samples = as<NumericMatrix>(B_tau["samples"]);
        Covariance = as<NumericMatrix>(B_tau["Sigma"]);
      }
      if(CDP_residual){
        M = pow(N, (double)(runif(1, 0, 0.5)[0]));
        if(binary){
          if(CDP_re)
            tau = DP(List::create(Named("p") = 1, Named("sd") = 1, Named("pi") = pi_CDP), M, sqrt(N), N, true);
          else
            tau = DP(List::create(Named("p") = 1, Named("sd") = 1, Named("pi") = pi_CDP), M, sqrt(N), N, true);
        }else{
          if(CDP_re)
            tau = DP(List::create(Named("p") = 1, Named("sd") = (double)lmm["sigma"], Named("pi") = pi_CDP), M, sqrt(N), N, true);
          else
            tau = DP(List::create(Named("p") = 1, Named("sd") = (double)lmm["sigma"], Named("pi") = pi_CDP), M, sqrt(N), N, true);
        }
        sigma = as<double>(tau["sigma"]);
        tau_samples = NumericVector(as<NumericMatrix>(tau["samples"])(_,0));
        //Rcout << "initialization" << std::endl;
        //Rcout << tau_samples << std::endl;
      }
      //
      if(!CDP_re)
        Covariance = inverse_wishart_matrix;
      B = NumericMatrix(n_subject, d);
      //B = coe;
      
      re = cal_random_effects(z, subject_id, B, subject_to_B);
      tree = new bart_model(this->X, this->Y - re - tau_samples, 100L, false, false, false,  ntrees);
      if(CDP_residual){
        tree -> update(sigma, 50, 50, 1, false, 10L);
      }else{
        tree -> update(50, 50, 1, false, 10L);
      }
      tree_pre = colMeans(tree->predict(this->X));
      if(CDP_re || CDP_residual)
        tree_pre_mean = mean(tree_pre);
      else
        tree_pre_mean = 0;
      tree_pre = tree_pre - tree_pre_mean;
    }
  }

  
  NumericVector get_Y(){
    return this->Y;
  }
  
  NumericVector get_re(){
    return this->re;
  }
  
  double get_tau_sigma(){
    return sigma;
  }
  
  
  void update_X_Y(NumericMatrix X, NumericVector Y){
    this->X = clone(X);
    this->Y_original = clone(Y);
    this->Y = clone(Y);
    if(binary){
      NumericVector y_ = clone(this->Y);
      y_ = y_ * 2 - 1;
      this->Y = clone(y_);
    }else{
      this->Y_mean = mean(this->Y_original);
      Y_sd = 1;//sd(this->Y_original);
      if(Y_sd == 0){
        Y_sd = 1;
      }
      this->Y = (this->Y_original - Y_mean);///Y_sd;
    }
  }
  
  void update_sigma(){
    
    NumericVector Y_hat = re + tau_samples + tree_pre;
    if(!binary){
      double rss = sum(pow(this->Y - Y_hat, 2));
      //double bias = mean(this->Y- Y_hat); 
      //Rcout << "rss:" << rss << std::endl;
      //Rcout << "bias:" << bias << std::endl;
      //sigma = sqrt((1.0/Rcpp::rgamma(1, (tree->get_nu() + N)/ 2, 2 / (tree->get_nu() * tree->get_lambda() + rss) ))[0]);
      //sigma = 0.05;//sqrt((1.0/Rcpp::rgamma(1, N/ 2, 2 / rss ))[0]);
      //sigma = tree -> get_sigma();
      if(!CDP_residual)
        sigma = tree -> get_invchi(N, rss);
      else
        sigma = tau["sigma"];
        //sigma = 1;
        //sigma = 0.5;
      
      
      
    }else{
      sigma = 1;
    }
    
    //Rcout << "sigma:" << sigma << std::endl;
  }
  
  void update_tree(){
    //Function update_tree = G["update_tree"];
    NumericVector Y_ = Y - re - tau_samples;
    
    tree->set_data(X, Y_);
    List tree_obj = tree -> update(sigma, 1, 1, 1, false, 1L);
    //Rcout << "123" << std::endl;
    tree_pre = tree_obj["yhat.train.mean"];
    if(CDP_re || CDP_residual)
      tree_pre_mean = mean(tree_pre);
    else
      tree_pre_mean = 0;
    tree_pre = tree_pre - tree_pre_mean;
    //Rcout << mean(tree_pre) << std::endl;
    //Rcout << "456" << std::endl;
    //sigma =  as<NumericVector>(tree_obj["sigma"])[0];
    //Rcout << "789" << std::endl;
    //Rcout << "sigma:" << sigma << std::endl;
  }
  
  List get_tree_training_data(){
    return List::create(Named("X") = X, Named("Y") = Y - re - tau_samples);
  }
  
  void set_tree(List tree){
    //this->tree = tree;
    tree_pre = tree["yhat.train.mean"];
    
    sigma = 1;
    if(!binary){
      sigma = tree["sigma.mean"];
    }
  }
  
  List get_tree(){
    return this->tree -> get_tree_object();;
  }
  
  List get_nCDP_residual_data(bool verbose = false){
    if(CDP_residual){
      if(verbose)
        Rcout << "update residual" << std::endl;
      NumericVector residual_tem = Y - re - tree_pre;
      NumericMatrix residual(N, 1, residual_tem.begin());
      if(verbose)
        Rcout << "update nDP residual" << std::endl;
      tau["sigma"] = sigma;
      return(List::create(Named("residual") = residual, Named("tau") = tau));
    }else{
      return(List::create());
    }
  }
  
  List get_CDP_re_data(bool verbose = false){
    if(CDP_re){
      if(verbose)
        Rcout << "update DP" << std::endl;
      B_tau["Sigma"] = Covariance;
      if(d == 1){
        B_tau["sigma"] = Covariance;
      }
      return(List::create(Named("B") = B, Named("B_tau") = B_tau));
    }else{
      return(List::create());
    }
  }
  
  void set_CDP_re_data(List B_tau){
    this->B_tau = B_tau;
    B_tau_samples = as<NumericMatrix>(B_tau["samples"]);
    M_re = B_tau["M"];
  }
  
  void update_all(bool verbose = false){
    if(verbose)
      Rcout << "update BART" << std::endl;
    
    update_tree();
    if(verbose)
      Rcout << "update residual" << std::endl;
    NumericVector residual_tem = Y - re - tree_pre;
    NumericMatrix residual(N, 1, residual_tem.begin());
    
    if(CDP_residual){
      //Rcout << "residual" << residual << std::endl;
      if(verbose)
        Rcout << "update DP residual" << std::endl;
      tau["sigma"] = sigma;
      //Rcout<< sigma << std::endl;
      //Rcout << residual;
      tau =  update_DP_normal(residual, tau, 0, 0.5);
      tau_samples = tau["samples"];
      M = tau["M"];
      sigma = tau["sigma"];
    }else{
      if(verbose)
        Rcout << "update sigma" << std::endl;
      update_sigma();
    }
    // //
    // //Rcout << 3 << std::endl;
    //Covariance = NumericMatrix::diag(d, 1);;
    // //Rcout << Covariance(0, 0) << std::endl;
    if(CDP_re){
      if(verbose)
        Rcout << "update DP random effects" << std::endl;
      B_tau["Sigma"] = Covariance;
      if(d == 1){
        B_tau["sigma"] = Covariance;
      }
      //return List::create(Named("B") = B, Named("B_tau") = B_tau);
      B_tau = update_DP_normal(B, B_tau, 0, 0.5);
      B_tau_samples = as<NumericMatrix>(B_tau["samples"]);
      M_re = B_tau["M"];
      Covariance = as<NumericMatrix>(B_tau["Sigma"]);
      //Rcout << max(abs(Covariance)) << " ";
    }else{
      Covariance = update_Covariance(clone(B), clone(B_tau_samples), inverse_wishart_matrix, d + 2, n_subject);
    }

    if(verbose)
      Rcout << "M_re:" << M_re << "  " << "M:" << M << std::endl;

    if(verbose)
      Rcout << "update B" << std::endl;
    //NumericVector Y_mis = Y - tau_samples - tree_pre;
    
    //Rcout << z << std::endl;
    //Rcout << B_tau_samples << std::endl;
    //Rcout << Covariance << std::endl;
    B = update_B(Y - tau_samples - tree_pre, z, subject_id, B_tau_samples, subject_to_B, Covariance, sigma);

    if(verbose)
      Rcout << "update random effects" << std::endl;
    re = cal_random_effects(z, subject_id, B, subject_to_B);
    //re_arma = cal_random_effects_arma(Z_arma, subject_id, B_arma, subject_to_B);

    if(binary){
      for(int i = 0; i < N; ++i){
        if(Y_original[i] == 0){
          NumericVector mean_y = rtruncnorm(1, tree_pre[i] + re[i] + tau_samples[i], sigma, R_NegInf, 0);
          Y[i] = mean_y[0];
        }else{
          NumericVector mean_y = rtruncnorm(1, tree_pre[i] + re [i] + tau_samples[i], sigma, 0, R_PosInf);
          Y[i] = mean_y[0];
        }
      }
      if(verbose)
        Rcout << "update probit outcome" << std::endl;
    }
    random_test = NumericVector(0);
    re_test = NumericVector(0);
  }
  
  
  List posterior_sampling(){
    return List::create(
      Named("tree") = tree->get_tree_object(),
      Named("M") = M,
      Named("M_re") = M_re,
      Named("sigma") = sigma,// * Y_sd,
      Named("alpha") = alpha,
      Named("Sigma") = Covariance,
      Named("B") = B,
      Named("tau_samples") = tau_samples,
      Named("B_tau_samples") = B_tau_samples,
      Named("re") = re,
      Named("tree_pre") = tree_pre + Y_mean,
      Named("y_predict") = tree_pre + re + tau_samples + Y_mean,//(tree_pre + re + tau_samples) * Y_sd + Y_mean,
      Named("tau") = tau,
      Named("B_tau") = B_tau,
      Named("tree_pre_mean") = tree_pre_mean
    );
  }
  
  NumericVector get_tau_samples(){
    return tau_samples;
  }
  
  NumericVector get_tau_mu(){
    return tau["y"];
  }
  
  NumericVector get_tau_pi(){
    return tau["pi"];
  }
  
  NumericVector get_B_tau_samples(){
    return B_tau_samples;
  }
  
  NumericVector get_B_tau_mu(){
    return B_tau["y"];
  }
  
  NumericVector get_B_tau_pi(){
    return B_tau["pi"];
  }
  
  NumericVector get_B_tau_lambda(){
    return B_tau["lambda"];
  }
  
  // NumericVector predict(NumericMatrix X_test, Nullable<NumericMatrix> Z_test, CharacterVector subject_id_test, IntegerVector row_id_test, bool keep_re = true){
  //   //Rcout << "predict into" <<std::endl;
  //   int n = X_test.nrow();
  //   NumericMatrix z_test = NumericMatrix(n, d);
  //   z_test(_, 0) = z_test(_, 0) + 1;
  //   if(!Z_test.isNull()){
  //     NumericMatrix z0 = as<NumericMatrix>(Z_test);
  //     for(int i = 1; i < d; ++i){
  //       //z_test(_, i) = (z0(_, i - 1));
  //       z_test(_, i) = (z0(_, i - 1) - Z_mean[i]) / Z_sd[i];
  //     }
  //   }
  //   
  //   NumericVector X_hat_test = colMeans(tree -> predict(X_test, false));
  //   if(keep_re & random_test.length() > 0){
  //     ;
  //   }else{
  //     re_test = cal_random_effects(z_test, subject_id_test, B, subject_to_B);
  //     NumericVector e(n);
  //     if(CDP_residual){
  //       NumericVector values = tau["y"];
  //       NumericVector pi = tau["pi"];
  //       for(int i = 0 ; i < n ; ++i){
  //         e[i] = sample(values, 1, false, pi)[0];
  //       }
  //     }
  //     
  //     random_test = re_test + e;
  //   }
  // 
  //   return Y_mean + X_hat_test + random_test;
  // } 
  // 
  NumericVector predict_expectation(NumericMatrix X_test, Nullable<NumericMatrix> Z_test, CharacterVector subject_id_test, IntegerVector row_id_test, bool keep_re = true){
    //Rcout << "predict into" <<std::endl;
    int n = X_test.nrow();
    NumericMatrix z_test = NumericMatrix(n, d);
    
    if(!Z_test.isNull()){
      NumericMatrix z0 = as<NumericMatrix>(Z_test);
      for(int i = 0; i < d; ++i){
        //z_test(_, i) = (z0(_, i - 1));
        z_test(_, i) = (z0(_, i) - Z_mean[i]) / Z_sd[i];
      }
    }
    NumericVector X_hat_test = colMeans(tree -> predict(X_test, false));
    X_hat_test = X_hat_test - tree_pre_mean;
    if(keep_re && random_test.length() > 0){
      ;
    }else{
      re_test = cal_random_effects(z_test, subject_id_test, B, subject_to_B);
      
      if(CDP_residual){
        NumericVector values = tau["y"];
        NumericVector pi = tau["pi"];
        for(int i = 0 ; i < n ; ++i){
          double e = 0;
          if(resample > 0){
            NumericVector loc = sample(values, resample, true, pi);
            for(int k = 0; k < resample; ++k){
              e += loc[k];
            }
            e = e / resample;
          }
          re_test[i] += e;
        }
      }
      random_test = re_test;
    }
    return Y_mean + X_hat_test + random_test;
  } 
  
  NumericVector predict_sample(NumericMatrix X_test, Nullable<NumericMatrix> Z_test, CharacterVector subject_id_test, IntegerVector row_id_test, bool keep_re = true){
    int n = X_test.nrow();
    NumericMatrix z_test = NumericMatrix(n, d);
    if(!Z_test.isNull()){
      NumericMatrix z0 = as<NumericMatrix>(Z_test);
      for(int i = 0; i < d; ++i){
        z_test(_, i) = (z0(_, i) - Z_mean[i]) / Z_sd[i];
      }
    }
    
    NumericVector X_hat = colMeans(tree -> predict(X_test, false));
    X_hat = X_hat - tree_pre_mean;
    if(keep_re && re_test.length() > 0){
      ;
    }else{
      re_test = cal_random_effects(z_test, subject_id_test, B, subject_to_B);
    }
    NumericVector e(n);
    if(resample == 0){
      e = rnorm(n);
      e = e * sigma;
      // if(CDP_residual){
      //   NumericVector values = tau["y"];
      //   NumericVector pi = tau["pi"];
      //   NumericVector loc = sample(values, 1, true, pi);
      //   for(int i = 0 ; i < n ; ++i){
      //     e[i] = R::rnorm(loc[i], sigma);
      //   }
      // }
      // if(CDP_residual){
      //   for(int i = 0 ; i < n ; ++i){
      //     auto it = row_id_to_id.find(std::to_string(row_id_test[i]));
      //     if (it != row_id_to_id.end()) {
      //       int pos = (it->second);
      //       e[i] += tau_samples[pos];
      //     }
      //     //e[i] = R::rnorm(e[i], sigma);
      //   }
      // }
    }else{
      if(CDP_residual){
        NumericVector values = tau["y"];
        NumericVector pi = tau["pi"];
        for(int i = 0 ; i < n ; ++i){
          NumericVector loc = sample(values, resample, true, pi);
          e[i] = 0;
          for(int k = 0; k < resample; ++k){
            e[i] += loc[k];
          }
          e[i] = e[i] / resample;
          e[i] = R::rnorm(e[i], sigma);
        }
      }
    }
    
    //e = rnorm(n);
    //e = e * sigma;
    NumericVector y_pre = Y_mean + X_hat + re_test;
    if(this->binary == true){
      y_pre = Rcpp::pnorm(y_pre);
      for(int i = 0 ; i < y_pre.length(); ++i){
         y_pre[i] = R::rbinom(1, y_pre[i]);
         //y_pre[i] = y_pre[i] >= 0;
      }
    }else{
      y_pre = y_pre + e;
    }
    return y_pre;
  } 
  
  // NumericVector predict_probability_log(NumericVector Y_test, NumericVector Mu_test){
  //   //Rcout << "predict into" <<std::endl;
  //   int n = Y_test.length();
  //   NumericVector prob(n);
  //   if(binary){
  //     
  //   }else{
  //     if(CDP_residual){
  //       NumericVector values = tau["y"];
  //       NumericVector pi = tau["pi"];
  //       for(int i = 0 ; i < n ; ++i){
  //         for(int j = 0 ; j < pi.length(); ++j){
  //           prob[i] += pi[j] * R::dnorm(Y_test[i], Mu_test[i] + values[j], sigma, true);
  //         }
  //       }
  //     }else{
  //       for(int i = 0 ; i < n ; ++i){
  //         prob[i] = R::dnorm(Y_test[i], Mu_test[i], sigma, true);
  //       }
  //     }
  //   }
  //   
  //   return prob;
  // } 
  // 
  double predict_probability_log(double Y_test, double Mu_test, int row_id_test){
    double prob = 0; 
    if(binary == false){
      //if(CDP_residual){
      if(false){
        NumericVector values = tau["y"];
        NumericVector pi = tau["pi"];
        for(int j = 0 ; j < pi.length(); ++j){
          auto it = row_id_to_id.find(std::to_string(row_id_test));
          if (it != row_id_to_id.end()) {
          //if (false) {
            int pos = (int)(it->second);
            prob = R::dnorm(Y_test, Mu_test + tau_samples[pos], sigma, true);
          }else{ 
            prob += pi[j] * R::dnorm(Y_test, Mu_test + values[j], sigma, true);
          }
          //prob += pi[j] * R::dnorm(Y_test, Mu_test + values[j], sigma, true);
        }
      }else{
        prob = R::dnorm(Y_test, Mu_test, sigma, true);
      }
    }else{
       //if(CDP_residual){
        if(false){
        NumericVector values = tau["y"];
        NumericVector pi = tau["pi"];
        for(int j = 0 ; j < pi.length(); ++j){
          auto it = row_id_to_id.find(std::to_string(row_id_test));
          if (it != row_id_to_id.end()) {
          //if (false) {
            prob = R::dbinom(Y_test, 1, R::pnorm(Mu_test, 0.0, 1.0, true, false), true);
          }else{
            prob += pi[j] * R::dbinom(Y_test, 1, R::pnorm(Mu_test + values[j], 0.0, 1.0, true, false), true);
          }
         // prob += pi[j] * R::dbinom(Y_test, 1, R::pnorm(Mu_test + values[j], 0.0, 1.0, true, false), true);
        }
      }else{
        prob = R::dbinom(Y_test, 1, R::pnorm(Mu_test, 0.0, 1.0, true, false), true);
        //prob = R::dnorm(Y_test, Mu_test, sigma, true);
      }
    }
    return prob;
  } 
  
  double predict_probability_log_expectation(double Y_test, double Mu_test){
    double prob = 0;
    if(binary == false){
      prob = R::dnorm(Y_test, Mu_test, sigma, true);
    }else{
      prob = R::dbinom(Y_test, 1, R::pnorm(Mu_test, 0.0, 1.0, true, false), true);
    }
    return prob;
  }
  
private:
  double tol;
  
  Environment G;
  
  int d;
  bool binary;
  NumericVector Y_original;
  NumericVector Y;
  NumericMatrix X;
  NumericMatrix z;
  CharacterVector subject_id;
  IntegerVector row_id;
  
  double Y_mean = 0;
  double Y_sd = 1;
  
  NumericVector Z_mean;
  NumericVector Z_sd;
  
  long N;
  int p;
  int n_subject;
  long n_obs_per_subject;
  int resample;
  
  std::unordered_map<std::string, int> subject_to_B;
  std::unordered_map<std::string, int> row_id_to_id;
  NumericMatrix inverse_wishart_matrix;
  NumericMatrix Covariance;
  NumericMatrix B;
  NumericVector alpha;
  
  double M_re = 0;
  double M = 0;
  double sigma = 1;
  List tau;
  List B_tau; 
  NumericVector tau_samples; 
  NumericMatrix B_tau_samples;
  
  bool CDP_residual;
  bool CDP_re;
  NumericVector re;
  arma::vec re_arma;
  
  //List tree;
  bart_model * tree;
  
  NumericVector tree_pre;
  NumericVector random_test;
  NumericVector re_test;
  
  double tree_pre_mean;
};

// // [[Rcpp::export]]
// List test(NumericVector Y, NumericMatrix X, NumericMatrix Z, CharacterVector subject_id, IntegerVector row_id, bool binary, bool nCDP_residual = false, bool CDP_re = false){
//   //Rcout << "123" << std::endl;
//   bmtrees a = bmtrees(Y, X, clone(Z), subject_id, row_id, binary, nCDP_residual, CDP_re);
//   a.update_all(true);
//   //a.update_all();
//   //a.update_all();
//   //Rcout << "456" << std::endl;
//   //a.update_tree();
//   //a.update_sigma();
//   //a.update(true);
//   List samples = a.posterior_sampling();
//   NumericVector expect_Y = as<NumericVector>(samples["tree_pre"]) + as<NumericVector>(samples["re"]);
//   return List::create(Named("sample") = a.posterior_sampling(), Named("X_hat_test") = a.predict(X, clone(Z), subject_id, row_id),Named("X_hat_test2") = a.predict(X, clone(Z), subject_id, row_id), Named("Y") = samples["y_predict"], Named("expect_Y") = expect_Y);
//   //return a.posterior_sampling();
//   //List post = a.posterior_sampling();
//   //NumericVector predict_y = a.predict(X, Z, subject_id);
//   //return a.posterior_sampling();
//   //return a.a_test;
//   //return List::create(Named("sample") = a.posterior_sampling(), Named("re") = a.get_re(), Named("samples") = a.get_tau_samples(), Named("Y") = a.get_Y());
// }
