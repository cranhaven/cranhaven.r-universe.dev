/*
 * Functions for logs of full conditional pdfs and their derivatives
 */

#include <R.h>
#include <Rmath.h>
#include <stdio.h>

#include "structures.h"
#include "my_math.h"

/*
 * BETA_CAT - ALL betas for one outcome and one cluster at once
 */

void logpYcat(struct str_state* last,  // IN last known values of generated parameters
              double* Y,                // IN [*N]
              double* X,                // IN [N*(#regr)]    regressors
              int* spec,            // [5]                class-specific parameters
              // order:   [0] prec_num, 
              //          [1] c_ord,
              //          [2] InvSigma, 
              //          [3] InvQ, 
              //          [4] naY
              int* dims,            // [33]:              what parameters are saved
              // order:   [0-1]   beta_num_fix, beta_num,
              //          [2-4]   prec_num, sd_num, var_num,
              //          [5-6]   beta_poi_fix, beta_poi,
              //          [7-8]   beta_bin_fix, beta_bin,
              //          [9-10]  beta_ord_fix, beta_ord,
              //          [11-13] c_ord, a_ord, pi_ord,
              //          [14-15] beta_cat_fix, beta_cat,
              //          [16-20] InvSigma, Sigma, sdSigma, corSigma, detInvSigma,
              //          [21-23] InvQ, Q, detInvQ,
              //          [24]    b,
              //          [25-26] w, ng,
              //          [27-29] loglik, pUig, U,
              //          [30,31] Gplus, e0,
              //          [32]    naY
              double* predictor_num,
              double* predictor_poi,
              double* predictor_bin,
              double* predictor_ord,
              double* predictor_cat,
              int* pred_skip,         // IN [1] Which part of predictor is to be skipped
              int* N,                 // IN [1] number of observations
              int* n,                 // IN [1] number of subjects
              int* g,                 // IN [1] *G will be passed for FIXED, g for group-specific data
              int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
              int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
              int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
              //        FALSE = all levels of cat var have one set of bi - compares last category with others
              int* nfix,              // IN [1] number of FIXED  regressors or each response
              int* cumnfix,           // IN [1] where to start in FormulaF
              int* totnfix,           // IN [1] total dimension of fixed parameter (useless)
              int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
              int* n_i,               // IN [n] number of observations dedicated to j-th subject
              int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
              int* nUg,               // IN [G] number of subjects in classes 1, ..., G
              int* listUi,            // IN [n*G] row by row indices of subjects in group g
              double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
              double* value           // OUT [1]
){
  // Auxiliary variables
  int i, j, k, l, m;
  int coord;
  double* eta;
  eta = (double*)malloc(*Kcat * sizeof(double));
  //double eta[*Kcat];
  double maxeta;
  //double exp_eta[*ncat];
  double log_sum_exp;
  
  *value = 0.0;
  
  for(l = 0; l < nUg[*g]; l++){
    // just those in g-th cluster (if *g == G), it is across all of them
    i = listUi[l + *n * *g];
    for(j = 0; j < n_i[i]; j++){
      coord = i_j[i + *n * j]; // index of j-th observation of i-th individual within g-th cluster
      
      maxeta = 0.0;
      for(k = 0; k < *Kcat; k++){
        eta[k] = 0.0;
        // New fixed/group-specific predictor part using theta
        for(m = 0; m < *nfix; m++){
          eta[k] += theta[m + *nfix * k] * X[coord + *N * FormulaF[m + *cumnfix]];
        }
        // Add other parts: G/F, R, O
        for(m = 0; m < 4; m++){
          if(*pred_skip != m){
            eta[k] += predictor_cat[m + 4*(coord + *N * k)];
          }
        }
        
        if(maxeta < eta[k]){
          maxeta = eta[k];
        }
        //fflush(stdout);
        //exp_eta[k] = exp(eta[k]);
        //log_sum_exp += exp_eta[k];
        //log_sum_exp += exp(eta[k]);
      }
      //log_sum_exp = log(log_sum_exp);
      log_sum_exp = exp(-maxeta);
      for(k = 0; k < *Kcat; k++){
        if(Y[coord] == k){
          *value += eta[k]; // if one of first K-1 categories, add corresponding predictor
        }
        log_sum_exp += exp(eta[k] - maxeta);
      }
      log_sum_exp = log(log_sum_exp);
      log_sum_exp += maxeta;
      
      // in ALL cases, even Y==K-1 (last category), subtract log_sum_exp
      //  *value -= log_sum_exp;
      *value -= log_sum_exp;
    }
  }
  
  free(eta);
  
}

void logpbeta_cat(struct str_state* last,  // IN last known values of generated parameters
                  double* sd_beta,          // hyperparameter - std. dev. of prior for beta
                  double* Y,                // IN [*N]
                  double* X,                // IN [N*(#regr)]    regressors
                  int* spec,            // [5]                class-specific parameters
                  // order:   [0] prec_num, 
                  //          [1] c_ord,
                  //          [2] InvSigma, 
                  //          [3] InvQ, 
                  //          [4] naY
                  int* dims,            // [33]:              what parameters are saved
                  // order:   [0-1]   beta_num_fix, beta_num,
                  //          [2-4]   prec_num, sd_num, var_num,
                  //          [5-6]   beta_poi_fix, beta_poi,
                  //          [7-8]   beta_bin_fix, beta_bin,
                  //          [9-10]  beta_ord_fix, beta_ord,
                  //          [11-13] c_ord, a_ord, pi_ord,
                  //          [14-15] beta_cat_fix, beta_cat,
                  //          [16-20] InvSigma, Sigma, sdSigma, corSigma, detInvSigma,
                  //          [21-23] InvQ, Q, detInvQ,
                  //          [24]    b,
                  //          [25-26] w, ng,
                  //          [27-29] loglik, pUig, U,
                  //          [30,31] Gplus, e0,
                  //          [32]    naY
                  double* predictor_num,
                  double* predictor_poi,
                  double* predictor_bin,
                  double* predictor_ord,
                  double* predictor_cat,
                  int* pred_skip,         // IN [1] Which part of predictor is to be skipped
                  int* N,                 // IN [1] number of observations
                  int* n,                 // IN [1] number of subjects
                  int* g,                 // IN [1] *G will be passed for FIXED, g for group-specific data
                  int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                  int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                  int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                  //        FALSE = all levels of cat var have one set of bi - compares last category with others
                  int* nfix,              // IN [1] number of FIXED  regressors or each response
                  int* cumnfix,           // IN [1] where to start in FormulaF
                  int* totnfix,           // IN [1] total dimension of fixed parameter (useless)
                  int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                  int* n_i,               // IN [n] number of observations dedicated to j-th subject
                  int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                  int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                  int* listUi,            // IN [n*G] row by row indices of subjects in group g
                  double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
                  double* value           // OUT [1]
){
  // Auxiliary variables
  //double loglik;
  double x;
  int k;
  
  logpYcat(last, Y, X, spec, dims, 
           predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
           N, n, g, Kord, Kcat, kspec_bi_cat, 
           nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, theta, value);
  
  
  //*value = loglik;
  for(k = 0; k < (*nfix * *Kcat); k++){
    x = theta[k] / (*sd_beta);
    *value -= 0.5 * x * x;
  }
  
}

void d_d2_logpbeta_cat(struct str_state* last,  // IN last known values of generated parameters
                       double* sd_beta,          // hyperparameter - std. dev. of prior for beta
                       double* Y,                // IN [*N]
                       double* X,                // IN [N*(#regr)]    regressors
                       int* spec,            // [5]                class-specific parameters
                       // order:   [0] prec_num, 
                       //          [1] c_ord,
                       //          [2] InvSigma, 
                       //          [3] InvQ, 
                       //          [4] naY
                       int* dims,            // [33]:              what parameters are saved
                       // order:   [0-1]   beta_num_fix, beta_num,
                       //          [2-4]   prec_num, sd_num, var_num,
                       //          [5-6]   beta_poi_fix, beta_poi,
                       //          [7-8]   beta_bin_fix, beta_bin,
                       //          [9-10]  beta_ord_fix, beta_ord,
                       //          [11-13] c_ord, a_ord, pi_ord,
                       //          [14-15] beta_cat_fix, beta_cat,
                       //          [16-20] InvSigma, Sigma, sdSigma, corSigma, detInvSigma,
                       //          [21-23] InvQ, Q, detInvQ,
                       //          [24]    b,
                       //          [25-26] w, ng,
                       //          [27-29] loglik, pUig, U,
                       //          [30,31] Gplus, e0,
                       //          [32]    naY
                       double* predictor_num,
                       double* predictor_poi,
                       double* predictor_bin,
                       double* predictor_ord,
                       double* predictor_cat,
                       int* pred_skip,         // IN [1] Which part of predictor is to be skipped
                       int* N,                 // IN [1] number of observations
                       int* n,                 // IN [1] number of subjects
                       int* g,                 // IN [1] *G will be passed for FIXED, g for group-specific data
                       int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                       int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                       int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                       //        FALSE = all levels of cat var have one set of bi - compares last category with others
                       int* nfix,              // IN [1] number of FIXED  regressors or each response
                       int* cumnfix,           // IN [1] where to start in FormulaF
                       int* totnfix,           // IN [1] total dimension of fixed parameter (useless)
                       int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                       int* n_i,               // IN [n] number of observations dedicated to j-th subject
                       int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                       int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                       int* listUi,            // IN [n*G] row by row indices of subjects in group g
                       double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
                       double* grad,           // OUT [dim(theta)]
                       double* hess            // OUT [dim(theta) * (dim(theta)+1)/2]
){
  // Auxiliary variables
  int i, j, k, k1, k2, l, m, m1, m2, d1, d2;
  int coord;
  double* eta;
  eta = (double*)malloc(*Kcat * sizeof(double));
  //double eta[*Kcat];
  double maxeta;
  double* exp_eta;
  exp_eta = (double*)malloc(*Kcat * sizeof(double));
  //double exp_eta[*Kcat];
  double* softmax;
  softmax = (double*)malloc(*Kcat * sizeof(double));
  //double softmax[*Kcat];
  double sum_exp;
  double hess_coef, grad_coef;
  double auxX;
  
  // start with prior distribution contribution
  for(k1 = 0; k1 < (*nfix * *Kcat); k1++){
    // gradient
    grad[k1] = -theta[k1] / (*sd_beta * *sd_beta );
    // Hess diagonal
    k = k1 * (k1+3) /2;
    hess[k] = 1.0 / (*sd_beta * *sd_beta );
    // Hess off-diagonal
    for(k2 = k1+1; k2 < (*nfix * *Kcat); k2++){
      k = k1 + k2*(k2+1)/2;
      hess[k] = 0.0;
    }
  }
  
  for(l = 0; l < nUg[*g]; l++){
    // just those in g-th cluster (if g == G*), it is across all of them
    i = listUi[l + *n * *g];
    for(j = 0; j < n_i[i]; j++){
      coord = i_j[i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
      
      maxeta = 0.0;
      for(k = 0; k < *Kcat; k++){
        eta[k] = 0.0;
        // New fixed/group-specific predictor part using theta
        for(m = 0; m < *nfix; m++){
          eta[k] += theta[m + *nfix * k] * X[coord + *N * FormulaF[m + *cumnfix]];
        }
        // Add other parts: G/F, R, O
        for(m = 0; m < 4; m++){
          if(*pred_skip != m){
            eta[k] += predictor_cat[m + 4*(coord + *N * k)];
          }
        }
        //fflush(stdout);
        if(maxeta < eta[k]){
          maxeta = eta[k];
        }
        exp_eta[k] = exp(eta[k]);
        sum_exp += exp_eta[k];
      }
      
      sum_exp = exp(-maxeta);
      for(k = 0; k < *Kcat; k++){
        exp_eta[k] = exp(eta[k] - maxeta);
        sum_exp += exp_eta[k];
      }
      
      // softmax computation
      for(k = 0; k < *Kcat; k++){
        softmax[k] = exp_eta[k]/sum_exp;
      }
      
      for(k1 = 0; k1 < *Kcat; k1++){
        // gradient coefficient
        grad_coef = (Y[coord] == k1) - softmax[k1];
        // Hess matrix diagonal k1=k2 coefficient
        hess_coef = softmax[k1] * (1-softmax[k1]);
        
        for(m1 = 0; m1 <*nfix; m1++){
          // gradient
          auxX = X[coord + *N * FormulaF[m1 + *cumnfix]];
          grad[m1 + *nfix * k1] += grad_coef * auxX;
          
          // Hess matrix diagonal
          for(m2 = m1; m2 < *nfix; m2++){
            d1 = *nfix * k1 + m1;
            d2 = *nfix * k1 + m2;
            k = d1 + d2*(d2+1)/2;
            hess[k] += hess_coef * auxX * X[coord + *N * FormulaF[m2 + *cumnfix]];
            // its positive on the diagonal --> +
          }
        }
        
        // now off-diagonal
        for(k2 = k1+1; k2 < *Kcat; k2++){
          hess_coef = softmax[k1] * softmax[k2];
          for(m1 = 0; m1 <*nfix; m1++){
            auxX = X[coord + *N * FormulaF[m1 + *cumnfix]];
            
            for(m2 = 0; m2 < *nfix; m2++){
              // m2 from 0, because we do rectangle, not upper triangle
              d1 = *nfix * k1 + m1;
              d2 = *nfix * k2 + m2;
              k = d1 + d2*(d2+1)/2;
              hess[k] -= hess_coef * auxX * X[coord + *N * FormulaF[m2 + *cumnfix]];
              // its negative off the block-diagonal --> -
            }
          }
        }
        // end of off-diagonal 
      } // end of for k1
      
      
    } // end for j
  } // end for l
  
  
  free(eta);
  free(exp_eta);
  free(softmax);
}
