/*
 * Metropolis step for arbitrary parameter
 */

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"
#include "matrmult.h"
#include "cholesky.h"

void metropolis(struct str_state* last,   // IN last known values of generated parameters
                double* hyperparameter,   // IN hyperparameters
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
                int* is_cat,            // IN [1] Is it categorical variable?
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
                double* theta,          // IN+OUT [dims[.]] theta from which we start the random walk
                //double* loglik,         // IN+OUT [1] the value of maximized log-likelihood
                double* proposal_chol,  // IN [dims[.] * (dims[.]+1) / 2]
                // Functions used
                void (*fun)(struct str_state*, double*, double*, double*, int*, int*, 
                      double*, double*, double*, double*, double*, int*,
                      int*, int*, int*, int*, int*, int*, 
                      int*, int*, int*, int*, int*, int*, int*, int*,
                      double*, double*),
                // Tuning parameters
                struct str_tuning* tuning, // tuning parameters
                double* const_proposal   // IN [1] pointer to one of tuning parameters
                  
){
  // Auxiliary parameters
  int j, k;
  double loglik_new; 
  double loglik;
  
  int dim_theta;
  
  if(*is_cat == 2){
    dim_theta = *totnfix; // we are working with random effects, actually totnran
  }else{
    if(*is_cat == 1){
      dim_theta = *nfix * *Kcat; // beta_cat_fix or beta_cat
    }else{
      dim_theta = *nfix; // some other beta parameter or a_ord
    }
  }
  
  (*fun)(last, hyperparameter, Y, X, spec, dims, 
   predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
   N, n, g, Kord, Kcat, kspec_bi_cat, 
   nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
   theta, &loglik);
  
  double N01[dim_theta];
  double theta_new[dim_theta];
  double shift[dim_theta];
  double accept_prob;
  double u;
  int accept;
  
  //printf("\nmetropolis: nfix = %d, Kcat = %d, dim_theta = %d", *nfix, *Kcat, dim_theta);
  //fflush(stdout);

  
  for(j = 0; j < *((*tuning).times_proposal); j++){
    //// Sample shift from N(0, const_proposal^2 * [Hess matrix]^{-1})
    // We already have cholesky decomposition of Hess matrix in proposal_chol
    //printf("\nmetropolis: g = %d, j = %d, theta", *g, j);
    for(k = 0; k < dim_theta; k++){
      N01[k] = rnorm(0.0, 1.0);
      //printf("[%d] = %f, ", k, theta[k]);
    }
    //fflush(stdout);
    //for(int p = 0; p < dim_theta; p++){
    //  printf("\n");
    //  for(int q = p; q < dim_theta; q++){
    //    printf("[%d,%d] = %f, ", p, q, proposal_chol[p + q*(q+1)/2]);
    //  }
    //}
    
    // Now backsolve for scaled shift
    backsolve2(proposal_chol, N01, &dim_theta, shift);
    // Scale shift by const_proposal --> to be done in next step
    //printf("\nmetropolis: shift: ");
    //for(k = 0; k < dim_theta; k++){
    //  printf("[%d] = %f, ", k, *const_proposal * shift[k]);
    //}
    //fflush(stdout);
    
    // Compute theta_new by adding shift to theta_last
    //printf("\nmetropolis: theta_new: ");
    for(k = 0; k < dim_theta; k++){
      theta_new[k] = theta[k] + *const_proposal * shift[k] ; // scaling in this step
      //printf("[%d] = %f, ", k, theta_new[k]);
    }
    
    // Log-likelihood at newly proposed theta
    (*fun)(last, hyperparameter, Y, X, spec, dims, 
     predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
     N, n, g, Kord, Kcat, kspec_bi_cat, 
     nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
     theta_new, &loglik_new);
    
    // Compute acceptance probability for theta_new
    //printf("\nmetropolis: loglik = %f, loglik_new = %f, ",
    //         loglik, loglik_new);
    //fflush(stdout);
    // Do we accept theta_new?
    accept = 0;
    if(loglik_new > loglik){
      // definitely accept this new theta
      //accept_prob = 1.0;
      accept = 1;
      //printf("accept = %d", accept);
    }else{
      accept_prob = exp(loglik_new - loglik);
      // accept_prob lies within [0, 1] --> sample u from Unif[0,1]
      u = runif(0.0, 1.0);
      if(u < accept_prob){
        accept = 1;
      }// otherwise accept remains to be 0
      //printf("accept_prob = %f, u = %f, accept = %d", accept_prob, u, accept);
      //fflush(stdout);
      
    }
    
    // If to be accepted switch theta and loglik
    if(accept){
      for(k = 0; k < dim_theta; k++){
        theta[k] = theta_new[k];
      }
      loglik = loglik_new;
    }
    
    // ... and continue with another proposals
  }
}
