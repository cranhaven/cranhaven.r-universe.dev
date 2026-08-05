/*
 * Generating random effects b_i from the conditioned distribution 
 */


#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"

#include "my_math.h"
#include "calculate_predictor.h"
#include "matrmult.h"
#include "cholesky.h"

#include "newton_raphson.h"
#include "metropolis.h"
#include "pdfs_derivatives_bi.h"

void metrgibbs_bi(struct str_state* last,   // OUT+IN last known values of generated parameters
                  struct str_param* param,  // IN hyperparameters
                  int* Id,                  // [N] IDs
                  double* Y,                // IN [*N * (1 + totnY)]
                  double* X,                // [N*(1 + #regr)]    ID + regressors
                  int* spec,                // [5]                class-specific parameters
                  // order:   [0] prec_num, 
                  //          [1] c_ord,
                  //          [2] InvSigma, 
                  //          [3] InvQ, 
                  //          [4] naY
                  int* dims,                // [33]:              what parameters are saved
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
                  double* predictor_cat,    // IN [N*2*nY]  both F and R parts of predictor for numeric outcomes 
                  int* N,                 // IN [1] number of observations
                  int* n,                 // IN [1] number of subjects
                  int* nY,                // IN [5] counts of outcomes
                  int* G,                 // IN [1] number of classes
                  int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                  int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                  int* nfix,              // IN [1] number of FIXED  regressors or each response
                  int* cumnfix,           // IN [1] where to start in FormulaF
                  int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                  int* ngrp,              // IN [1] number of GROUP-SPECIFIC  regressors or each response
                  int* cumngrp,           // IN [1] where to start in FormulaG
                  int* FormulaG,          // IN [ngrp]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                  int* nran,              // IN [1] number of RANDOM  regressors or each response
                  int* cumnran,           // IN [1] where to start in FormulaR
                  int* totnran,           // IN [1] total dimension of RANDOM parameter (useless)
                  int* FormulaR,          // IN [nran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                  int* noff,              // IN [1] number of OFFSET regressors or each response
                  int* cumnoff,           // IN [1] where to start in FormulaO
                  int* FormulaO,          // IN [noff]  numbers of columns of X that should be used for OFFSET  effects of modelled responses
                  int* n_i,               // IN [n] number of observations dedicated to j-th subject
                  int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                  int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                  int* listUi,            // IN [n*G] row by row indices of subjects in group g
                  // Proposal distribution parameters
                  double* prop_bi,          // OUT [*n * totnran] place for prop. distribution mean
                  double* prop_chol_bi,     // OUT [*n * totnran*(totnran+1)/2] cholesky decomposition of proposal distributions precision matrices
                  //double* loglik_last_bi, // OUT [n] loglikelihood under current beta_cat 
                  // Tuning parameters
                  int* lag_proposal_bi,           // [n] if 0, then we should update the distribution
                  struct str_tuning* tuning       // tuning parameters
){
  
  int y, i, k;
  int iter;
  int converged;
  double* loglik_bi;
  loglik_bi = (double*)malloc(*n* sizeof(double));
  //double loglik_bi[*n];
  double* some_double_pointer = (*param).sd_beta_bin;
  int is_cat; 
  //int pred_skip;
  int kspec_bi_cat = *((*tuning).kspec_bi_cat);
  
  ////--------------------------////
  ////   Random effects - b_i   ////
  ////--------------------------////
  //pred_skip = 2; // Random part of the predictor is skipped, F,G,O parts remain the same
  is_cat = 2; // signalizes that it is not beta nor cat, but bi
  
  //for(i = 0; i < 1; i++){ // for development only to 2
  for(i = 0; i < *n; i++){
    // update random effect for i-th subject
    //printf("\nmetrgibbs_bi: n = %d,  n_i[%d] = %d, lag[%d] = %d", *n, i, n_i[i], i, lag_proposal_bi[i]);
    // Start with updating the proposal distribution
    if(lag_proposal_bi[i] == 0){
      //printf("\nTrigerring newton_raphson for b[%d]: \n", i);
      newton_raphson(last, some_double_pointer, Y, X, spec, dims, 
                     predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                     // pred_skip = 2 is obvious, use this pointer to pass nY instead
                     // g/G parameter now serves for passing the value of i - the number of subject
                     nY, N, n, &i, Kord, Kcat, &is_cat, &kspec_bi_cat,
                     nran, cumnran, totnran, FormulaR, 
                     n_i, i_j, nUg, listUi, 
                     (*last).b + *totnran * i, // theta_0
                     prop_bi + *totnran * i, // theta_max
                     prop_chol_bi + *totnran*(*totnran+1)/2 * i, // var_at_theta_max
                     // Functions used
                     logpbi, d_d2_logpbi,
                     // Tuning parameters
                     tuning, &iter, &converged, loglik_bi + i             
      );
    }
    
    // Now perform Metropolis step
    //printf("\nC: Trigerring metropolis for b[%d]: \n", i);
    metropolis(last, some_double_pointer, Y, X, spec, dims, 
               predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
               // pred_skip = 2 is obvious, use this pointer to pass nY instead
               // g/G parameter now serves for passing the value of i - the number of subject
               nY, N, n, &i, Kord, Kcat, &is_cat, &kspec_bi_cat,
               nran, cumnran, totnran, FormulaR, 
               n_i, i_j, nUg, listUi, 
               (*last).b + *totnran * i, // theta_0
               prop_chol_bi + *totnran*(*totnran+1)/2 * i,
               // Functions used
               logpbi,
               // Tuning parameters
               tuning, (*tuning).const_proposal_b
    );
    
  }
  
  ////--------------------------------////
  //// Predictor update - random part ////
  ////--------------------------------////
  
  int update[4];
  int* K;
  K = (int*)malloc((nY[0] + nY[1] + nY[2] + nY[3] + nY[4])* sizeof(int));
  //int K[nY[0] + nY[1] + nY[2] + nY[3] + nY[4]];
  int shift_nY;
  
  update[2] = 1;
  update[0] = update[1] = update[3] = 0;
  
  for(y = 0; y < nY[0] + nY[1] + nY[2] + nY[3]; y++){
    K[y] = 1;
  }
  for(k = 0; k < nY[4]; k++){
    K[y+k] = Kcat[k];
  }
  
  shift_nY = 0;
  
  // The following are not categorical outcomes
  kspec_bi_cat = 0;
  //// Numeric outcome predictors
  calculate_predictor_separately(
    Id, X, 
    predictor_num, update,
    // Parameters describing dimensions //
    N, nY +0, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shift_nY, ngrp +shift_nY, nran +shift_nY, noff +shift_nY, // all start with numeric variables
    cumnfix +shift_nY, cumngrp +shift_nY, cumnran +shift_nY, cumnoff +shift_nY, 
    totnran,
    dims+1,     // dimension of single group-specific beta_num
    K +shift_nY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_num_fix, (*last).beta_num, (*last).b, (*last).U            
  );
  shift_nY += nY[0];
  
  //// Poisson outcome predictors
  calculate_predictor_separately(
    Id, X, 
    predictor_poi, update,
    // Parameters describing dimensions //
    N, nY +1, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shift_nY, ngrp +shift_nY, nran +shift_nY, noff +shift_nY, 
    cumnfix +shift_nY, cumngrp +shift_nY, cumnran +shift_nY, cumnoff +shift_nY, 
    totnran,
    dims+6,     // dimension of single group-specific beta_num
    K +shift_nY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_poi_fix, (*last).beta_poi, (*last).b, (*last).U            
  );
  shift_nY += nY[1];
  
  //// Binary outcome predictors
  calculate_predictor_separately(
    Id, X, 
    predictor_bin, update,
    // Parameters describing dimensions //
    N, nY +2, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shift_nY, ngrp +shift_nY, nran +shift_nY, noff +shift_nY, 
    cumnfix +shift_nY, cumngrp +shift_nY, cumnran +shift_nY, cumnoff +shift_nY, 
    totnran,
    dims+8,     // dimension of single group-specific beta_num
    K +shift_nY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_bin_fix, (*last).beta_bin, (*last).b, (*last).U            
  );
  shift_nY += nY[2];
  
  //// Ordinal outcome predictors
  calculate_predictor_separately(
    Id, X, 
    predictor_ord, update,
    // Parameters describing dimensions //
    N, nY +3, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shift_nY, ngrp +shift_nY, nran +shift_nY, noff +shift_nY, 
    cumnfix +shift_nY, cumngrp +shift_nY, cumnran +shift_nY, cumnoff +shift_nY, 
    totnran,
    dims+10,     // dimension of single group-specific beta_num
    K +shift_nY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_ord_fix, (*last).beta_ord, (*last).b, (*last).U            
  );
  shift_nY += nY[3];
  
  //// Categorical outcomes predictors
  kspec_bi_cat = *((*tuning).kspec_bi_cat);
  calculate_predictor_separately(
    Id, X, 
    predictor_cat, update,
    // Parameters describing dimensions //
    N, nY +4, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shift_nY, ngrp +shift_nY, nran +shift_nY, noff +shift_nY, 
    cumnfix +shift_nY, cumngrp +shift_nY, cumnran +shift_nY, cumnoff +shift_nY, 
    totnran,
    dims+15,     // dimension of single group-specific beta_num
    K +shift_nY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_cat_fix, (*last).beta_cat, (*last).b, (*last).U            
  );
  
  free(loglik_bi);
  free(K);
  
}
