/*
 * Generating beta_poi_fix and beta_poi from the conditioned distribution 
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
#include "pdfs_derivatives_beta_poi.h"

void metrgibbs_beta_poi(struct str_state* last,   // OUT+IN last known values of generated parameters
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
                        double* predictor_cat,
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
                        double* prop_beta_poi_fix,      // OUT [] place for prop. distribution mean
                        double* prop_chol_beta_poi_fix, // OUT [] cholesky decomposition of proposal distributions precision matrices
                        int* cumdim_prop_beta_poi_fix,  // IN [nY[1]+1] cummulative dimensions of chol matrices
                        int* totdim_prop_beta_poi_fix,  // IN [1] total dimension of chol matrices (per one cluster)
                        double* prop_beta_poi,          // OUT [] place for prop. distribution mean
                        double* prop_chol_beta_poi,     // OUT [] cholesky decomposition of proposal distributions precision matrices
                        int* cumdim_prop_beta_poi,      // IN [nY[1]+1] cummulative dimensions of chol matrices
                        int* totdim_prop_beta_poi,      // IN [1] total dimension of chol matrices (per one cluster)
                        int* m,                         // IN [1] number of the step (to decide whether to update proposal distribution)
                        // Tuning parameters
                        struct str_tuning* tuning       // tuning parameters
){
  //printf("\nmetrgibbs_beta_poi: Ahoj");
  //fflush(stdout);
  
  int y, g;
  int iter;
  int converged;
  double poi_loglik;
  int shift_nY;
  int is_cat = 0;
  int pred_skip;
  int kspec_bi_cat = 0;
  int beta_dim;
  
  ////------------------------------////
  //// Fixed effects - beta_poi_fix ////
  ////------------------------------////
  pred_skip = 0;
  beta_dim = 0;
  shift_nY = nY[0];
  
  for(y = 0; y < nY[1]; y++){
    // UPDATE AND PROPOSE ONLY WHEN THERE IS SOMETHING TO BE PROPOSED!
    if(nfix[shift_nY] > 0){
      // Update proposal distribution, if needed
      if((*m % *((*tuning).freq_proposal_update)) == 0){
        // call Newton-Raphson for y-th outcome and g=G, which means all of them
        //printf("\nC: Trigerring newton_raphson for beta_poi_fix: y = %d, g = %d \n", y, *G);
        newton_raphson(last, (*param).sd_beta_poi_fix, 
                       Y+*N * shift_nY, X, spec, dims, 
                       predictor_num, predictor_poi+*N * 4 * y, predictor_bin, predictor_ord, predictor_cat,
                       &pred_skip, N, n, G, Kord, Kcat, &is_cat, &kspec_bi_cat,
                       nfix+shift_nY, cumnfix+shift_nY, totnran, FormulaF, // use totnran - not used anyway
                       n_i, i_j, nUg, listUi, 
                       (*last).beta_poi_fix + beta_dim, // theta_0
                       prop_beta_poi_fix + beta_dim, // theta_max
                       prop_chol_beta_poi_fix + cumdim_prop_beta_poi_fix[y], // var_at_theta_max
                       // Functions used
                       logpbeta_poi, d_d2_logpbeta_poi,
                       // Tuning parameters
                       tuning, &iter, &converged, &poi_loglik             
        );
      }
      
      // Perform Metropolis within Gibbs
      //printf("\nC: Trigerring metropolis for beta_poi_fix: y = %d, g = %d \n", y, *G);
      metropolis(last, (*param).sd_beta_poi_fix, 
                 Y+*N * shift_nY, X, spec, dims, 
                 predictor_num, predictor_poi+*N * 4 * y, predictor_bin, predictor_ord, predictor_cat,
                 &pred_skip, N, n, G, Kord, Kcat, &is_cat, &kspec_bi_cat,
                 nfix+shift_nY, cumnfix+shift_nY, totnran, FormulaF, // use totnran - not used anyway
                 n_i, i_j, nUg, listUi, 
                 (*last).beta_poi_fix + beta_dim, // theta_0
                 prop_chol_beta_poi_fix + cumdim_prop_beta_poi_fix[y], // 
                 // Functions used
                 logpbeta_poi,
                 // Tuning parameters
                 tuning, (*tuning).const_proposal_beta_poi_fix
                 );
      
      beta_dim += nfix[shift_nY];
    } // end of if nfix>0
    shift_nY++;
  }
  
  
  ////-------------------------------////
  //// Predictor update - fixed part ////
  ////-------------------------------////
  int update[4];
  int* K;
  K = (int*)malloc(nY[1] * sizeof(int));
  //int K[nY[1]];
  
  update[0] = 1;
  update[1] = update[2] = update[3] = 0;
  for(y = 0; y < nY[1]; y++){
    K[y] = 1;
  }
  shift_nY = nY[0];
  
  calculate_predictor_separately(
    Id, X, 
    predictor_poi, update,
    // Parameters describing dimensions //
    N, nY +1, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shift_nY, ngrp +shift_nY, nran +shift_nY, noff +shift_nY, 
    cumnfix +shift_nY, cumngrp +shift_nY, cumnran +shift_nY, cumnoff +shift_nY, 
    totnran,
    dims+6,     // dimension of single group-specific beta_poi
    K, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_poi_fix, (*last).beta_poi, (*last).b, (*last).U            
  );
  
  ////-----------------------------------////
  //// Group-specific effects - beta_poi ////
  ////-----------------------------------////
  pred_skip = 1;
  beta_dim = 0;
  
  for(g = 0; g < *G; g++){
    // First by g because these g-blocks of betas for Pois outcomes are stored after each other
    shift_nY = nY[0];
    for(y = 0; y < nY[1]; y++){
      // UPDATE AND PROPOSE ONLY WHEN THERE IS SOMETHING TO BE PROPOSED!
      if(ngrp[shift_nY] > 0){
        // Update proposal distribution, if needed
        if((*m % *((*tuning).freq_proposal_update)) == 0){
          // call Newton-Raphson for y-th outcome within g-th cluster
          //printf("\nC: Trigerring newton_raphson for beta_poi: y = %d, g = %d \n", y, g);
          newton_raphson(last, (*param).sd_beta_poi, 
                         Y+*N * shift_nY, X, spec, dims, 
                         predictor_num, predictor_poi+*N * 4 * y, predictor_bin, predictor_ord, predictor_cat,
                         &pred_skip, N, n, &g, Kord, Kcat, &is_cat, &kspec_bi_cat,
                         ngrp+shift_nY, cumngrp+shift_nY, totnran, FormulaG, // use totnran - not used anyway
                         n_i, i_j, nUg, listUi, 
                         (*last).beta_poi + beta_dim, // theta_0
                         prop_beta_poi + beta_dim, // theta_max
                         prop_chol_beta_poi + cumdim_prop_beta_poi[y] + *totdim_prop_beta_poi * g, // var_at_theta_max
                         // Functions used
                         logpbeta_poi, d_d2_logpbeta_poi,
                         // Tuning parameters
                         tuning, &iter, &converged, &poi_loglik             
          );
        }
        
        // Perform Metropolis within Gibbs
        //printf("\nC: Trigerring metropolis for beta_poi: y = %d, g = %d \n", y, g);
        metropolis(last, (*param).sd_beta_poi, 
                   Y+*N * shift_nY, X, spec, dims, 
                   predictor_num, predictor_poi+*N * 4 * y, predictor_bin, predictor_ord, predictor_cat,
                   &pred_skip, N, n, &g, Kord, Kcat, &is_cat, &kspec_bi_cat,
                   ngrp+shift_nY, cumngrp+shift_nY, totnran, FormulaG, // use totnran - not used anyway
                   n_i, i_j, nUg, listUi, 
                   (*last).beta_poi + beta_dim, // theta_0
                   prop_chol_beta_poi + cumdim_prop_beta_poi[y] + *totdim_prop_beta_poi * g, // 
                   // Functions used
                   logpbeta_poi,
                   // Tuning parameters
                   tuning, (*tuning).const_proposal_beta_poi
        );
        
        beta_dim += ngrp[shift_nY];
      } // end of if nfix>0
      shift_nY++;
    } // end of for y
  }// end of for g
  
  ////----------------------------------------////
  //// Predictor update - group-specific part ////
  ////----------------------------------------////
  
  update[1] = 1;
  update[0] = update[2] = update[3] = 0;
  shift_nY = nY[0];
  
  calculate_predictor_separately(
    Id, X, 
    predictor_poi, update,
    // Parameters describing dimensions //
    N, nY +1, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shift_nY, ngrp +shift_nY, nran +shift_nY, noff +shift_nY, 
    cumnfix +shift_nY, cumngrp +shift_nY, cumnran +shift_nY, cumnoff +shift_nY, 
    totnran,
    dims+6,     // dimension of single group-specific beta_poi
    K, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_poi_fix, (*last).beta_poi, (*last).b, (*last).U            
  );

  free(K);
}
