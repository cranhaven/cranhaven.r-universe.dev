/*
 * Generating a_ords from the conditioned distribution 
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
#include "pdfs_derivatives_api_ord.h"

void metrgibbs_a_ord(struct str_state* last,   // OUT+IN last known values of generated parameters
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
                     int* n_i,               // IN [n] number of observations dedicated to j-th subject
                     int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                     int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                     int* listUi,            // IN [n*G] row by row indices of subjects in group g
                     // Proposal distribution parameters
                     double* prop_a_ord,          // OUT [] place for prop. distribution mean
                     double* prop_chol_a_ord,     // OUT [] cholesky decomposition of proposal distributions precision matrices
                     int* cumdim_prop_a_ord,      // IN [nY[1]+1] cummulative dimensions of chol matrices
                     int* totdim_prop_a_ord,      // IN [1] total dimension of chol matrices (per one cluster)
                     int* m,                         // IN [1] number of the step (to decide whether to update proposal distribution)
                     // Tuning parameters
                     struct str_tuning* tuning       // tuning parameters
){
  int y, g;
  int iter;
  int converged;
  double ord_loglik;
  int shift_nY;
  int pred_skip = 4;
  int kspec_bi_cat = 0;
  int a_ord_dim = 0;
  int is_cat = 0;
  int* cumKord;
  cumKord = (int*)malloc((nY[3]+1) * sizeof(int));
  //int cumKord[nY[3]+1];
  cumKord[0] = 0;
  for(y = 0; y < nY[3]; y++){
    cumKord[y+1] = cumKord[y] + Kord[y]; 
  }
  
  if(spec[1]){
    // a_ord is group-specific
    for(g = 0; g < *G; g++){
      shift_nY = nY[0]+nY[1]+nY[2];
      for(y = 0; y < nY[3]; y++){
        // Update proposal distribution, if needed
        if((*m % *((*tuning).freq_proposal_update)) == 0){
          // call Newton-Raphson for y-th outcome and g-th cluster
          //printf("\nC: Trigerring newton_raphson for a_ord: y = %d, g = %d \n", y, g);
          newton_raphson(last, (*param).api_prior, 
                         Y+*N * shift_nY, X, spec, dims, 
                         predictor_num, predictor_poi, predictor_bin, predictor_ord+*N * 4 * y, predictor_cat,
                         &pred_skip, N, n, &g, Kord+y, Kcat, &is_cat, &kspec_bi_cat,
                         Kord+y, cumnfix+shift_nY, cumKord+y, FormulaF, // instead of totnran --> cumKord
                         // nfix serves as a dimension, which is Kord[y], formula is not needed
                         n_i, i_j, nUg, listUi, 
                         (*last).a_ord + a_ord_dim, // theta_0
                         prop_a_ord + a_ord_dim, // theta_max
                         prop_chol_a_ord + cumdim_prop_a_ord[y] + *totdim_prop_a_ord * g, // var_at_theta_max
                         // Functions used
                         logpapi_ord, d_d2_logpapi_ord,
                         // Tuning parameters
                         tuning, &iter, &converged, &ord_loglik             
          );
        }
        
        // Perform Metropolis within Gibbs
        //printf("\nC: Trigerring metropolis for a_ord: y = %d, g = %d \n", y, g);
        metropolis(last, (*param).api_prior, 
                   Y+*N * shift_nY, X, spec, dims, 
                   predictor_num, predictor_poi, predictor_bin, predictor_ord+*N * 4 * y, predictor_cat,
                   &pred_skip, N, n, &g, Kord+y, Kcat, &is_cat, &kspec_bi_cat,
                   Kord+y, cumnfix+shift_nY, cumKord+y, FormulaF, // instead of totnran --> cumKord
                   n_i, i_j, nUg, listUi, 
                   (*last).a_ord + a_ord_dim, // theta_0
                   prop_chol_a_ord + cumdim_prop_a_ord[y] + *totdim_prop_a_ord * g, // 
                   // Functions used
                   logpapi_ord,
                   // Tuning parameters
                   tuning, (*tuning).const_proposal_a_ord
        );
        
        a_ord_dim += Kord[y];
        shift_nY++;
      }
      
    }
  }else{
    // a_ord is not group-specific
    shift_nY = nY[0] + nY[1] + nY[2];
    for(y = 0; y < nY[3]; y++){
      // Update proposal distribution, if needed
      if((*m % *((*tuning).freq_proposal_update)) == 0){
        // call Newton-Raphson for y-th outcome and g-th cluster
        //printf("\nC: Trigerring newton_raphson for a_ord: y = %d, g = %d \n", y, *G);
        newton_raphson(last, (*param).api_prior, 
                       Y+*N * shift_nY, X, spec, dims, 
                       predictor_num, predictor_poi, predictor_bin, predictor_ord+*N * 4 * y, predictor_cat,
                       &pred_skip, N, n, G, Kord+y, Kcat, &is_cat, &kspec_bi_cat,
                       Kord+y, cumnfix+shift_nY, cumKord+y, FormulaF, // instead of totnran --> cumKord
                       // nfix serves as a dimension, which is Kord[y], formula is not needed
                       n_i, i_j, nUg, listUi, 
                       (*last).a_ord + a_ord_dim, // theta_0
                       prop_a_ord + a_ord_dim, // theta_max
                       prop_chol_a_ord + cumdim_prop_a_ord[y], // var_at_theta_max
                       // Functions used
                       logpapi_ord, d_d2_logpapi_ord,
                       // Tuning parameters
                       tuning, &iter, &converged, &ord_loglik             
        );
      }
      
      // Perform Metropolis within Gibbs
      //printf("\nC: Trigerring metropolis for a_ord: y = %d, g = %d \n", y, *G);
      metropolis(last, (*param).api_prior, 
                 Y+*N * shift_nY, X, spec, dims, 
                 predictor_num, predictor_poi, predictor_bin, predictor_ord+*N * 4 * y, predictor_cat,
                 &pred_skip, N, n, G, Kord+y, Kcat, &is_cat, &kspec_bi_cat,
                 Kord+y, cumnfix+shift_nY, cumKord+y, FormulaF, // instead of totnran --> cumKord
                 n_i, i_j, nUg, listUi, 
                 (*last).a_ord + a_ord_dim, // theta_0
                 prop_chol_a_ord + cumdim_prop_a_ord[y], // 
                 // Functions used
                 logpapi_ord,
                 // Tuning parameters
                 tuning, (*tuning).const_proposal_a_ord
      );
      
      a_ord_dim += Kord[y];
      shift_nY++;
    }
  }
  
  // NO NEED TO UPDATE PREDICTOR
  free(cumKord);
  
}
