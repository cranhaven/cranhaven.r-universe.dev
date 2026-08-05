/*
 * Calculating full-conditional classification probabilities pUig 
 * and sampling new group allocations U[i]
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

#include "pdfs_last.h"

#define LOGPINF -10e100
#define SHIFTLOGP 10

void gibbs_pUig(struct str_state* last,  // IN last known values of generated parameters
                struct str_param* param, // IN hyperparameters
                int* Id,                  // [N] IDs
                double* Y,                // IN [totnY*N]
                int* isYna,               // IN [*N * totnY]
                int* isYna_inv,           // IN [*N * totnY]
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
                int* G,                 // IN [1] total number of clusters
                int* Gp,                // OUT [1] number of nonempty clusters
                int* Gm,                // OUt [1] number of empty clusters
                int* N,                 // IN [1] number of observations
                int* n,                 // IN [1] number of subjects
                int* nY,                // IN [5]
                int* Kord,              // IN [nY[3]] total number of categories of Ord outcomes -1
                int* Kcat,              // IN [nY[4]] total number of categories of Cat outcomes -1
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
                int* n_i,               // IN [n] number of observations dedicated to i-th subject
                int* i_j,               // IN [n * max_n_j] indeces dedicated to i-th subject 
                int* ng,                // OUT [G] individual observation count in g-th cluster
                int* nUg,               // OUT [G] number of subjects in g-th cluster
                int* listUi,            // OUT [(G+1) * n] row by row indices of subjects in group g
                double* loglik,         // OUT [1] conditional-loglikelihood under new sampled U[i]
                // Tuning parameters
                int* lag_proposal_bi,     // OUT [*n] number of non-updated proposals for bi in a row, if 0 --> to be updated
                struct str_tuning* tuning // tuning parameters
){
  
  int i, g;
  //double* pInvSigma;
  double x;
  double* logp;
  logp = (double*)malloc(*G * sizeof(double));
  //double logp[*G];
  double* explogp;
  explogp = (double*)malloc(*G * sizeof(double));
  //double explogp[*G];
  double sumexplogp;
  double maxlogp;
  
  int newU;
  double sump;
  double upom;
  int print;
  
  
  // to be updated
  for(g = 0; g < *G; g++){
    nUg[g] = 0;
    ng[g]  = 0;
  }
  *loglik = 0.0;
  
  // take each individual separately
  for(i = 0; i < *n; i++){
    //// Calculate log P(U[i] = g | ...)
    maxlogp = LOGPINF;
    for(g = 0; g < *G; g++){
      // pointers to parameters
      //logp[g] = 0.0; // already done by logYi_g
      // Contribution of p(Y_i | ...)
      print = 0;
      logpYi_g(last, param, Y, isYna, isYna_inv, X, spec, dims,
               predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
               &g, N, n, nY, &i, Kord, Kcat, 
               ngrp, cumngrp, FormulaG, n_i, i_j, &print,
               logp + g
      );
      if(isfinite(logp[g]) == 0){
        Rprintf("\ngibbs_pUig: i=%d, g=%d returned infinity from logpYi_g", i, g);
        print = 1;
        logpYi_g(last, param, Y, isYna, isYna_inv, X, spec, dims,
                 predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                 &g, N, n, nY, &i, Kord, Kcat, 
                 ngrp, cumngrp, FormulaG, n_i, i_j, &print,
                 logp + g
        );
      }
      
      // Contribution of p(b_i | ...)
      if(*totnran > 0){
        if(spec[2]){
          // InvSigma is g-specific
          aBa((*last).b + *totnran * i, (*last).InvSigma + g*dims[16], &x, totnran);
          logp[g] -= 0.5 * x;
          logp[g] += 0.5 * log((*last).detInvSigma[g]);
        }else{
          // else does not matter to be included in pUig calculation 
          // but it matters for loglik evaluation
          aBa((*last).b + *totnran * i, (*last).InvSigma, &x, totnran);
          logp[g] -= 0.5 * x;
          logp[g] += 0.5 * log((*last).detInvSigma[0]);
        }
        logp[g] -= *totnran * HALF_LOG_2PI; // normalizing constant for totnran dimensional N distribution
      }
      
      if(isfinite(logp[g]) == 0){
        Rprintf("\ngibbs_pUig: i=%d, g=%d returned infinity after addition of p(b_i | InvSigma)", i, g);
      }
      
      // marginal P(U[i] = g) contribution
      if((*last).w[g] == 0){
        logp[g] -= 1e15;
      }else{
        logp[g] += log((*last).w[g]);
      }
      //logp[g] += log((*last).w[g]);
      //(*last).pUig[(*G) * i + g] = logp[g];
      
      if(isfinite(logp[g]) == 0){
        Rprintf("\ngibbs_pUig: i=%d, g=%d returned infinity after addition of p(w)", i, g);
      }
      
      // maxlogp actualization
      if(maxlogp < logp[g]){
        maxlogp = logp[g];
      }
    }
    
    // shifting on log scale for computational reasons
    sumexplogp = 0.0;
    for(g = 0; g < *G; g++){
      explogp[g] = exp(logp[g] + SHIFTLOGP - maxlogp);
      sumexplogp += explogp[g];
    }
    
    if(isfinite(sumexplogp) == 0){
      Rprintf("\ngibbs_pUig: i=%d returned infinity of sumexplogp: ", i);
      for(g = 0; g < *G; g++){
        Rprintf("%d = %f, ", g, explogp[g]);
      }
    }
    
    // computing and updating pUig
    for(g = 0; g < *G; g++){
      explogp[g] /= sumexplogp;
      (*last).pUig[(*n) * g + i] = explogp[g];
    }
    
    //// Sample new U[i] given new full-cond probabilities pUig
    newU = 0;
    sump = explogp[0];
    //if(sump > 1.0){
    //  printf("\ngibbs_pUig: i = %d, sump = %f", i, sump);
    //  fflush(stdout);
    //}
    upom = runif(0.0, 1.0);
    while(sump < upom){
      newU++;
      sump += explogp[newU];
    } // newU in {0, 1, ..., G-1}
    
    if((*last).U[i] == newU){
      // the same cluster as before
      lag_proposal_bi[i]++;
      if(lag_proposal_bi[i] == *((*tuning).freq_proposal_update)){
        // it is time to be updated
        lag_proposal_bi[i] = 0;
      }
    }else{
      // different cluster than before --> definitely time to update the proposal distribution
      lag_proposal_bi[i] = 0;
    }
    (*last).U[i] = newU;
    *loglik += logp[newU]; // log-likelihood as if latent data observed at current values
    
    
    // update of nUg, ng and listUi
    ng[newU] += n_i[i];
    listUi[*n * newU + nUg[newU]] = i; // add to cluster newU
    nUg[newU]++;
    
  }
  
  *Gm = 0;
  for(g = 0; g < *G; g++){
    if(nUg[g] == 0){
      (*Gm)++;
    }
  }
  *Gp = *G - *Gm;
  
  
  
  //// Some subjects could be sent into a different cluster
  //// Which means that their group-specific part of predictor needs to be updated
  
  ////----------------------------------------////
  //// Predictor update - group-specific part ////
  ////----------------------------------------////
  
  int update[4];
  int* K;
  K = (int*)malloc((nY[0] + nY[1] + nY[2] + nY[3] + nY[4]) * sizeof(int));
  //int K[nY[0] + nY[1] + nY[2] + nY[3] + nY[4]];
  int y, k, shift_nY;
  
  update[1] = 1;
  update[0] = update[2] = update[3] = 0;
  
  for(y = 0; y < nY[0] + nY[1] + nY[2] + nY[3]; y++){
    K[y] = 1;
  }
  for(k = 0; k < nY[4]; k++){
    K[y+k] = Kcat[k];
  }
  
  shift_nY = 0;
  
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
    K +shift_nY, (*tuning).kspec_bi_cat,
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
    K +shift_nY, (*tuning).kspec_bi_cat,
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
    K +shift_nY, (*tuning).kspec_bi_cat,
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
    K +shift_nY, (*tuning).kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_ord_fix, (*last).beta_ord, (*last).b, (*last).U            
  );
  shift_nY += nY[3];
  
  //// Categorical outcomes predictors
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
    K +shift_nY, (*tuning).kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_cat_fix, (*last).beta_cat, (*last).b, (*last).U            
  );
  
  free(logp);
  free(explogp);
  free(K);
  
}

