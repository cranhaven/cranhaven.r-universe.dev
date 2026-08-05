/*
 * Generating unknown Y values from the conditioned distribution 
 */


#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"
#include "my_math.h"

void gibbs_naY(struct str_state* last,   // OUT+IN last known values of generated parameters
               struct str_param* param,  // IN hyperparameters
               double* Y,                // IN+OUT [*N * (totnY)]
               int* isYna,               // IN [*N * totnY]
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
               double* predictor_cat,    // IN [N*2*nY]  both F and R parts of predictor for numeric outcomes 
               int* G,                   // IN [1]        number of groups=classes=clusters
               int* N,                   // IN [1] number of observations
               int* n,                   // IN [1] number of subjects
               int* nY,                  // IN [4] counts of each of the outcome types
               int* Kord,                // IN [nY[2]] number of categories of each of the Ord outcomes -1
               int* Kcat,                // IN [nY[3]] number of categories of each of the Cat outcomes -1
               int* ngrp,                // IN [totnY] number of FIXED  regressors or each response
               int* cumngrp,             // IN [totnY] where to start in FormulaF
               int* FormulaG,            // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
               int* n_i,                 // IN [n] number of observations dedicated to j-th subject
               int* i_j                 // IN [n * max_n_i] indeces dedicated to j-th subject 
){
  // Declarations
  int i, j, g, k, l, t, t2;      // looping indeces
  int y;                      // variable index
  int shift_nY;
  int coord, predcoord, Xcoord;       // coordinate for matrices as arrays
  double eta, sd;
  double maxeta;
  double u, p, sump;
  int maxK = 0;
  
  for(y = 0; y < nY[3]; y++){
    if(maxK < Kord[y]){
      maxK = Kord[y];
    }
  }
  for(y = 0; y < nY[4]; y++){
    if(maxK < Kcat[y]){
      maxK = Kcat[y];
    }
  }
  
  double* etas;
  etas = (double*)malloc(maxK * sizeof(double));
  //double etas[maxK];
  double* probs;
  probs = (double*)malloc((maxK+1) * sizeof(double));
  double* cumprobs;
  cumprobs = (double*)malloc((maxK+1) * sizeof(double));
  //double probs[maxK+1], cumprobs[maxK+1];
  double* pbeta_num; 
  double* pbeta_poi; 
  double* pbeta_bin;
  double* pbeta_ord;
  double* pbeta_cat;
  double* pc;
  int pnaY;
  int cumKord, cumKcat;
  int newY;
  int fromg, tog, ig;
  int cumindbeta;
  int nacounter = 0;
  
  // Numeric outcomes
  shift_nY = 0;
  cumindbeta = 0;
  for(y = 0; y < nY[0]; y++){
    // Replacing NAs with values sampled from the current model specification
    for(i = 0; i < *n; i++){
      ig = (*last).U[i];
      
      for(j = 0; j < n_i[i]; j++){
        Xcoord = i_j[i + *n * j];
        coord = Xcoord + *N * shift_nY;
        
        // Is Y present?
        if(isYna[coord]){
          if(spec[4]){
            // We sample missing outcome value for each of the clusters, where it might be
            fromg = 0;
            tog = *G;
          }else{
            // We sample missing outcome depending on in which cluster subject i currently is
            fromg = ig;
            tog = ig+1;
          }
          
          for(g = fromg; g < tog; g++){
            
            if(spec[4]){
              pnaY = nacounter + g*dims[32];
            }else{
              pnaY = nacounter;
            }
            
            if(spec[0]){
              sd = 1/sqrt((*last).prec_num[y + g * dims[2]]);
            }else{
              sd = 1/sqrt((*last).prec_num[y]);
            }
            
            pbeta_num = (*last).beta_num + g * dims[1];
            
            // creating predictor by summing F, R and O part
            eta = predictor_num[4*coord] + predictor_num[2 + 4*coord] + predictor_num[3 + 4*coord]; // random part of predictor, which is stable
            // Adding group-specific part - that may differ depending on current ig and g
            t = cumindbeta;
            t2 = cumngrp[shift_nY];
            for(l = 0; l < ngrp[shift_nY]; l++){
              // fixed part of predictor, that needs to be updated wrt beta_num of different g
              eta += pbeta_num[t] * X[Xcoord + *N * FormulaG[t2]]; 
              t++; t2++;
            }
            // not present --> sample from the current model specification
            (*last).naY[pnaY] = rnorm(eta, sd);

            if(g == ig){
              // g is the same as the one in which i currently lies --> fill also Y value 
              Y[coord] = (*last).naY[pnaY];
            }

          } // end for g
          nacounter++;
          
        } // end if is.na Y[coord]
      } // end for j
    } // end for i
    cumindbeta += ngrp[shift_nY];
    shift_nY++;
  }
  
  // Poisson outcome
  cumindbeta = 0;
  for(y = 0; y < nY[1]; y++){
    // Replacing NAs with values sampled from the current model specification
    for(i = 0; i < *n; i++){
      ig = (*last).U[i];
      
      for(j = 0; j < n_i[i]; j++){
        Xcoord = i_j[i + *n * j];
        coord = Xcoord + *N * shift_nY;
        predcoord = Xcoord + *N * y;
        
        // Is Y present?
        if(isYna[coord]){
          if(spec[4]){
            // We sample missing outcome value for each of the clusters, where it might be
            fromg = 0;
            tog = *G;
          }else{
            // We sample missing outcome depending on in which cluster subject i currently is
            fromg = ig;
            tog = ig+1;
          }
          
          for(g = fromg; g < tog; g++){
            
            if(spec[4]){
              pnaY = nacounter + g*dims[32];
            }else{
              pnaY = nacounter;
            }
            
            pbeta_poi = (*last).beta_poi + g * dims[6];
            
            // creating predictor by summing F, R and O part
            eta = predictor_poi[4*predcoord] + predictor_poi[2 + 4*predcoord] + predictor_poi[3 + 4*predcoord]; 
            // Adding group-specific part - that may differ depending on current ig and g
            t = cumindbeta;
            t2 = cumngrp[shift_nY];
            for(l = 0; l < ngrp[shift_nY]; l++){
              // group-specific part of predictor, that needs to be updated wrt beta_poi of different g
              eta += pbeta_poi[t] * X[Xcoord + *N * FormulaG[t2]]; 
              t++; t2++;
            }
            // not present --> sample from the current model specification
            (*last).naY[pnaY] = rpois(exp(eta));
            
            if(g == ig){
              // g is the same as the one in which i currently lies --> fill also Y value 
              Y[coord] = (*last).naY[pnaY];
            }
            
          } // end for g
          nacounter++;
          
        } // end if is.na Y[coord]
      } // end for j
    } // end for i
    cumindbeta += ngrp[shift_nY];
    shift_nY++;
  }
  
  // Binary outcomes
  cumindbeta = 0;
  for(y = 0; y < nY[2]; y++){
    // Replacing NAs with values sampled from the current model specification
    for(i = 0; i < *n; i++){
      ig = (*last).U[i];
      
      for(j = 0; j < n_i[i]; j++){
        Xcoord = i_j[i + *n * j];
        coord = Xcoord + *N * shift_nY;
        predcoord = Xcoord + *N * y;
        
        // Is Y present?
        if(isYna[coord]){
          if(spec[4]){
            // We sample missing outcome value for each of the clusters, where it might be
            fromg = 0;
            tog = *G;
          }else{
            // We sample missing outcome depending on in which cluster subject i currently is
            fromg = ig;
            tog = ig+1;
          }
          
          for(g = fromg; g < tog; g++){
            
            if(spec[4]){
              pnaY = nacounter + g*dims[32];
            }else{
              pnaY = nacounter;
            }

            pbeta_bin = (*last).beta_bin + g * dims[8];
            
            // creating predictor by summing F, R and O part
            eta = predictor_bin[4*predcoord] + predictor_bin[2 + 4*predcoord] + predictor_bin[3 + 4*predcoord]; 
            // group-specific part
            t = cumindbeta;
            t2 = cumngrp[shift_nY];
            for(l = 0; l < ngrp[shift_nY]; l++){
              // group-specific part of predictor, that needs to be updated wrt beta_bin of different g
              eta += pbeta_bin[t] * X[Xcoord + *N * FormulaG[t2]]; 
              t++; t2++;
            }
            // not present --> sample from the current model specification
            u = runif(0.0, 1.0);
            p = logit_inv(eta);
            if(u < p){
              (*last).naY[pnaY] = 1.0;
            }else{
              (*last).naY[pnaY] = 0.0;
            }

            if(g == ig){
              // g is the same as the one in which i currently lies --> fill also Y value 
              Y[coord] = (*last).naY[pnaY];
            }
          } // end for g
          nacounter++;
        } // end for if isYna
      } // end for j
    } // end for i  
    cumindbeta += ngrp[shift_nY];
    shift_nY++;
  } // end for y
  
  // Ordinal outcomes
  cumKord = 0;
  cumindbeta = 0;
  for(y = 0; y < nY[3]; y++){
    // Replacing NAs with values sampled from the current model specification
    for(i = 0; i < *n; i++){
      ig = (*last).U[i];
      
      for(j = 0; j < n_i[i]; j++){
        Xcoord = i_j[i + *n * j];
        coord = Xcoord + *N * shift_nY;
        predcoord = Xcoord + *N * y;
        
        // Is Y present?
        if(isYna[coord]){
          if(spec[4]){
            // We sample missing outcome value for each of the clusters, where it might be
            fromg = 0;
            tog = *G;
          }else{
            // We sample missing outcome depending on in which cluster subject i currently is
            fromg = ig;
            tog = ig+1;
          }
          
          for(g = fromg; g < tog; g++){
            
            if(spec[4]){
              pnaY = nacounter + g*dims[32];
            }else{
              pnaY = nacounter;
            }
            
            pbeta_ord = (*last).beta_ord + g * dims[10];
            
            if(spec[4]){
              pc = (*last).c_ord + cumKord + g*dims[11];
            }else{
              pc = (*last).c_ord + cumKord;
            }
            
            // creating predictor by summing F, R and O part
            eta = predictor_ord[4*predcoord] + predictor_ord[2 + 4*predcoord] + predictor_ord[3 + 4*predcoord]; 
            // group-specific part
            t = cumindbeta;
            t2 = cumngrp[shift_nY];
            for(l = 0; l < ngrp[shift_nY]; l++){
              // group-specific part of predictor, that needs to be updated wrt beta_ord of different g
              eta += pbeta_ord[t] * X[Xcoord + *N * FormulaG[t2]]; 
              t++; t2++;
            }
            // not present --> sample from the current model specification
            cumprobs[0] = 1.0;
            for(k = 0; k < Kord[y]; k++){
              cumprobs[k+1] = logit_inv(eta - pc[k]);
              probs[k] = cumprobs[k] - cumprobs[k+1];
            }
            probs[Kord[y]] = cumprobs[Kord[y]];
            
            u = runif(0.0, 1.0);
            newY = 0;
            sump = probs[0];
            while(sump < u){
              newY++;
              sump += probs[newY];
            } // newY in {0, 1, ..., Kord[y]}
            (*last).naY[pnaY] = newY;
            
            if(g == ig){
              // g is the same as the one in which i currently lies --> fill also Y value 
              Y[coord] = (*last).naY[pnaY];
            }
          } // end for g
          nacounter++;
        } // end for if isYna
      } // end for j
    } // end for i  
    cumindbeta += ngrp[shift_nY];
    cumKord += Kord[y];
    shift_nY++;
  }  
  
  // Categorical outcomes
  cumKcat = 0;
  cumindbeta = 0;
  for(y = 0; y < nY[4]; y++){
    // Replacing NAs with values sampled from the current model specification
    for(i = 0; i < *n; i++){
      ig = (*last).U[i];
      
      for(j = 0; j < n_i[i]; j++){
        Xcoord = i_j[i + *n * j];
        coord = Xcoord + *N * shift_nY;
        
        // Is Y present?
        if(isYna[coord]){
          if(spec[4]){
            // We sample missing outcome value for each of the clusters, where it might be
            fromg = 0;
            tog = *G;
          }else{
            // We sample missing outcome depending on in which cluster subject i currently is
            fromg = ig;
            tog = ig+1;
          }
          
          for(g = fromg; g < tog; g++){
            
            if(spec[4]){
              pnaY = nacounter + g*dims[32];
            }else{
              pnaY = nacounter;
            }
            pbeta_cat = (*last).beta_cat + g * dims[15];
            
            t = cumindbeta;
            maxeta = 0.0;
            for(k = 0; k < Kcat[y]; k++){
              predcoord = Xcoord + *N * (cumKcat + k);
              // creating predictor by summing F, R and O part
              etas[k] = predictor_cat[4*predcoord] + predictor_cat[2 + 4*predcoord] + predictor_cat[3 + 4*predcoord];
              // not present --> sample from the current model specification
              // group-specific part
              t2 = cumngrp[shift_nY];
              for(l = 0; l < ngrp[shift_nY]; l++){
                // group-specific part of predictor, that needs to be updated wrt beta_cat of different g
                etas[k] += pbeta_cat[t] * X[Xcoord + *N * FormulaG[t2]]; 
                t++; t2++;
              }
              
              if(maxeta < etas[k]){
                maxeta = etas[k];
              }
            }
            sump = exp(-maxeta);
            for(k = 0; k < Kcat[y]; k++){
              probs[k] = exp(etas[k] - maxeta);
              sump += probs[k];
            }
            
            for(k = 0; k < Kcat[y]; k++){
              probs[k] /= sump;
            }
            probs[Kcat[y]] = 1.0/sump;
            
            // sampling from categorical distribution with probabilities = probs
            u = runif(0.0, 1.0);
            newY = 0;
            sump = probs[0];
            while(sump < u){
              newY++;
              sump += probs[newY];
            } // newY in {0, 1, ..., Kcat[y]}
            (*last).naY[pnaY] = newY;
            
            if(g == ig){
              // g is the same as the one in which i currently lies --> fill also Y value 
              Y[coord] = (*last).naY[pnaY];
            }
          } // end for g
          nacounter++;
        } // end for if isYna
      } // end for j
    } // end for i  
    
    cumindbeta += Kcat[y]*ngrp[shift_nY];
    cumKcat += Kcat[y];
    shift_nY++;
  }  
  
  
  free(etas);
  free(probs);
  free(cumprobs);
  
  
}
