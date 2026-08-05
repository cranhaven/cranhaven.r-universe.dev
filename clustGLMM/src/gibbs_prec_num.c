/*
 * Generating prec from the conditioned distribution 
 * also sampling numeric Y if NA using the last known values of predictor and prec before the sampling of the new prec
 */


#include <R.h>
#include <Rmath.h>
#include <stdio.h>

#include "structures.h"

void gibbs_prec_num(struct str_state* last,  // OUT+IN last known values of generated parameters
                    struct str_param* param, // IN hyperparameters
                    double* Y,            // IN [*N * totnY]
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
                    double* predictor_num, // IN [N * 2 * nY[0]]  both R and F parts of predictor of numeric outcomes  
                    int* G,           // IN [1] number of classes
                    int* N,           // IN [1] number of observations
                    int* n,           // IN [1] number of subjects
                    int* nY,          // IN [1]        counts of Nums outcomes
                    int* ngrp,        // IN [sum(nY)]  number of FIXED  regressors for each response
                    int* cumngrp,     // IN [totnY] cummulative number of fixed  regressors
                    int* n_i,         // IN [n] number of observations dedicated to j-th subject
                    int* i_j,         // IN [n * max_n_j] indeces dedicated to j-th subject 
                    int* ng,          // IN [G] number of observations in classes 1, ..., G
                    int* nUg,         // IN [G] number of subjects in classes 1, ..., G
                    int* listUi       // IN [n*G] row by row indices of subjects in group g
                )    
{ 
  /** Declarations **/
  int i, j, g, l;               // looping indeces
  int y;                        // variable index
  int coord;                    // coordinate for matrices as arrays
  double newa, newb;            // updated set of parameters for gamma distribution
  double x, eta;                // auxiliary value for squares
  
  // Is prec class-specific?
  if(spec[0]){
    
    for(g = 0; g < *G; g++){
      for(y = 0; y < nY[0]; y++){
        // prior contribution
        newa = *((*param).gamma_a);
        newb = *((*param).gamma_b);
        //printf("\nprec_num: g = %d, y = %d, newa = %f, newb = %f", g, y, newa, newb);
        
        // model contributions
        newa += (ng[g] + ngrp[y])/2.0;
        // beta prior contribution
        for(j = 0; j < ngrp[y]; j++){
          x = (*last).beta_num[j + cumngrp[y] + g*dims[1]] / *((*param).sd_beta_num); 
          newb += 0.5 * x * x;
        }
        
        //printf("\nprec_num: g = %d, y = %d, newa = %f, newb = %f", g, y, newa, newb);
        // model contribution - only those observations in k-th class
        for(l = 0; l < nUg[g]; l++){
          i = listUi[l + *n * g];
          for(j = 0; j < n_i[i]; j++){
            coord = (i_j[i + *n * j] + *N * y);
            // creating predictor by summing F, G, R, and O part
            eta  = predictor_num[4*coord];
            eta += predictor_num[1 + 4*coord];
            eta += predictor_num[2 + 4*coord];
            eta += predictor_num[3 + 4*coord];
            
            x = Y[coord] - eta; 
            newb += 0.5 * x * x;
          }
        }
        
        // generating new prec
        (*last).prec_num[y + g * dims[2]] = rgamma(newa, 1/newb);
        
        //printf("\nprec_num newa = %f, newb = %f, prec_num[%d] = %f",
        //       newa, newb, y + g * dims[3], (*last).prec_num[y + g * dims[3]]);
      }
    }
  }else{ 
    // there is only one prec for each numeric response to be generated
    
    for(y = 0; y < nY[0]; y++){
      // prior contribution
      newa = *((*param).gamma_a) + *N/2.0;
      newb = *((*param).gamma_b);
      // beta_num prior contributions
      if(spec[1]){
        // beta is class-specific
        newa += *G * ngrp[y]/2.0;
        
        for(g = 0; g < *G; g++){
          for(j = 0; j < ngrp[y]; j++){
            x = (*last).beta_num[j + cumngrp[y] + g*dims[1]] / *((*param).sd_beta_num); 
            newb += 0.5 * x * x;
          }
        }
        
      }else{
        // beta is not class-specific
        newa += ngrp[y]/2.0;
        
        for(j = 0; j < ngrp[y]; j++){
          x = (*last).beta_num[j + cumngrp[y]] / *((*param).sd_beta_num);
          //- (*param).fixmu[j + cumnfix[y]];
          newb += 0.5 * x * x;
        }
      }
      
      //printf("gibbs_prec: newa = %f, newb = %f\n", newa, newb);
      //fflush(stdout);
      
      
      // model contribution - all observations
      for(j = 0; j < *N; j++){
        coord = j + *N * y;
        
        // creating predictor by summing F, G, R, and O part
        eta  = predictor_num[4*coord];
        eta += predictor_num[1 + 4*coord];
        eta += predictor_num[2 + 4*coord];
        eta += predictor_num[3 + 4*coord];
        
        x = Y[coord] - eta; 
        
        newb += 0.5 * x * x;
      }
      
      // generating new prec
      (*last).prec_num[y] = rgamma(newa, 1/newb);
      //printf("y=%d, newa = %f, newb = %f\n", y, newa, newb);
    }
    
  } // end of if spec["prec_num"]
  
  
} // end of void 
