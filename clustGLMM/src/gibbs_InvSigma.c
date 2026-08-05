/*
 * Generating InvSigma from the conditioned distribution 
 */


#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>

#include "myrwishart.h"
#include "matrmult.h"
#include "cholesky.h"

#include "structures.h"

void gibbs_InvSigma(struct str_state* last,  // OUT+IN last known values of generated parameters
                    struct str_param* param, // IN hyperparameters
                    int* spec,            // [5]                class-specific parameters
                    // order:   [0] prec_num, 
                    //          [1] c_ord,
                    //          [2] InvSigma, 
                    //          [3] InvQ, 
                    //          [4] naY
                    int* whatsave,        // [33]:              what parameters are saved
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
                    int* dimswithG,       // [33]:              what parameters are saved
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
                    int* G,                 // IN [1] number of classes
                    int* n,                 // IN [1] number of subjects
                    int* totnran,           // IN [1] total number of random regressors
                    int* m,                 // IN [1] iteration number
                    int* nUg,               // IN [K] number of subjects     in classes 1, ..., G
                    double* Sigma,          // [(B+M)*totnran*(totnran+1)/2(* G)] Prior variance matrix of random effects
                    double* detInvSigma,    // [(B+M)(* G)] determinant of InvSigma
                    double* sdSigma,        // [(B+M)*totnran(* G)] Prior standard deviations of random effects
                    double* corSigma       // [(B+M)*totnran*(totnran-1)/2(* G)] Prior correlation matrix of random effects
                    )    
{ 
  /** Declarations **/
  int i, j, g, l;           // looping indeces
  int coord;                // coordinate for matrices as arrays
  double* Smatrix;
  Smatrix = (double*)malloc(dims[16] * sizeof(double));
  //double Smatrix[dims[16]]; // Scale matrix of Wishart distribution
  double* M;
  M = (double*)malloc(dims[16] * sizeof(double));
  //double M[dims[16]];       // sum of InvSigma and param$V
  double* chol;
  chol = (double*)malloc(dims[16] * sizeof(double));
  //double chol[dims[16]];    // cholesky decomposition of matrix M
  double df;                // degrees of freedom for Wishart distribution
  double* pInvQ;            // pointer to InvQ
  double* Sig;
  Sig = (double*)malloc(dims[16] * sizeof(double));
  //double Sig[dims[16]];     // auxiliary matrix Sigma
  double* pSigma;           // pointer to Sigma  
  
  /*
  for(j = 0; j < dimswithK[9]; j++){
    printf("%d = %f\n", j, (*last).InvSigma[j]);
  }
  */
  
  // Is InvSigma class specific?
  if(spec[2]){
    
    for(g = 0; g < *G; g++){
      /* Setting pointers */
      // InvQ
      pInvQ = (*last).InvQ;
      if(spec[3]){
        pInvQ += g*dims[21];
      }
      
      // find the scale matrix
      for(l = 0; l < *totnran; l++){
        for(j = l; j < *totnran; j++){
          coord = l + j*(j+1)/2;
          // inicialization
          M[coord] = 0.0;
          // add (b-mu)T(b-mu)
          for(i = 0; i < *n; i++){
            // add only if this subject lies in category g
            if((*last).U[i] == g){
              M[coord] += ((*last).b[*totnran * i + l]) * ((*last).b[*totnran * i + j]);
            }
          }
          // prior contribution
          M[coord] += pInvQ[coord];
          
        }
      }
     
      // positive definite matrix M now needs to be inversed
      justcholesky(M, chol, totnran);
      choltoinv(chol, Smatrix, totnran);
      
      // cholesky decomposition of Smatrix for generating from wishart
      justcholesky(Smatrix, chol, totnran);
      df = nUg[g] + *((*param).nu_0);
      
      /*
        printf("%p\n", (*last).InvSigma + k*dims[9]);
        printf("%p\n", &((*last).InvSigma[k*dims[9]]));
      */
      
      // generating from Wishart distribution
      my_rwishart((*last).InvSigma + g*dims[16], chol, &df, totnran);

      // calculate possible parameters
      // we will surely need cholesky decomposition
      justcholesky((*last).InvSigma + g*dims[16], chol, totnran);
      detchol(chol, (*last).detInvSigma + g, totnran);
      
      
      if(whatsave[17]){
        choltoinv(chol, Sigma + *m * dimswithG[16] + g*dims[16], totnran);
      }
      
      if(whatsave[20]){
        detInvSigma[*m * *G + g] = (*last).detInvSigma[g];
      }
      
      if(whatsave[18] || whatsave[19]){
        // setting pointer to Sigma matrix
        if(whatsave[17]){
          pSigma = Sigma + *m * dimswithG[17] + g*dims[17];
        }else{ // inverse was not calculated yet
          choltoinv(chol, Sig, totnran);
          pSigma = Sig;
        }
        
        /*
        for(l = 0; l < *totnran; l++){
          for(j = l; j < *totnran; j++){
            printf("%f, ", Sig[l + j*(j+1)/2]);
          }
          printf("\n");
        }
        */
        
        if(whatsave[18]){
          for(j = 0; j < *totnran; j++){
            sdSigma[*m * dimswithG[18] + g*dims[18] + j] = sqrt(pSigma[j*(j+3)/2]);
          }
        }
        
        
        
        if(whatsave[19]){
          for(l = 0; l < *totnran; l++){
            for(j = l+1; j < *totnran; j++){
              corSigma[*m * dimswithG[19] + g*dims[19] + l + j*(j-1)/2] = pSigma[l + j*(j+1)/2] / sqrt(pSigma[j*(j+3)/2] * pSigma[l*(l+3)/2]); 
            }
          }
        }
      }
    } // end of for g
    
    //---------------------------------------------------------
  }else{
    // InvSigma is just one for all classes --> so must be InvQ
    pInvQ = (*last).InvQ;
    
    // find the scale matrix
    for(l = 0; l < *totnran; l++){
      for(j = l; j < *totnran; j++){
        coord = l + j*(j+1)/2;
        // inicialization
        M[coord] = 0.0;
        // add (b-mu)T(b-mu)
        for(i = 0; i < *n; i++){
          M[coord] += ((*last).b[*totnran * i + l])* ((*last).b[*totnran * i + j]);
        }
        // prior contribution
        M[coord] += pInvQ[coord];
        
      }
    }
    //printf("\ngibbs_InvSigma:");
    //for(l = 0; l < *totnran; l++){
    //  printf("\n");
    //  for(j = l; j < *totnran; j++){
    //    printf("M[%d,%d] = %f, ", l, j, M[l + j*(j+1)/2]);
    //  }
    //}
    //fflush(stdout);
    
    // positive definite matrix M now needs to be inversed
    justcholesky(M, chol, totnran);
    choltoinv(chol, Smatrix, totnran);
    //printf("\ngibbs_InvSigma:");
    //for(l = 0; l < *totnran; l++){
    //  printf("\n");
    //  for(j = l; j < *totnran; j++){
    //    printf("Smatrix[%d,%d] = %f, ", l, j, Smatrix[l + j*(j+1)/2]);
    //  }
    //}
    //fflush(stdout);
    
    // cholesky decomposition of Smatrix for generating from wishart
    justcholesky(Smatrix, chol, totnran);
    df = *n + *((*param).nu_0);
    
    // generating from Wishart distribution
    my_rwishart((*last).InvSigma, chol, &df, totnran);
    
    // calculate possible parameters
    // we will surely need cholesky decomposition
    justcholesky((*last).InvSigma, chol, totnran);
    detchol(chol, (*last).detInvSigma, totnran);
    
    if(whatsave[17]){
      choltoinv(chol, Sigma + *m * dimswithG[17], totnran);
    }
    
    if(whatsave[20]){
      detInvSigma[*m] = (*last).detInvSigma[0]; // [0] needs to be there to not to return adress but the value pointed by this adress
    }
    
    if(whatsave[18] || whatsave[19]){
      // setting pointer to Sigma matrix
      if(whatsave[17]){
        pSigma = Sigma + *m * dimswithG[17];
      }else{ // inverse was not calculated yet
        choltoinv(chol, Sig, totnran);
        pSigma = Sig;
      }
      
      if(whatsave[18]){
        for(j = 0; j < *totnran; j++){
          sdSigma[*m * dimswithG[18] + j] = sqrt(pSigma[j*(j+3)/2]);
        }
      }
      
      if(whatsave[19]){
        for(l = 0; l < *totnran; l++){
          for(j = l+1; j < *totnran; j++){
            corSigma[*m * dimswithG[19] + l + j*(j-1)/2] = pSigma[l + j*(j+1)/2] / sqrt(pSigma[l*(l+3)/2] * pSigma[j*(j+3)/2]); 
          }
        }
      }
    }
  } // end of if(spec["InvSigma"])
  
  free(Smatrix);
  free(M);
  free(chol);
  free(Sig);
  
} // end of void 
