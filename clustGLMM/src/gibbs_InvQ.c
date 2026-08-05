/*
 * Generating InvQ from the conditioned distribution 
 */


#include <R.h>
#include <Rmath.h>
#include <stdio.h>


#include "myrwishart.h"
#include "matrmult.h"
#include "cholesky.h"

#include "structures.h"

void gibbs_InvQ(struct str_state* last,  // OUT+IN last known values of generated parameters
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
                int* totnran,           // IN [1] total number of random regressors
                int* m,                 // IN [1] iteration number
                double* Q,              // [(B+M)*totnran*(totnran+1)/2(* G)] Prior scale matrix of InvSigma
                double* detInvQ         // [(B+M)(* G)] determinant of InvQ
                )    
{ 
  /** Declarations **/
  int j, g;                 // looping indices
  int coord;                // coordinate for matrices as arrays
  double* Smatrix;
  Smatrix = (double*)malloc(dims[21] * sizeof(double));
  //double Smatrix[dims[21]]; // Scale matrix of Wishart distribution
  double* M;
  M = (double*)malloc(dims[21] * sizeof(double));
  //double M[dims[21]];       // sum of InvSigma and param$V
  double* chol;
  chol = (double*)malloc(dims[21] * sizeof(double));
  //double chol[dims[21]];    // cholesky decomposition of matrix M
  double degf;                // degrees of freedom for Wishart distribution
  
  //printf("\ngibbs_InvQ: Ahoj");
  // Is InvQ class specific?
  if(spec[3]){
    // and so is InvSigma...
    
    for(g = 0; g < *G; g++){
      // find the scale matrix
      for(j = 0; j < dims[21]; j++){
        M[j] = (*last).InvSigma[j + g*dims[16]] + (*param).InvV[j];
      }

      justcholesky(M, chol, totnran);
      choltoinv(chol, Smatrix, totnran);
      
      // cholesky decomposition of Smatrix for generating from wishart
      justcholesky(Smatrix, chol, totnran);
        
      degf = *((*param).nu_0) + *((*param).nu_1);
      
      my_rwishart((*last).InvQ + g*dims[21], chol, &degf, totnran);
      
      // calculate possible parameters
      if( whatsave[22] || whatsave[23]){
        justcholesky((*last).InvQ + g*dims[21], chol, totnran);
      }
      
      if(whatsave[22]){
        choltoinv(chol, Q + *m * dimswithG[22] + g*dims[22], totnran);
      }
      
      if(whatsave[23]){
        coord = *m * *G + g;
        detchol(chol, detInvQ + coord, totnran);
      }
      
    }
  }else{
    // InvQ is just one for all classes
    
    // Is InvSigma class-specific?
    if(spec[2]){
      // inicialization of Smatrix
      for(j = 0; j < dims[21]; j++){
        M[j] = (*param).InvV[j];
      }
      // add InvSigma for each class
      for(g = 0; g < *G; g++){
        for(j = 0; j < dims[21]; j++){
          M[j] += (*last).InvSigma[j + g * dims[16]];
        }
      }
      
      degf = *G * *((*param).nu_0) + *((*param).nu_1);
    }else{
      
      for(j = 0; j < dims[21]; j++){
        M[j] = (*param).InvV[j] + (*last).InvSigma[j];
      }
      
      degf = *((*param).nu_0) + *((*param).nu_1);
    } // end of if(spec["InvSigma"])
    
    //printf("\ngibbs_InvQ:");
    //for(j = 0; j < *totnran; j++){
    //  printf("\n");
    //  for(int k = j; k < *totnran; k++){
    //    printf("M[%d,%d] = %f, ", j, k, M[j + k*(k+1)/2]);
    //  }
    //}
    //fflush(stdout);
    
    // finding the inverse
    justcholesky(M, chol, totnran);
    choltoinv(chol, Smatrix, totnran);
    
    // cholesky decomposition of Smatrix for generating from wishart
    justcholesky(Smatrix, chol, totnran);
    choltoinv(chol, Smatrix, totnran);
    
    my_rwishart((*last).InvQ, chol, &degf, totnran);
      
    
    // calculate possible parameters
    if( whatsave[22] || whatsave[23] ){
      justcholesky((*last).InvQ, chol, totnran);
    }
    
    if(whatsave[22]){
      choltoinv(chol, Q + *m * dimswithG[22], totnran);
    }
    
    if(whatsave[23]){
      detchol(chol, detInvQ + *m, totnran);
    }
     
  } // end of if(spec["InvQ"])
  
  free(Smatrix);
  free(M);
  free(chol);
  
} // end of void 
