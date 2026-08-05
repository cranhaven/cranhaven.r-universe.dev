/*
 * Generating betas from the conditioned distribution 
 */


#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"
#include "matrmult.h"
#include "cholesky.h"
#include "calculate_predictor.h"


void gibbs_beta_num(struct str_state* last,   // OUT+IN last known values of generated parameters
                    struct str_param* param,  // IN hyperparameters
                    int* Id,                  // [N] IDs
                    double* Y,                // IN [*N * totnY]
                    double* X,                // [N*(#regr)]    ID + regressors
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
                    int* G,                   // IN [1] number of classes
                    int* N,                   // IN [1] number of observations
                    int* n,                   // IN [1] number of subjects
                    int* nY,                  // IN [1]        counts of Nums variables
                    double* predictor_num,    // IN [N*2*nY]  both F and R parts of predictor for numeric outcomes 
                    int* FormulaF,    // [sum(nfix)]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                    int* FormulaG,    // [sum(ngrp)]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                    int* FormulaR,    // [sum(nran)]  numbers of columns of X that should be used for RANDOM effects of modelled responses
                    int* FormulaO,    // [sum(noff)]  numbers of columns of X that should be used for OFFSET effects of modelled responses
                    int* nfix,        // [sum(nY)]  number of FIXED  regressors for each response
                    int* ngrp,        // [sum(nY)]  number of GROUP-SPECIFIC  regressors for each response
                    int* nran,        // [sum(nY)]  number of RANDOM regressors for each response
                    int* noff,        // [sum(nY)]  number of OFFSET regressors for each response
                    int* cumnfix,     // []         cummulative sums of nfix values of all together (for Formula)
                    int* cumngrp,     // []         cummulative sums of ngrp values of all together (for Formula)
                    int* cumnran,     // []         cummulative sums of nran values of all together (for Formula)
                    int* cumnoff,     // []         cummulative sums of noff values of all together (for Formula)
                    int* totnran,     // [1]        total dimension of one single b_i
                    int* n_i,                 // IN [n] number of observations dedicated to j-th subject
                    int* i_j,                 // IN [n * max_n_j] indeces dedicated to j-th subject 
                    int* nUg,                 // IN [G] number of subjects     in classes 1, ..., G
                    int* listUi               // IN [n*G] list of who belongs to which cluster
                )    
{ 
  // Declarations //
  
  int i, j, l, g, p, q;             // looping indeces
  int y;                            // variable index
  int coord;                        // coordinate for matrices as arrays
  int row, col, col1, col2;         // indeces to orient in the symmetric matrix
  int maxdim;
  
  if(dims[0] > dims[1]){
    maxdim = dims[0];
  }else{
    maxdim = dims[1];
  }
  
  double* chol;
  chol = (double*)malloc(maxdim * (maxdim + 1)/2 * sizeof(double));
  //double chol[maxdim * (maxdim + 1)/2];        // cholesky decomposition of matrix M
  //double betahat[*totnfix];         // mean value of the normal distribution
  double* almost_betahat;
  almost_betahat = (double*)malloc(maxdim * sizeof(double));
  //double almost_betahat[maxdim];  // chol^{-T} * bhat --> to be backsolved to get muhat
  double* rightb;
  rightb = (double*)malloc(maxdim * sizeof(double));
  //double rightb[maxdim];          // right-hand side of the equation to get muhat
  double* rvec;
  rvec = (double*)malloc(maxdim * sizeof(double));
  //double rvec[maxdim];            // vector of generated N(0,1) values
  double* scaled_rvec;
  scaled_rvec = (double*)malloc(maxdim * sizeof(double));
  //double scaled_rvec[maxdim];     // vector of generated N(0,Sigma) values
  double* XtX;
  XtX = (double*)malloc(maxdim * (maxdim + 1)/2 * sizeof(double));
  //double XtX[maxdim * (maxdim + 1)/2]; // XtX + diag(1/param.fixD)
  double* modY;
  modY = (double*)malloc(*N * sizeof(double));
  //double modY[*N];                  // modified response (Y/latent - b*Z)
  double* pY;                       // pointer to Y
  double* ppred;                    // pointer to predictor
  double* ptau;                       // tau value to multiply XtX
  double tau;
  
  ////-----------------------------////
  //// Fixed - beta_num_fix - part ////
  ////-----------------------------////
  
  for(y = 0; y < nY[0]; y++){
    if(nfix[y] > 0){
      // there are some fixed covariates
      // pointer to the y-th numeric variable
      pY = Y + *N * y;
      ppred = predictor_num + *N * y * 4;
      
      // modified response = response - rand effects* regressors
      for(j = 0; j < *N; j++){
        // cycle through all observations
        // substracting other parts of predictor
        modY[j] = pY[j]; 
        modY[j] -= ppred[1 + 4*j]; // group-spec part
        modY[j] -= ppred[2 + 4*j]; // random part
        modY[j] -= ppred[3 + 4*j]; // offset part 
        //modY[j] -= predictor_num[1 + 4*(j + *N * y)]; // group-spec part
        //modY[j] -= predictor_num[2 + 4*(j + *N * y)]; // random part
        //modY[j] -= predictor_num[3 + 4*(j + *N * y)]; // offset part 
      }
      
      // pointer to tau --> later add g*dims[2] if group-specific
      ptau = (*last).prec_num + y;
      
      // XtX + D calculation
      for(p = 0; p < nfix[y]; p++){
        for(q = p; q < nfix[y]; q++){
          coord = p + q*(q+1)/2;
          // start with fixD and then add XtX
          XtX[coord] = (p == q) / (*((*param).sd_beta_num_fix) * *((*param).sd_beta_num_fix)); // 0 off diagonal, 1/fixD on the diagonal
          
          col1 = FormulaF[cumnfix[y] + p];
          col2 = FormulaF[cumnfix[y] + q];
          
          for(i = 0; i < *n; i++){
            g = (*last).U[i];
            if(spec[0]){
              // prec_num is group-specific
              tau = ptau[g*dims[2]];
            }else{
              tau = *ptau;
            }
            // cycle through all subjects
            for(j = 0; j < n_i[i]; j++){
              // cycle through observations dedicated to subject j
              row = i_j[i + *n * j];
              XtX[coord] += tau * X[row + *N * col1] * X[row + *N * col2];
            }
          } 
        }
      } // end of XtX + 1/sd^2 calculation
      
      // rightb calculation
      for(p = 0; p < nfix[y]; p++){
        rightb[p] = 0.0;
        col = FormulaF[cumnfix[y] + p];
        // now add t(X) * modY
        for(i = 0; i < *n; i++){
          g = (*last).U[i];
          if(spec[0]){
            // prec_num is group-specific
            tau = ptau[g*dims[2]];
          }else{
            tau = *ptau;
          }
          for(j = 0; j < n_i[i]; j++){
            // cycle through observations dedicated to subject j
            row = i_j[i + *n * j];
            rightb[p] += tau * X[row + *N * col] * modY[row];
          }
        }
      }
  
      // mean value calculation
      cholesky_solve(XtX, nfix + y, rightb, chol, almost_betahat);
      
      // now we have mean value (betahat) and also the inverse of variance matrix (XtX)
      for(l = 0; l < nfix[y]; l++){
        rvec[l] = almost_betahat[l] + rnorm(0.0, 1.0);
      }
      // scale it 
      backsolve2(chol, rvec, nfix + y, scaled_rvec);
      
      // add mean value and store to last
      for(l = 0; l < nfix[y]; l++){
        (*last).beta_num_fix[l + cumnfix[y]] = scaled_rvec[l];
      }
    }else{
      // there are no effects fixed for all clusters
      // nothing to be updated
    }
    
  }
  
  ////-----------------------------////
  //// Fixed predictor part update ////
  ////-----------------------------////
  
  int update[4];
  int* K_beta;
  K_beta = (int *)malloc(nY[0] * sizeof(int));
  //int K_beta[nY[0]];
  int kspec_bi_cat = 0;
  update[0] = 1; // only fixed part of predictor is to be updated
  update[1] = update[2] = update[3] = 0;
  for(y = 0; y < nY[0]; y++){
    K_beta[y] = 1;
  }
  
  calculate_predictor_separately(
    Id, X, 
    predictor_num, update,
    // Parameters describing dimensions //
    N, nY +0, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix, ngrp, nran, noff, 
    cumnfix, cumngrp, cumnran, cumnoff, 
    totnran,
    dims+1,     // dimension of single group-specific beta_num
    K_beta, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_num_fix, (*last).beta_num, (*last).b, (*last).U            
  );
  
  ////----------------------------------////
  //// Group-specific - beta_num - part ////
  ////----------------------------------////
  
  for(y = 0; y < nY[0]; y++){
    //printf("\nAhoj podruhe");
    if(ngrp[y] > 0){
      // there are some group-specific effects for this outcome  
    
      // pointer to the y-th numeric variable
      pY = Y + *N * y;
      ppred = predictor_num + *N * y * 4;
      
      // modified response = response - rand effects* regressors
      for(j = 0; j < *N; j++){
        // cycle through all observations
        // substracting other parts of predictor
        modY[j] = pY[j]; 
        modY[j] -= ppred[4*j];     // fixed part
        modY[j] -= ppred[2 + 4*j]; // random part
        modY[j] -= ppred[3 + 4*j]; // offset part 
        //modY[j] -= predictor_num[4*(j + *N * y)];     // fixed part
        //modY[j] -= predictor_num[2 + 4*(j + *N * y)]; // random part
        //modY[j] -= predictor_num[3 + 4*(j + *N * y)]; // offset part 
      }
      
      for(g = 0; g < *G; g++){
        
        // Is tau class specific? 
        if(spec[0]){
          tau = (*last).prec_num[y + g * dims[2]];
        }else{
          tau = (*last).prec_num[y];
        }
        //printf("\ngibbs_beta_num: tau = %f", tau);
        //fflush(stdout);
        
        // XtX (for class g) + D calculation
        for(p = 0; p < ngrp[y]; p++){
          for(q = p; q < ngrp[y]; q++){
            coord = p + q*(q+1)/2;
            // prior contribution
            XtX[coord] = ((p == q) / (*((*param).sd_beta_num) * *((*param).sd_beta_num))); // 0 off diagonal, 1/sd2 on the diagonal
            
            col1 = FormulaG[cumngrp[y] + p];
            col2 = FormulaG[cumngrp[y] + q];
            //printf("\ngibbs_beta_num: g=%d, col1=%d, col2=%d, coord=%d", g, col1, col2, coord);
            //fflush(stdout);
            
            for(l = 0; l < nUg[g]; l++){
              // cycle through subjects... but only those that are in the class k
              // j-th individual in g-th cluster then has number listUi[j+*n*g]
              i = listUi[l + *n * g];
              for(j = 0; j < n_i[i]; j++){
                // cycle through observations dedicated to subject j
                row = i_j[i + *n * j];
                //printf("\ngibbs_beta_num: i=%d, j=%d, row=%d", i, j, row);
                //fflush(stdout);
                XtX[coord] += X[row + *N * col1] * X[row + *N * col2];
              }
            }
            XtX[coord] *= tau; 
          }
        } // end of XtX + 1/sd2 calculation
        
        //for(p = 0; p < ngrp[y]; p++){
        //  printf("\nXtX:");
        //  for(q = p; q < ngrp[y]; q++){
        //    coord = p + q*(q+1)/2;
        //    printf("[%d] = %f", coord, XtX[coord]);
        //  }
        //} // end of XtX + 1/sd2 calculation
        //fflush(stdout);
        
        
        // rightb calculation
        for(p = 0; p < ngrp[y]; p++){
          // prior mean value for beta is supposed to be 0
          rightb[p] = 0.0;
          col = FormulaG[cumngrp[y] + p];
          // now add t(X) * modY
          for(l = 0; l < nUg[g]; l++){
            // cycle through subjects... but only those that are in the class g
            i = listUi[l + *n * g];
            for(j = 0; j < n_i[i]; j++){
              // cycle through observations dedicated to subject j
              row = i_j[i + *n * j];
              rightb[p] += X[row + *N * col] * modY[row];
            }
          }
          rightb[p] *= tau;
        }
        
        //for(p = 0; p < ngrp[y]; p++){
        //  printf("\nXtX:");
        //  for(q = p; q < ngrp[y]; q++){
        //    coord = p + q*(q+1)/2;
        //    printf("[%d] = %f", coord, XtX[coord]);
        //  }
        //} // end of XtX + 1/sd2 calculation
        //fflush(stdout);
        
        // mean value calculation
        cholesky_solve(XtX, ngrp + y, rightb, chol, almost_betahat);
        
        // now we have mean value (betahat) and also the inverse of variance matrix (XtX)
        for(l = 0; l < ngrp[y]; l++){
          rvec[l] = almost_betahat[l] + rnorm(0.0, 1.0);
        }
        
        // scale it 
        backsolve2(chol, rvec, ngrp + y, scaled_rvec);
        
        // add mean value and store to last
        for(l = 0; l < ngrp[y]; l++){
          (*last).beta_num[l + g * dims[1] + cumngrp[y]] = scaled_rvec[l];
        }
        
      } // end of for g
      
    }else{
      // there are no group-specific effects for this outcome
    }
  } // end of for y
  
  
  ////--------------------------------------////
  //// Group-specific predictor part update ////
  ////--------------------------------------////
  
  update[1] = 1; // only group-specific part of predictor is to be updated
  update[0] = update[2] = update[3] = 0;
  
  calculate_predictor_separately(
    Id, X, 
    predictor_num, update,
    // Parameters describing dimensions //
    N, nY +0, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix, ngrp, nran, noff, 
    cumnfix, cumngrp, cumnran, cumnoff, 
    totnran,
    dims+1,     // dimension of single group-specific beta_num
    K_beta, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    (*last).beta_num_fix, (*last).beta_num, (*last).b, (*last).U            
  );
  
  
  free(chol);
  free(almost_betahat);
  free(rightb);
  free(rvec);
  free(scaled_rvec);
  free(XtX);
  free(modY);
  free(K_beta);
  
  
} // end of void 
