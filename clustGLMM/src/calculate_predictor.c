/* Predictor calculation */
#include <R.h>
#include <Rmath.h>
#include <stdio.h>

void calculate_predictor_separately( 
    /** IN parameters **/
    int* Id,        // [N]          IDs
    double* X,      // [N*#regr]    ID + regressors
    /** Output **/
    double* predictor,      //   OUT       [*N * *nY] current value of predictor
    int* update,      // [4] 0/1 - update Fixed, Group-specific, Random, Offset part
    /** Parameters describing dimensions **/
    int* N,           // [1]        total number of observations
    int* nY,          // [1]        total number of responses
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
    int* beta_dim,    // [1]        dimension of single group-specific beta parameter
    int* K,           // [nY]        number of categories (K=1 if Num,Poi,Bin,Ord, but K=Kcat for Cats)
    int* kspec_bi_cat,// [1]        are random effects k-specific
    //** Arrays with necessary parameters from one state **/
    double* beta_fix,     // [totnfix ]       Beta parameters for fixed effects
    double* beta,         // [totngrp * G]    Beta parameters for group-specific effects
    double* b,            // [totnran * n]    Random effects
    double* U                // [n]              What is the current class of i-th subject
)
{
  /*** Declarations ***/
  /*** ------------ ***/
  int y;                // number of current response
  int i1, i2;           // looping indeces
  int k;                // category
  int coord;            // coordinate to not to repeat its calculation
  int g;                // number of class (category)
  int id;               // number of ID
  int xdim, bdim;
  int bfix, bgrp, bran;
  
  /*** Predictor calculation ***/
  /*** --------------------- ***/
  //printf("\nAhoj");
  //fflush(stdout);
  
  coord = 0;
  bfix = bgrp = bran = 0;
  for(y = 0; y < *nY; y++){
    for(k = 0; k < K[y]; k++){
      for(i1 = 0; i1 < *N; i1++){
        //printf("\ny=%d, k=%d, i1=%d", y, k, i1);
        //fflush(stdout);
        // What is the class this observation belongs to?
        id = Id[ i1 ];            
        g = U[ id ]; 
        
        // Fixed part - position + 0
        if(update[0]){
          predictor[coord] = 0.0;
          //if(i1 < 10){
          //  printf("\ncalculate_predictor: id = %d, y = %d, k = %d, bfix = %d", id, y, k, bfix);
          //}
          
          for(i2 = 0; i2 < nfix[y]; i2++){
            xdim = i1 + *N * FormulaF[i2 + cumnfix[y]];
            //if(i1 < 10){
            //  printf("\ncalculate_predictor: nfix=%d, cumnfix=%d, N=%d, xdim=%d, bfix+i2=%d", 
            //         nfix[y], cumnfix[y], *N, xdim, bfix+i2);
            //}
            // add i2-th regressor for y multiplied by corresponding beta
            predictor[coord] += X[xdim] * beta_fix[bfix+i2];
          }
        }
        coord++;
        
        // Group-specific part - position + 1
        if(update[1]){
          predictor[coord] = 0.0;
          bdim = bgrp + *beta_dim * g;
          //if(i1 < 10){
          //  printf("\ncalculate_predictor: id = %d, g = %f,%d, bdim = %d", id, U[id], g, bdim);
          //}
          
          for(i2 = 0; i2 < ngrp[y]; i2++){
            xdim = i1 + *N * FormulaG[i2 + cumngrp[y]];
            // add i2-th regressor for y multiplied by corresponding beta
            predictor[coord] += X[xdim] * beta[bdim];
            bdim++;
          }
          
        }
        coord++;
        
        // Random part - position + 2
        if(update[2]){
          predictor[coord] = 0.0;
          
          bdim = *totnran * id + cumnran[0] + bran;
          //if((i1 < 10) | (i1 > *N-10)){
          //  printf("\ncalculate_predictor: id=%d, g=%d, totnran=%d, cumnran[0]=%d, bran=%d, bdim=%d", 
          //        id, g, *totnran, cumnran[0], bran, bdim);
          //}
          for(i2 = 0; i2 < nran[y]; i2++){
            xdim = i1 + *N * FormulaR[i2 + cumnran[y]];
            // add i2-th regressor for y multiplied by corresponding b
            predictor[coord] += X[xdim] * b[bdim];
            bdim++;
          }
        }
        coord++;
        
        // Offset part - position + 3
        if(update[3]){
          predictor[coord] = 0.0;
          
          for(i2 = 0; i2 < noff[y]; i2++){
            xdim = i1 + *N * FormulaO[i2 + cumnoff[y]];
            // add i2-th offset regressor 
            predictor[coord] += X[xdim];
          }
        }
        coord++;
      }
      
      bfix += nfix[y];
      bgrp += ngrp[y];
      if(*kspec_bi_cat){
        // add nran for each category k of Cat outcome
        bran += nran[y];
      }
    } // end for k
    if(!(*kspec_bi_cat)){
      // add nran only once (Cat has kspec_bi_cat = F) or (only one if K==1 - NumPoiBinOrd)
      bran += nran[y];
    }
  } // end for y 
   
} // end of function calculate_predictor
