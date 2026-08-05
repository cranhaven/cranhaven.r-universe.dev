/*
 * Newton-Raphson method for maximization of function dependant on vector paramter theta=b_i
 * using first and second order derivatives
 * Only for random effects that are a bit specific
 */
#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "matrmult.h"
#include "my_math.h"
#include "cholesky.h"

#include "pdfs_derivatives_bi_dev.h"

void newton_raphson_bi_dev(double* Y,                 // IN [*N]
                           int* isYna,                // [N*sum(nY)]  0 = Y value present, 1 = Y value is NA
                           double* X,                 // IN [N*(#regr)]    regressors
                           double* pbeta_num_fix,     // pointer to beta_num_fix
                           double* pbeta_num,         // pointer to beta_num
                           double* pprec_num,         // pointer to prec_num
                           double* pbeta_poi_fix,     // pointer to beta_poi_fix
                           double* pbeta_poi,         // pointer to beta_poi
                           double* pbeta_bin_fix,     // pointer to beta_bin_fix
                           double* pbeta_bin,         // pointer to beta_bin
                           double* pbeta_ord_fix,     // pointer to beta_ord_fix
                           double* pbeta_ord,         // pointer to beta_ord
                           double* pc_ord,            // pointer to c_ord
                           double* pbeta_cat_fix,     // pointer to beta_cat_fix
                           double* pbeta_cat,         // pointer to beta_cat
                           double* pInvSigma,         // pointer to InvSigma
                           int* N,                    // IN [1] number of observations
                           int* n,                    // IN [1] number of subjects
                           int* nY,                   // IN [5]
                           int* i,                    // IN [1] number of subject 
                           int* Kord,                 // IN [nY[3]] total number of categories of Ord outcomes -1
                           int* Kcat,                 // IN [nY[4]] total number of categories of Cat outcomes -1
                           int* nfix,                 // IN [totnY] number of FIXED  regressors or each response
                           int* cumnfix,              // IN [totnY] where to start in FormulaF
                           int* FormulaF,             // IN [totnfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                           int* ngrp,                 // IN [totnY] number of GROUP-SPECIFIC  regressors or each response
                           int* cumngrp,              // IN [totnY] where to start in FormulaG
                           int* FormulaG,             // IN [totnfix]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                           int* nran,                 // IN [totnY] number of RANDOM  regressors or each response
                           int* cumnran,              // IN [totnY] where to start in FormulaR
                           int* totnran,              // IN [1] total dimension of b_i
                           int* FormulaR,             // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                           int* noff,                 // IN [totnY] number of OFFSET  regressors or each response
                           int* cumnoff,              // IN [totnY] where to start in FormulaO
                           int* FormulaO,             // IN [totnran]  numbers of columns of X that should be used for OFFSET  effects of modelled responses
                           int* ij,                   // IN [1] number of first ij observations of j-th subject to be used
                           int* i_j,                  // IN [n * max_n_j] indeces dedicated to j-th subject 
                           int* kspec_bi_cat,         // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                           //        FALSE = all levels of cat var have one set of bi - compares last category with others
                           double* theta_max,         // OUT [dims[.]] theta maximizing the function
                           double* chol_at_theta_max, // OUT [dims[.] * (dims[.]+1) / 2]
                           // Tuning parameters
                           double* tolerance,         // IN [1] tolerance in the norm of change
                           int* maxiter,              // [1] maximum iterations allowed during Newton-Raphson update of proposal distribution
                           int* maxnrep,              // [1] maximum number of repetitions of Newton-Raphson from different starting pointsint* iter,                 // OUT [1] number of iterations performed before convergence
                           int* iter,                 // OUT [1] number of iterations performed before convergence
                           int* converged,            // OUT [1] indicator of convergence
                           double* max_value          // OUT [1] the value of maximized log-likelihood
){
  
  // Auxiliary parameters
  int j, k;
  int nrep;
  int cont;
  int cont_half;
  int print = 0;
  double u;
  
  //printf("\nnewton_raphson_bi: totnran = %d", *totnran);
  
  double* almost_shift;
  almost_shift = (double*)malloc(*totnran * sizeof(double));
  //double almost_shift[*totnran];
  double* shift;
  shift = (double*)malloc(*totnran * sizeof(double));
  //double shift[*totnran];
  double* right_side;
  right_side = (double*)malloc(*totnran * sizeof(double));
  //double right_side[*totnran];
  double* chol;
  chol = (double*)malloc((*totnran) * ((*totnran) + 1)/2 * sizeof(double));
  //double chol[(*totnran) * ((*totnran) + 1)/2];
  double norm_shift = 0.0;
  double* var;
  var = (double*)malloc((*totnran) * ((*totnran) + 1)/2 * sizeof(double));
  //double var[(*totnran) * ((*totnran) + 1)/2];
  double* prev_max;
  prev_max = (double*)malloc(*totnran * sizeof(double));
  double* theta_new;
  theta_new = (double*)malloc(*totnran * sizeof(double));
  //double prev_max[*totnran];
  double loglik_max;
  double loglik_new;
  
  // save previous max value --> in case we would start not from that point, but rather point "near"
  for(k = 0; k < *totnran; k++){
    prev_max[k] = theta_max[k];
  }
  
  //printf("\nnewton_raphson_bi_dev: n = %d,  n_j[%d] = %d", *n, *i, *ij);
  int bad_start = 1;
  nrep = 0;
  
  while(bad_start){
    
    logpbi_dev(Y, isYna, X,  
               pbeta_num_fix, pbeta_num, pprec_num, 
               pbeta_poi_fix, pbeta_poi, 
               pbeta_bin_fix, pbeta_bin, 
               pbeta_ord_fix, pbeta_ord, pc_ord, 
               pbeta_cat_fix, pbeta_cat, 
               pInvSigma,
               N, n, nY, i, Kord, Kcat, 
               nfix, cumnfix, FormulaF,
               ngrp, cumngrp, FormulaG,
               nran, cumnran, totnran, FormulaR,
               noff, cumnoff, FormulaO,
               ij, i_j, 
               kspec_bi_cat, &print, theta_max, max_value);
    
    if(!isfinite(*max_value)){
      bad_start = 1;
      print = 1;
      nrep++;
      //printf("\n");
      //printf("\nnewton_raphson_bi_dev: nrep = %d, i=%d, max_value = %f, b[%d] = theta:  ", 
      //       nrep, *i, *max_value, *i);
      //for(k=0; k < *totnran; k++){
      //  printf("%d = %f, ", k, theta_max[k]);
      //}
      //printf("\nnewton_raphson_bi_dev: nrep = %d, i=%d, b[%d] = newtheta:  ", 
      //       nrep, *i, *i);
      for(k=0; k < *totnran; k++){
        theta_max[k] = rnorm((0.0 + prev_max[k])/(1+nrep),0.5+1/nrep);
        //printf("%d = %f, ", k, theta_max[k]);
      }
      //fflush(stdout);
      
    }else{
      print= 0;
      bad_start = 0;
      loglik_max = *max_value;
      for(k = 0; k < *totnran; k++){
        prev_max[k] = theta_max[k];
      }
    }
    if(nrep == *maxnrep){
      //printf("\nnewton_raphson_bi_dev: Did not find any suitable start for N-R method for i=%d", *i);
      //fflush(stdout);
      
      for(k = 0; k < *totnran; k++){
        prev_max[k] = theta_max[k] = 0.0;
      }
      logpbi_dev(Y, isYna, X,  
                 pbeta_num_fix, pbeta_num, pprec_num, 
                 pbeta_poi_fix, pbeta_poi, 
                 pbeta_bin_fix, pbeta_bin, 
                 pbeta_ord_fix, pbeta_ord, pc_ord, 
                 pbeta_cat_fix, pbeta_cat, 
                 pInvSigma,
                 N, n, nY, i, Kord, Kcat, 
                 nfix, cumnfix, FormulaF,
                 ngrp, cumngrp, FormulaG,
                 nran, cumnran, totnran, FormulaR,
                 noff, cumnoff, FormulaO,
                 ij, i_j, 
                 kspec_bi_cat, &print, prev_max, &loglik_max);
      *max_value = loglik_max;
      if(isfinite(*max_value)){
        bad_start = 0;
      }else{
        bad_start = 1;
        nrep = 0; /// THIS MAY LEAD TO INFINITE CYCLE, IF NO SUITABLE START EXISTS!
      }
      //break;
    }
  } // end of while
  
  
  *converged = 0;
  nrep = 0;
  cont = 1;
  
  for(j = 0; ((j < *maxiter) & (!(*converged)) & cont); j++){
    if(print){
      //printf("\nnewton_raphson_bi_dev: j = %d, nrep = %d", j, nrep);
    }
    // First compute the right-hand side of equation
    // which is the gradient of fun at current value of theta
    // Compute the symmetric (-)hessian matrix on the left side
    d_d2_logpbi_dev(Y, isYna, X,  
                    pbeta_num_fix, pbeta_num, pprec_num, 
                    pbeta_poi_fix, pbeta_poi, 
                    pbeta_bin_fix, pbeta_bin, 
                    pbeta_ord_fix, pbeta_ord, pc_ord, 
                    pbeta_cat_fix, pbeta_cat, 
                    pInvSigma,
                    N, n, nY, i, Kord, Kcat, 
                    nfix, cumnfix, FormulaF,
                    ngrp, cumngrp, FormulaG,
                    nran, cumnran, totnran, FormulaR,
                    noff, cumnoff, FormulaO,
                    ij, i_j, 
                    kspec_bi_cat,
                    theta_max, right_side, var);
    //if(print){
    //printf("\nnewton_raphson_bi_dev: grad: ");
    //for(k = 0; k < *totnran; k++){
    //  printf("%d = %f, ", k, right_side[k]);
    //}
    //printf("\nnewton_raphson_bi_dev: hess: ");
    //for(k = 0; k < *totnran*(*totnran + 1)/2; k++){
    //  printf("%d = %f, ", k, var[k]);
    //}
    //fflush(stdout);
    //}
    
    // Solve for shift using Cholesky decomposition
    cholesky_solve(var, totnran, right_side, chol, almost_shift);
    backsolve2(chol, almost_shift, totnran, shift);
    //printf("\nnewton_raphson_bi_dev: After solve.\n");
    
    // New theta - shift added to previous max
    for(k = 0; k < *totnran; k++){
      theta_new[k] = theta_max[k] + shift[k];
    }
    
    // Halving cycle
    cont_half = 1;
    while(cont_half){
      // Compute the norm of shift
      norm_shift = 0;
      for(k = 0; k < *totnran; k++){
        norm_shift += shift[k] * shift[k];
      }
      norm_shift = sqrt(norm_shift);
      
      // Evaluate new loglik value
      logpbi_dev(Y, isYna, X,  
                 pbeta_num_fix, pbeta_num, pprec_num, 
                 pbeta_poi_fix, pbeta_poi, 
                 pbeta_bin_fix, pbeta_bin, 
                 pbeta_ord_fix, pbeta_ord, pc_ord, 
                 pbeta_cat_fix, pbeta_cat, 
                 pInvSigma,
                 N, n, nY, i, Kord, Kcat, 
                 nfix, cumnfix, FormulaF,
                 ngrp, cumngrp, FormulaG,
                 nran, cumnran, totnran, FormulaR,
                 noff, cumnoff, FormulaO,
                 ij, i_j,  
                 kspec_bi_cat, &print, theta_new, &loglik_new);
      // Compare the likelihoods
      if((!isfinite(loglik_new)) | (loglik_new < *max_value) ){
        // We have NOT improved the previous solution
        // OR we end up with some Inf nonsense
        // OR the difference in the shift and loglik is already negligible
        
        // Try step-halving
        for(k = 0; k < *totnran; k++){
          shift[k] = 0.5*shift[k];
          theta_new[k] = theta_max[k] + shift[k];
        }
        
        if((isfinite(loglik_new) == 1) & (norm_shift + abs(loglik_new - *max_value) < *tolerance)){
          cont_half = 0;
        }else{
          cont_half = 1;
        }
        
      }else{
        // We have improved the previous solution 
        for(k = 0; k < *totnran; k++){
          theta_max[k] = theta_new[k];
        }
        *max_value = loglik_new;
        cont_half = 0;
        
      }
      
    }
    
    
    // Did we converge?
    // both differences in thetas and logliks have to be negligible
    *converged = (norm_shift + abs(loglik_new - *max_value) < *tolerance);
    
    if(isfinite(*max_value)){
      print = 0;
      if(*max_value > loglik_max){
        for(k = 0; k < *totnran; k++){
          prev_max[k] = theta_max[k];
        }
        loglik_max = *max_value;
      }
      if(print){
        //printf("\nnewton_raphson_bi_dev: prev_max_loglik = %f, max_value = %f, dif = %f", loglik_max, *max_value, loglik_max-*max_value);
        
        //printf("\nnewton_raphson_bi_dev: Iteration %d for b[%d] = theta: ", j, *i);
        //for(k=0; k < *totnran; k++){
        //  printf("%d = %f, ", k, theta_max[k]);
        //}
        //printf("\nnewton_raphson_bi_dev: normshift = %f \n", norm_shift);
        //fflush(stdout);
      }
    }else{
      // We have some Inf problem going on
      print = 1;
      //printf("\n");
      //printf("\nnewton_raphson_bi_dev: i=%d, j=%d, nrep=%dm max_value = %f, b[%d] = theta:  ", 
      //       *i, j, nrep, *max_value, *i);
      //for(k=0; k < *totnran; k++){
      //  printf("%d = %f, ", k, theta_max[k]);
      //}
      // repeat it to show what exactly went wrong
      logpbi_dev(Y, isYna, X,  
                 pbeta_num_fix, pbeta_num, pprec_num, 
                 pbeta_poi_fix, pbeta_poi, 
                 pbeta_bin_fix, pbeta_bin, 
                 pbeta_ord_fix, pbeta_ord, pc_ord, 
                 pbeta_cat_fix, pbeta_cat, 
                 pInvSigma,
                 N, n, nY, i, Kord, Kcat, 
                 nfix, cumnfix, FormulaF,
                 ngrp, cumngrp, FormulaG,
                 nran, cumnran, totnran, FormulaR,
                 noff, cumnoff, FormulaO,
                 ij, i_j, 
                 kspec_bi_cat, &print, theta_max, max_value);
      nrep++;
      // it came to a nonsense --> start all again from different value
      Rprintf("\nnewton_raphson_bi_dev: loglik is infinite i = %d, start over! nrep = %d", *i, nrep);
      //fflush(stdout);
      j = 0;
      u = runif(0.0, 1.0)*runif(0.0, 1.0); // closer to zero
      for(k = 0; k < *totnran; k++){
        //theta_max[k] = rnorm((0.0 + nrep*prev_max[k])/(1+nrep),0.5+1/nrep);
        //theta_max[k] = (0.0 + nrep*prev_max[k])/(1+nrep);
        theta_max[k] = u*prev_max[k];
        // seems that lesser variance and closer to 0 is better
      }
      
      logpbi_dev(Y, isYna, X,  
                 pbeta_num_fix, pbeta_num, pprec_num, 
                 pbeta_poi_fix, pbeta_poi, 
                 pbeta_bin_fix, pbeta_bin, 
                 pbeta_ord_fix, pbeta_ord, pc_ord, 
                 pbeta_cat_fix, pbeta_cat, 
                 pInvSigma,
                 N, n, nY, i, Kord, Kcat, 
                 nfix, cumnfix, FormulaF,
                 ngrp, cumngrp, FormulaG,
                 nran, cumnran, totnran, FormulaR,
                 noff, cumnoff, FormulaO,
                 ij, i_j,  
                 kspec_bi_cat, &print, theta_max, max_value);
      if(*max_value > loglik_max){
        for(k = 0; k < *totnran; k++){
          prev_max[k] = theta_max[k];
        }
        loglik_max = *max_value;
      }
      
      if(nrep == *maxnrep){
        // 10 seem enough, but rather put there 100
        //break;
        cont = 0;
      }
    }
  }
  
  if(nrep < *maxnrep){
    // Cycle has ended by either convergence at j-th step or at maxiter;
    *iter = j;
    *converged = 1;
  }else{
    // do nothing --> chol_at_theta_max will be used from the previous step
    // however, it may not be initialized yet
    *iter = *maxiter;
    *converged = 0;
    // evaluate using the so far best solution prev_max
    for(k = 0; k < *totnran; k++){
      theta_max[k] = prev_max[k];
      *max_value = loglik_max;
    }
  }
  // Update the hess matrix to be at theta_max
  d_d2_logpbi_dev(Y, isYna, X,  
                  pbeta_num_fix, pbeta_num, pprec_num, 
                  pbeta_poi_fix, pbeta_poi, 
                  pbeta_bin_fix, pbeta_bin, 
                  pbeta_ord_fix, pbeta_ord, pc_ord, 
                  pbeta_cat_fix, pbeta_cat, 
                  pInvSigma,
                  N, n, nY, i, Kord, Kcat, 
                  nfix, cumnfix, FormulaF,
                  ngrp, cumngrp, FormulaG,
                  nran, cumnran, totnran, FormulaR,
                  noff, cumnoff, FormulaO,
                  ij, i_j,  
                  kspec_bi_cat,
                  theta_max, right_side, var);
  
  //printf("\nnewton_raphson_bi_dev: grad: ");
  //for(k = 0; k < *totnran; k++){
  //  printf("%d = %f, ", k, right_side[k]);
  //}
  //printf("\nnewton_raphson_bi_dev: hess: ");
  //for(k = 0; k < *totnran*(*totnran + 1)/2; k++){
  //  printf("%d = %f, ", k, var[k]);
  //}
  
  // Store the cholesky decomposition of that hess matrix
  justcholesky(var, chol_at_theta_max, totnran);
  
  //printf("\nnewton_raphson_bi_dev: chol_at_theta_max: ");
  //for(k = 0; k < *totnran*(*totnran + 1)/2; k++){
  //  printf("%d = %f, ", k, chol_at_theta_max[k]);
  //}
  
  
  free(almost_shift);
  free(shift);
  free(right_side);
  free(chol);
  free(var);
  free(prev_max);
  free(theta_new);
  
}
