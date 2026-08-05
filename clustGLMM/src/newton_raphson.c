/*
 * Newton-Raphson method for maximization of function dependant on vector paramter theta
 * using first and second order derivatives
 */
#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"
#include "matrmult.h"
#include "cholesky.h"

void newton_raphson(struct str_state* last,   // IN last known values of generated parameters
    double* hyperparameter,   // IN hyperparameters
    double* Y,                // IN [*N]
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
    int* pred_skip,         // IN [1] Which part of predictor is to be skipped
    int* N,                 // IN [1] number of observations
    int* n,                 // IN [1] number of subjects
    int* g,                 // IN [1] *G will be passed for FIXED, g for group-specific data
    int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
    int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
    int* is_cat,            // IN [1] Is it categorical variable?
    int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
    //        FALSE = all levels of cat var have one set of bi - compares last category with others
    int* nfix,              // IN [1] number of FIXED  regressors or each response
    int* cumnfix,           // IN [1] where to start in FormulaF
    int* totnfix,           // IN [1] total dimension of fixed parameter (useless)
    int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
    int* n_i,               // IN [n] number of observations dedicated to j-th subject
    int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
    int* nUg,               // IN [G] number of subjects in classes 1, ..., G
    int* listUi,            // IN [n*G] row by row indices of subjects in group g
    double* theta_0,        // IN [dims[.]] starting value for the algorithm
    double* theta_max,      // OUT [dims[.]] theta maximizing the function
    double* chol_at_theta_max, // OUT [dims[.] * (dims[.]+1) / 2]
    // Functions used
    void (*fun)(struct str_state*, double*, double*, double*, int*, int*, 
          double*, double*, double*, double*, double*, int*,
          int*, int*, int*, int*, int*, int*, 
          int*, int*, int*, int*, int*, int*, int*, int*,
          double*, double*),
    void (*d_d2_fun)(struct str_state*, double*, double*, double*, int*, int*, 
          double*, double*, double*, double*, double*, int*,
          int*, int*, int*, int*, int*, int*, 
          int*, int*, int*, int*, int*, int*, int*, int*,
          double*, double*, double*),
    // Tuning parameters
    struct str_tuning* tuning,
    int* iter,                   // OUT [1] number of iterations performed before convergence
    int* converged,              // OUT [1] indicator of convergence
    double* max_value            // OUT [1] the value of maximized log-likelihood
){
  // Auxiliary parameters
  int j, k;
  int dim_theta;
  int nrep;
  int retchol;
  int cont_half;
  
  if(*is_cat == 2){
    dim_theta = *totnfix; // we are working with random effects, actually totnran
  }else{
    if(*is_cat == 1){
      dim_theta = *nfix * *Kcat; // beta_cat_fix or beta_cat
    }else{
      dim_theta = *nfix; // some other beta parameter or a_ord
    }
  }
  
  //printf("\nnewton_raphson: nfix = %d, ncat = %d, dim_theta = %d", *nfix, *ncat, dim_theta);
  
  double* almost_shift;
  almost_shift = (double*)malloc(dim_theta * sizeof(double));
  //double almost_shift[dim_theta];
  double* shift;
  shift = (double*)malloc(dim_theta * sizeof(double));
  //double shift[dim_theta];
  double* grad;
  grad = (double*)malloc(dim_theta * sizeof(double));
  //double grad[dim_theta];
  double* chol;
  chol = (double*)malloc((dim_theta) * ((dim_theta) + 1)/2 * sizeof(double));
  //double chol[(dim_theta) * ((dim_theta) + 1)/2];
  double norm_shift = 0.0;
  double* hess;
  hess = (double*)malloc((dim_theta) * ((dim_theta) + 1)/2 * sizeof(double));
  //double hess[(dim_theta) * ((dim_theta) + 1)/2];
  double* prev_max;
  prev_max = (double*)malloc(dim_theta * sizeof(double));
  double* theta_new;
  theta_new = (double*)malloc(dim_theta * sizeof(double));
  //double prev_max[dim_theta];
  double loglik_max, loglik, loglik_new, u;
  
  for(k = 0; k < dim_theta; k++){
    prev_max[k] = theta_max[k] = theta_0[k];
  }
  
  (*fun)(last, hyperparameter, Y, X, spec, dims, 
   predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
   N, n, g, Kord, Kcat, kspec_bi_cat, 
   nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
   theta_max, &loglik);
  

  //printf("\nnewton_raphson: g = %d, loglik = %f, theta:  ", *g, loglik);
  //for(k=0; k < dim_theta; k++){
  //  printf("%d = %f, ", k, theta_0[k]);
  //}
  //printf("\n");
  //fflush(stdout);
  
  *max_value = loglik_max = loglik;
  
  *converged = 0;
  nrep = 0;
  
  for(j = 0; ((j < *((*tuning).maxiter)) & (!(*converged))); j++){
    // Rprintf("\nnewton_raphson: j = %d", j);
    // First compute the gradient and negative hessian matrix
    (*d_d2_fun)(last, hyperparameter, Y, X, spec, dims, 
     predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
     N, n, g, Kord, Kcat, kspec_bi_cat, 
     nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
     theta_max, grad, hess);
    
    // Rprintf("\nnewton_raphson: grad: ");
    // for(k = 0; k < dim_theta; k++){
    //   Rprintf("%d = %f, ", k, grad[k]);
    // }
    //fflush(stdout);
    
    // Rprintf("\nnewton_raphson: hess: ");
    // for(k = 0; k < dim_theta*(dim_theta + 1)/2; k++){
    //   Rprintf("%d = %f, ", k, hess[k]);
    // }
    //fflush(stdout);
    
    // Solve for shift using Cholesky decomposition
    retchol = cholesky_solve(hess, &dim_theta, grad, chol, almost_shift);
    if(retchol == 0){
      // Rprintf("\nnewton raphson: hess not positive definite!!! j=%d, g=%d, dim_theta=%d, nrep=%d",
      //        j, *g, dim_theta, nrep);
      //fflush(stdout);
      //failed = 1;
      nrep++;
      // it came to a nonsense --> start all again from different value
      j = 0; // start over
      // Rprintf("\nnewton_raphson: prev theta_max for g = %d: ", *g);
      // for(k=0; k < dim_theta; k++){
      //   Rprintf("%d = %f, ", k, theta_max[k]);
      // }
      //fflush(stdout);
      u = runif(0.0, 1.0)*runif(0.0, 1.0);
      for(k = 0; k < dim_theta; k++){
        theta_max[k] = u * prev_max[k];
        //theta_max[k] = rnorm((0.0 + nrep*prev_max[k])/(1+nrep),0.5+1/nrep);
        // seems that lesser variance and closer to previous maximal solution is better
      }
      // Rprintf("\nnewton_raphson: new theta_max for g = %d: ", *g);
      // for(k=0; k < dim_theta; k++){
      //   Rprintf("%d = %f, ", k, theta_max[k]);
      // }
      //fflush(stdout);
      
      (*fun)(last, hyperparameter, Y, X, spec, dims, 
       predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
       N, n, g, Kord, Kcat, kspec_bi_cat, 
       nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
       theta_max, max_value);
      
      if(*max_value > loglik_max){
        for(k = 0; k < dim_theta; k++){
          prev_max[k] = theta_max[k];
        }
        loglik_max = *max_value;
      }
      
      if(nrep == *((*tuning).maxnrep)){
        // 10 seem enough, but rather put there 100
        break;
      }
    }else{

      // Rprintf("\nnewton_raphson: chol: ");
      // for(k = 0; k < dim_theta*(dim_theta + 1)/2; k++){
      //   Rprintf("%d = %f, ", k, chol[k]);
      // }
      //fflush(stdout);
      
      backsolve2(chol, almost_shift, &dim_theta, shift);
      // Rprintf("\nnewton_raphson: After solve.\n");
      
      // New theta - shift added to previous max
      for(k = 0; k < dim_theta; k++){
        theta_new[k] = theta_max[k] + shift[k];
      }
      
      // Halving cycle
      cont_half = 1;
      while(cont_half){
        // Compute the norm of shift
        norm_shift = 0.0;
        for(k = 0; k < dim_theta; k++){
          norm_shift += shift[k] * shift[k];
        }
        norm_shift = sqrt(norm_shift);
        //Rprintf("\nnormshift = %f, ", norm_shift);
        
        // Evaluate new loglik value
        (*fun)(last, hyperparameter, Y, X, spec, dims, 
         predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
         N, n, g, Kord, Kcat, kspec_bi_cat, 
         nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
         theta_new, &loglik_new);
        // Compare the likelihoods
        if((!isfinite(loglik_new)) | (loglik_new < loglik_max) ){
          // We have NOT improved the previous solution
          // OR we end up with some Inf nonsense
          // OR the difference in the shift and loglik is already negligible
          
          // Try step-halving
          for(k = 0; k < dim_theta; k++){
            shift[k] = 0.5*shift[k];
            theta_new[k] = theta_max[k] + shift[k];
          }
          
          // if((isfinite(loglik_new)) & (norm_shift + abs(loglik_new - loglik_max) < *((*tuning).tolerance))){
          if(isfinite(loglik_new)){
            if(norm_shift < *((*tuning).tolerance)){
              cont_half = 0;
            }else{
              // Rprintf("continue in halving!1");
              cont_half = 1;
            }
          }else{
            if(norm_shift < *((*tuning).tolerance)){
              cont_half = 0;
            }else{
              // Rprintf("continue in halving!2");
              cont_half = 1;
            }
          }
          
        }else{
          // We have improved the previous solution 
          for(k = 0; k < dim_theta; k++){
            theta_max[k] = theta_new[k];
          }
          loglik = loglik_new;
          cont_half = 0;
          
        }
        
      }
      
      // Did we converge?
      *converged = (norm_shift + abs(loglik - loglik_max) < *((*tuning).tolerance));
      
      if(loglik > loglik_max){
        for(k = 0; k < dim_theta; k++){
          prev_max[k] = theta_max[k];
        }
        loglik_max = loglik;
      }
      
      *max_value = loglik;
      
      if(!isfinite(*max_value)){
        //failed = 1;
        nrep++;
        // it came to a nonsense --> start all again from different value
        // Rprintf("\nnewton_raphson: loglik for [g=%d] is infinite start over!", *g);
        //fflush(stdout);
        j = 0; // start over
        u = runif(0.0, 1.0)*runif(0.0, 1.0); // closer to zero
        for(k = 0; k < dim_theta; k++){
          // theta_max[k] = rnorm((0.0 + nrep*prev_max[k])/(1+nrep),0.5+1/nrep);
          // seems that lesser variance and closer to previous maximal solution is better
          theta_max[k] = u*prev_max[k];
        }
        // Rprintf("\nnewton_raphson: new theta_max for g = %d: ", *g);
        // for(k=0; k < dim_theta; k++){
        //   Rprintf("%d = %f, ", k, theta_max[k]);
        // }
        //fflush(stdout);
        
        (*fun)(last, hyperparameter, Y, X, spec, dims, 
         predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
         N, n, g, Kord, Kcat, kspec_bi_cat, 
         nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
         theta_max, max_value);
        
        if(*max_value > loglik_max){
          for(k = 0; k < dim_theta; k++){
            prev_max[k] = theta_max[k];
          }
          loglik_max = *max_value;
        }
        
        if(nrep == *((*tuning).maxnrep)){
          // 10 seem enough, but rather put there 100
          break;
        }
      }else{
  
        // Rprintf("\nnewton_raphson: Iteration %d for g = %d, theta: ", j, *g);
        // for(k=0; k < dim_theta; k++){
        //   Rprintf("%d = %f, ", k, theta_max[k]);
        // }
        //fflush(stdout);
        // Rprintf("\nnewton_raphson: normshift = %f ", norm_shift);
        //fflush(stdout);
        
      }
    }
  }
  
  //printf("\nnewton_raphson: Iteration %d for g = %d, theta: ", j, *g);
  //for(k=0; k < dim_theta; k++){
  //  printf("%d = %f, ", k, theta_max[k]);
  //}
  //printf("\nnewton_raphson: normshift = %f ", norm_shift);
  //fflush(stdout);
  
  if(nrep < *((*tuning).maxnrep)){
    // Cycle has ended by either convergence at j-th step or at maxiter;
    *iter = j;
    *converged = 1;
  }else{
    // do nothing --> chol_at_theta_max will be used from the previous step
    // however, it may not be initialized yet
    *iter = *((*tuning).maxiter);
    *converged = 0;
    // evaluate using the so far best solution prev_max
    for(k = 0; k < dim_theta; k++){
      theta_max[k] = prev_max[k];
    }
    *max_value = loglik_max;
    // or use point 0
    //for(k = 0; k < dim_theta; k++){
    //  theta_max[k] = 0.0;
    //}
    //(*fun)(last, hyperparameter, Y, X, spec, dims, 
    // predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
    // N, n, g, Kord, Kcat, kspec_bi_cat, 
    // nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
    // theta_max, max_value);
  }
  // Update the hess matrix to be at theta_max
  (*d_d2_fun)(last, hyperparameter, Y, X, spec, dims, 
   predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
   N, n, g, Kord, Kcat, kspec_bi_cat, 
   nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, 
   theta_max, grad, hess);
  
  //printf("newton_raphson: grad: ");
  //for(k = 0; k < dim_theta; k++){
  //  printf("%d = %f, ", k, grad[k]);
  //}
  //printf("\nnewton_raphson: hess: ");
  //for(k = 0; k < dim_theta*(dim_theta + 1)/2; k++){
  //  printf("%d = %f, ", k, hess[k]);
  //}
  
  // Store the cholesky decomposition of that hess matrix
  justcholesky(hess, chol_at_theta_max, &dim_theta);
  
  //printf("\nnewton_raphson: chol_at_theta_max: ");
  //for(k = 0; k < dim_theta*(dim_theta + 1)/2; k++){
  //  printf("%d = %f, ", k, chol_at_theta_max[k]);
  //}
  
  
  free(almost_shift);
  free(shift);
  free(grad);
  free(chol);
  free(hess);
  free(prev_max);
  free(theta_new);
  
}
