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

#include "structures.h"
#include "pdfs_derivatives_le0.h"

void newton_raphson_le0(struct str_state* last,    // OUT+IN last known values of generated parameters
                       struct str_param* param,   // IN hyperparameters
                       int* G,                    // IN [1] number of all clusters
                       int* Gplus,                // IN [1] number of non-empty clusters sum(nUg>0)
                       int* n,                   // IN [1] total number of subjects
                       int* nUg,                  // IN [G] number of subjects currently within g-th cluster 
                       // Proposal distribution parameters
                       double* proposal_prec,      // IN+OUT[1] precision of proposal distribution for log(e0)
                       // Tuning parameters
                       struct str_tuning* tuning,
                       int* iter,                   // OUT [1] number of iterations performed before convergence
                       int* converged,              // OUT [1] indicator of convergence
                       double* max_value            // OUT [1] the value of maximized log-likelihood
){
  
  // Auxiliary parameters
  int j;
  int cont;
  double loglik;
  
  double le0 = log(*((*last).e0));
  double grad = 0.0;
  double shift = 0.0;
  double norm_shift = 0.0;
  double le0_new;
  
  logple0(last, param, 
          G, Gplus, n, nUg,
          &le0, max_value);
  
  //printf("\nnewton_raphson_le0: max_value = %f", *max_value);
  //fflush(stdout);
  *converged = 0;
  
  for(j = 0; ((j < *((*tuning).maxiter)) & (!(*converged))); j++){
    //printf("newton_raphson_le0: j = %d \n", j);
    //fflush(stdout);
    // First compute the first and second derivative
    d_d2_logple0(last, param, 
                 G, Gplus, n, nUg,
                 &le0, &grad, proposal_prec);
    // Compute the shift
    shift = grad/(*proposal_prec);
    
    
    le0_new = le0 + shift;
    
    cont = 1;
    while(cont){
      // loglik
      logple0(last, param, 
              G, Gplus, n, nUg,
              &le0_new, &loglik);
      
      // Compare the likelihoods
      if((!isfinite(loglik)) | (loglik < *max_value) ){
        // We have NOT improved the previous solution
        // OR we end up with some Inf nonsense
        // OR the difference in the shift and loglik is already negligible
        
        // Try step-halving
        shift = 0.5*shift;
        le0_new = le0 + shift;
        
        if((isfinite(loglik)) & (abs(shift) + abs(loglik - *max_value) < *((*tuning).tolerance))){
          cont = 0;
        }else{
          cont = 1;
        }
        
      }else{
        // We have improved the previous solution 
        le0 = le0_new;
        *max_value = loglik;
        cont = 0;
        
      }
      
    }
    
    // Did we converge?
    norm_shift = abs(shift);
    *converged = ( norm_shift + abs(loglik - *max_value) < *((*tuning).tolerance));
    //printf("\nnewton_raphson_e0: normshift = %f", norm_shift);
    //fflush(stdout);
    
  }
  
  // Cycle has ended by either convergence at j-th step or at maxiter;
  *iter = j;
  // Update proposal_prec to be at the last value of le0
  d_d2_logple0(last, param, 
               G, Gplus, n, nUg,
               &le0, &grad, proposal_prec);
  
}
