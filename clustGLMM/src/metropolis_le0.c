/*
 * Metropolis step for arbitrary parameter
 */

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"
#include "pdfs_derivatives_le0.h"

void metropolis_le0(struct str_state* last,    // OUT+IN last known values of generated parameters
                    struct str_param* param,   // IN hyperparameters
                    int* G,                    // IN [1] number of all clusters
                    int* Gplus,                // IN [1] number of non-empty clusters sum(nUg>0)
                    int* n,                    // IN [1] total number of subjects
                    int* nUg,                  // IN [G] number of subjects currently within g-th cluster
                    // Proposal distribution parameters
                    double* proposal_prec,      // IN+OUT[1] precision of proposal distribution for log(e0)
                    // Tuning parameters
                    struct str_tuning* tuning
){
  // Auxiliary parameters
  int j;
  double le;
  double loglik_new; 
  double loglik;
  double proposal_sd;
  double proposal;
  double log_accept_prob;
  double accept_prob;
  double u;
  int accept;
  
  le = log(*((*last).e0));
  // proposal var is actually inverse variance
  proposal_sd = sqrt(1/(*proposal_prec)) * *((*tuning).const_proposal_e0);
  
  logple0(last, param, 
         G, Gplus, n, nUg,
         &le, &loglik);
  
  for(j = 0; j < *((*tuning).times_proposal); j++){
    //// Sample proposal as current value + N(0, var * const_propsal^2)
    proposal = le + rnorm(0.0, 1.0) * proposal_sd;
    //printf("\nmetropolis_e0: e = %f, le = %f, proposal_prec=%f, sd=%f, proposal = %f, new_e0 = %f", 
    //       *((*last).e0), le, *proposal_prec, proposal_sd, proposal, exp(proposal));
    //fflush(stdout);
    // Log-likelihood at newly proposed value for e0
    logple0(last, param, 
            G, Gplus, n, nUg,
            &proposal, &loglik_new);
    
    // Compute acceptance probability for theta_new
    log_accept_prob = loglik_new - loglik;
    
    // Do we accept proposal?
    accept = 0;
    if(log_accept_prob > 0){
      // definitely accept this proposal
      //accept_prob = 1.0;
      accept = 1;
      //printf("\naccept = %d", accept);
      //fflush(stdout);
    }else{
      accept_prob = exp(log_accept_prob);
      // accept_prob lies within [0, 1] --> sample u from Unif[0,1]
      u = runif(0.0, 1.0);
      if(u < accept_prob){
        accept = 1;
      }// otherwise accept remains to be 0
      //printf("\naccept_prob = %f, u = %f, accept = %d", accept_prob, u, accept);
      //fflush(stdout);
    }
    
    // If to be accepted switch le to proposal, last.e0 to exp of proposal and loglik
    if(accept){
      le = proposal;
      *((*last).e0) = exp(proposal);
      loglik = loglik_new;
    }
    
    // ... and continue with another proposals
  }
}
