/*
 * Generating beta bins from the conditioned distribution 
 */


#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"

#include "newton_raphson_le0.h"
#include "metropolis_le0.h"
#include "pdfs_derivatives_le0.h"

void metrgibbs_e0(struct str_state* last,    // OUT+IN last known values of generated parameters
                  struct str_param* param,   // IN hyperparameters
                  int* G,                    // IN [1] number of all clusters
                  int* Gplus,                // IN [1] number of non-empty clusters sum(nUg>0)
                  int* n,                   // IN [1] total number of subjects
                  int* nUg,                  // IN [G] number of subjects currently within g-th cluster 
                  int* m,                    // IN [1] number of iteration
                  // Proposal distribution parameters
                  double* proposal_prec,      // IN+OUT[1] precision of proposal distribution for log(e0)
                  // Tuning parameters
                  struct str_tuning* tuning
){
  int iter;
  int converged;
  double loglik;
  
  
  if((*m % *((*tuning).freq_proposal_update)) == 0){
    //printf("\nTrigerring newton_raphson_bi for b[%d]: \n", i);
    newton_raphson_le0(last, param, 
                      G, Gplus, n, nUg,
                      // IN+OUT parameters
                      proposal_prec,
                      // Tuning parameters
                      tuning, &iter, &converged, &loglik);
  }
  
  
  // Now perform Metropolis step
  metropolis_le0(last, param, 
                G, Gplus, n, nUg,
                // IN+OUT parameters
                proposal_prec,
                // Tuning parameters
                tuning);

  
}
