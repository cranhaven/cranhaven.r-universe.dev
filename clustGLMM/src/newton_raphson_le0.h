#ifndef NEWTON_RAPHSON_LE0_H
#define NEWTON_RAPHSON_LE0_H

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
);

#endif
