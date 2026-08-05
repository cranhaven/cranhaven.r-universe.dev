#ifndef METRGIBBS_E0_H
#define METRGIBBS_E0_H

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
);

#endif
